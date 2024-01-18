-- |
-- Module      : ZuulWeeder.Monitoring
-- Description : Service monitoring middleware
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- The monitoring middleware to export prometheus metrics.
module ZuulWeeder.Monitoring (mkMonitoring) where

import Data.ByteString qualified as BS
import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket qualified
import Network.Wai qualified as Wai
import Prometheus qualified
import Prometheus.Metric.GHC qualified
import Web.HttpApiData (toHeader)
import ZuulWeeder.Prelude

-- | Create the monitoring middleware.
mkMonitoring :: Logger -> IO Wai.Middleware
mkMonitoring logger = do
  Prometheus.unregisterAll
  void $ Prometheus.register Prometheus.Metric.GHC.ghcMetrics
  counter <- Prometheus.register $ Prometheus.counter (Prometheus.Info "http_request" "")
  error_counter <- Prometheus.register $ Prometheus.counter (Prometheus.Info "http_request_error" "")
  pure $ monitoring logger (counter, error_counter)

monitoring :: Logger -> (Prometheus.Counter, Prometheus.Counter) -> Wai.Middleware
monitoring logger (counter, error_counter) baseApp req resp = case Wai.rawPathInfo req of
  "/health" -> resp $ Wai.responseLBS HTTP.ok200 [] mempty
  "/metrics" -> resp . Wai.responseLBS HTTP.ok200 [] =<< Prometheus.exportMetricsAsText
  p | "/dists/" `BS.isPrefixOf` p -> baseApp req resp
  p -> do
    measure <- intervalMilliSec
    baseApp req $ \r -> do
      result <- resp r
      elapsed <- measure
      let statusCode = HTTP.statusCode $ Wai.responseStatus r
          htmx = any (\h -> fst h == "HX-Request") $ Wai.requestHeaders req
          client = remoteHash (Wai.remoteHost req) + hash (Wai.requestHeaderUserAgent req)
          msg =
            p
              <> (" code=" <> toHeader statusCode)
              <> (" ms=" <> toHeader elapsed)
              <> (" htmx=" <> toHeader htmx)
              <> (" client=" <> toHeader client)
      if statusCode >= 200 && statusCode < 300
        then Prometheus.incCounter counter
        else Prometheus.incCounter error_counter
      info logger msg
      pure result
  where
    remoteHash :: Network.Socket.SockAddr -> Int
    remoteHash = \case
      Network.Socket.SockAddrInet _ h -> hash h
      Network.Socket.SockAddrInet6 _ h _ _ -> hash h
      Network.Socket.SockAddrUnix _ -> 0
