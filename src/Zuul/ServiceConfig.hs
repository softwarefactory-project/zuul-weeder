-- |
-- Module      : Zuul.Serviceconfig
-- Description : Helper for zuul.conf
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- The Zuul Service configuration (zuul.conf)
module Zuul.ServiceConfig
  ( ServiceConfig (..),
    readServiceConfig,
  )
where

import Data.HashMap.Strict qualified as HM
import Data.Ini qualified
import Data.Map qualified as Map
import Data.Text qualified as Text
import Network.URI (parseURI, uriAuthority, uriRegName)
import Zuul.Config
  ( ConnectionName (ConnectionName),
    ConnectionUrl (..),
    ProviderName (ProviderName),
  )
import Zuul.ZooKeeper (ZKConnection (..))
import ZuulWeeder.Prelude

type ConfigSection = (Text, [(Text, Text)])

-- | The zuul.conf content
data ServiceConfig = ServiceConfig
  { -- | The list of connections.
    connections :: Map ConnectionName ProviderName,
    -- | The list of connection urls, to build config loc url in the UI.
    urlBuilders :: Map ProviderName ConnectionUrl,
    -- | The dump script parameter: hosts, key, cert, ca
    zookeeper :: ZKConnection
  }

-- | Read the zuul.conf file
readServiceConfig ::
  -- | An action to produce the zuul.conf content
  IO Text ->
  -- | The decoded config
  ExceptT Text IO ServiceConfig
readServiceConfig getContent = do
  content <- lift getContent
  case Data.Ini.parseIni content of
    Right (Data.Ini.Ini sections _) -> except $ parseConfig sections
    Left _ -> throwError "Unable to read Zuul config file"

parseConfig :: HM.HashMap Text [(Text, Text)] -> Either Text ServiceConfig
parseConfig sections = do
  zookeeper <- do
    zkSection <- getZkSection
    let getZK k = lookup k zkSection `orDie` ("No " <> k <> " in zookeeper section")
    ZKConnection <$> traverse getZK ["hosts", "tls_key", "tls_cert", "tls_ca"]
  connections <- Map.fromList . catMaybes <$> traverse getConn (HM.toList connSections)
  let urlBuilders = Map.fromList (mapMaybe getGitwebBuilder (HM.toList connSections))
  pure $ ServiceConfig {connections, urlBuilders, zookeeper}
  where
    getZkSection = HM.lookup "zookeeper" sections `orDie` "No zookeeper section"
    connSections = HM.filterWithKey (\k _ -> Text.isPrefixOf "connection " k) sections

    getGitwebBuilder :: ConfigSection -> Maybe (ProviderName, ConnectionUrl)
    getGitwebBuilder (_, section) =
      let sectionHM = HM.fromList section
       in case HM.lookup "driver" sectionHM of
            Just driver | driver `elem` ["gerrit", "gitlab", "pagure"] -> do
              let baseUrl = getBaseUrl sectionHM
              case getCanonicalName sectionHM of
                Right canonicalName ->
                  Just (ProviderName canonicalName, getUrl driver baseUrl)
                _ -> Nothing
            Just "github" -> do
              let baseUrl = "https://" <> fromMaybe "github.com" (HM.lookup "server" sectionHM)
              case getCanonicalName sectionHM of
                Right canonicalName ->
                  Just (ProviderName canonicalName, GithubUrl baseUrl)
                _ -> Nothing
            Just "git" -> do
              case getGitProviderInfo sectionHM of
                Left x -> error $ "Can't get git url: " <> Text.unpack x
                Right (pn, baseUrl) -> Just (ProviderName pn, GitUrl baseUrl)
            _ -> Nothing
      where
        getUrl driver url = case driver of
          "gerrit" -> GerritUrl url
          "gitlab" -> GitlabUrl url
          "pagure" -> PagureUrl url
          _ -> error $ "Unknown driver type: " <> from driver
        getBaseUrl :: HM.HashMap Text Text -> Text
        getBaseUrl sectionHM = case HM.lookup "baseurl" sectionHM of
          Just url -> url
          Nothing -> case getServer sectionHM of
            Right server -> "https://" <> server
            Left _ -> error $ "Unable to find 'server' attribute in: " <> show sectionHM

    getConn :: ConfigSection -> Either Text (Maybe (ConnectionName, ProviderName))
    getConn (sectionName, section) =
      let sectionHM = HM.fromList section
       in case HM.lookup "driver" sectionHM of
            Just driver | driver `elem` ["gerrit", "github", "gitlab", "pagure"] -> do
              server <- getCanonicalName sectionHM
              pure $ Just (getSectionName sectionName, ProviderName server)
            Just "git" -> do
              (pn, _) <- getGitProviderInfo sectionHM
              pure $ Just (getSectionName sectionName, ProviderName pn)
            _ -> pure Nothing

    -- return ('host','http://host:42/') from "baseurl=http://host:42/"
    getGitProviderInfo section = do
      baseUrl <- HM.lookup "baseurl" section `orDie` "No baseurl"
      uri <- parseURI (from baseUrl) `orDie` ("Unable to parse URI: " <> baseUrl)
      host <- uriAuthority uri `orDie` "invalid url"
      pure (from $ uriRegName host, baseUrl)

    dropSectionPrefix = Text.drop 11
    getSectionName sn = ConnectionName $ dropSectionPrefix sn
    getCanonicalName hm = case HM.lookup "canonical_hostname" hm of
      Just hostname -> pure hostname
      Nothing -> getServer hm
    getServer hm = case HM.lookup "server" hm of
      Just server -> pure server
      Nothing -> Left $ "Unable to find mandatory 'server' key in: " <> Text.pack (show hm)
