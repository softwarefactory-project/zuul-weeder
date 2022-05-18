-- | The Zuul Service configuration (zuul.conf)
module Zuul.ServiceConfig
  ( ServiceConfig (..),
    readServiceConfig,
  )
where

import Data.HashMap.Strict qualified as HM
import Data.Ini qualified
import Data.Map qualified as Map
import Data.Text qualified as Text
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName)
import Zuul.Config
  ( ConnectionName (ConnectionName),
    ConnectionUrl (..),
    ProviderName (ProviderName),
  )
import Zuul.ZooKeeper (ZKConnection (..))
import ZuulWeeder.Prelude

type ConfigSection = (Text, [(Text, Text)])

data ServiceConfig = ServiceConfig
  { connections :: Map ConnectionName ProviderName,
    urlBuilders :: Map ProviderName ConnectionUrl,
    -- | The dump script parameter: hosts, key, cert, ca
    zookeeper :: ZKConnection
  }

readServiceConfig :: IO Text -> ExceptT Text IO ServiceConfig
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
            Just "gerrit" -> do
              let baseUrl = case HM.lookup "baseurl" sectionHM of
                    Just url -> Just url
                    Nothing -> case getServer sectionHM of
                      Right server -> Just $ "https://" <> server
                      Left _ -> Nothing
              case (getCanonicalName sectionHM, baseUrl) of
                (Right canonicalName, Just url) ->
                  Just (ProviderName canonicalName, GerritUrl url)
                _ -> Nothing
            Just "git" -> do
              case getGitProviderName sectionHM of
                Left x -> error $ "Can't get git url: " <> Text.unpack x
                Right pn -> Just (ProviderName pn, GitUrl pn)
            _ -> Nothing

    getConn :: ConfigSection -> Either Text (Maybe (ConnectionName, ProviderName))
    getConn (sectionName, section) =
      let sectionHM = HM.fromList section
       in case HM.lookup "driver" sectionHM of
            Just driver | driver `elem` ["gerrit", "github", "gitlab", "pagure"] -> do
              server <- getCanonicalName sectionHM
              pure $ Just (getSectionName sectionName, ProviderName server)
            Just "git" -> do
              pn <- getGitProviderName sectionHM
              pure $ Just (getSectionName sectionName, ProviderName pn)
            _ -> pure Nothing

    -- return 'host' from "baseurl=http://host:42/"
    getGitProviderName section = do
      uri <- (parseURI . from =<< HM.lookup "baseurl" section) `orDie` "No baseurl"
      host <- uriAuthority uri `orDie` "invalid url"
      pure . from $ uriRegName host <> "/" <> drop 1 (uriPath uri)

    dropSectionPrefix = Text.drop 11
    getSectionName sn = ConnectionName $ dropSectionPrefix sn
    getCanonicalName hm = case HM.lookup "canonical_hostname" hm of
      Just hostname -> pure hostname
      Nothing -> getServer hm
    getServer hm = case HM.lookup "server" hm of
      Just server -> pure server
      Nothing -> Left $ "Unable to find mandatory 'server' key in: " <> Text.pack (show hm)
