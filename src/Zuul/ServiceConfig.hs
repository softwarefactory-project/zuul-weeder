-- | A zuul.conf loader
module Zuul.ServiceConfig (ServiceConfig (..), ConnectionCName (..), readServiceConfig) where

import Data.HashMap.Strict qualified as HM
import Data.Ini qualified
import Data.Map qualified as Map
import Data.Text qualified as Text
import Zuul.ConfigLoader (ConnectionName (ConnectionName))
import ZuulWeeder.Prelude

newtype ConnectionCName = ConnectionCName Text deriving (Show, Eq, Ord)

type ConfigSection = (Text, [(Text, Text)])

data ServiceConfig = ServiceConfig
  { connections :: Map ConnectionName ConnectionCName,
    -- | The dump script parameter: hosts, key, cert, ca
    zookeeper :: [Text]
  }

readServiceConfig :: FilePathT -> ExceptT Text IO ServiceConfig
readServiceConfig fp = do
  iniE <- lift $ Data.Ini.readIniFile (getPath' fp)
  case iniE of
    Right (Data.Ini.Ini sections _) -> except $ parseConfig sections
    Left _ -> throwError "Unable to read Zuul config file"

parseConfig :: HM.HashMap Text [(Text, Text)] -> Either Text ServiceConfig
parseConfig sections = do
  zookeeper <- do
    zkSection <- getZkSection
    let getZK k = lookup k zkSection `orDie` ("No " <> k <> " in zookeeper section")
    traverse getZK ["hosts", "tls_key", "tls_cert", "tls_ca"]
  connections <- Map.fromList <$> traverse getConn (HM.toList connSections)
  pure $ ServiceConfig {connections, zookeeper}
  where
    getZkSection = HM.lookup "zookeeper" sections `orDie` "No zookeeper section"
    connSections = HM.filterWithKey (\k _ -> Text.isPrefixOf "connection " k) sections

    getConn :: ConfigSection -> Either Text (ConnectionName, ConnectionCName)
    getConn (sectionName, section) =
      let sectionHM = HM.fromList section
       in case HM.lookup "driver" sectionHM of
            Just driver | driver `elem` ["gerrit", "github", "gitlab", "pagure"] -> do
              server <- getCannonicalName sectionHM
              pure (getSectionName sectionName, server)
            _ -> Left ("Connection not supported: " <> Text.pack (show section))
    getSectionName sn = ConnectionName $ Text.drop 11 sn
    getCannonicalName hm =
      ConnectionCName <$> case HM.lookup "canonical_hostname" hm of
        Just hostname -> pure hostname
        Nothing -> getServer hm
    getServer hm = case HM.lookup "server" hm of
      Just server -> pure server
      Nothing -> Left $ "Unable to find mandatory 'server' key in: " <> Text.pack (show hm)
