module Zuul.Config where

import ZuulWeeder.Prelude
import Data.HashMap.Strict qualified as HM
import Data.Ini qualified
import Data.Map qualified as Map
import Data.Text qualified as Text
import Zuul.ConfigLoader (ConnectionName (ConnectionName))

newtype ConnectionCName = ConnectionCName Text deriving (Show, Eq, Ord)

type ConfigSection = (Text, [(Text, Text)])

type ConnectionsConfig = Map ConnectionName ConnectionCName

readConnections :: FilePathT -> IO ConnectionsConfig
readConnections fp = do
  iniE <- Data.Ini.readIniFile (getPath' fp)
  case iniE of
    Right (Data.Ini.Ini sections _) ->
      let filteredHM = HM.filterWithKey (\k _ -> Text.isPrefixOf "connection " k) sections
       in pure $ Map.fromList (mapMaybe getConn (HM.toList filteredHM)) -- catMaybes $ getConn <$> HM.toList filteredHM)
    Left _ -> error "Unable to read Zuul config file"
  where
    getConn :: ConfigSection -> Maybe (ConnectionName, ConnectionCName)
    getConn (sectionName, section) =
      let sectionHM = HM.fromList section
       in case HM.lookup "driver" sectionHM of
            Just driver ->
              if driver
                `elem` [ "gerrit",
                         "github",
                         "gitlab",
                         "pagure"
                       ]
                then Just (getSectionName sectionName, getCannonicalName sectionHM)
                else Nothing
            _ -> error "Not supported"
    getSectionName sn = ConnectionName $ Text.drop 11 sn
    getServer hm = case HM.lookup "server" hm of
      Just server -> server
      Nothing -> error $ "Unable to find mandatory 'server' key in: " <> show hm
    getCannonicalName hm = ConnectionCName $ case HM.lookup "canonical_hostname" hm of
      Just hostname -> hostname
      Nothing -> getServer hm
