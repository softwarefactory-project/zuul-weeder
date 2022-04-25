module Zuul.Tenant where

import Control.Lens
import qualified Data.Aeson
import qualified Data.HashMap.Strict as HM (keys, lookup, toList)
import qualified Data.Map
import qualified Data.Text
import qualified Data.Vector as V
import Zuul.ConfigLoader
  ( ConnectionName (ConnectionName),
    ProjectName (ProjectName),
    TenantName (TenantName),
    getObjValue,
    unwrapObject,
  )
import Zuul.ZKDump (ZKSystemConfig (..))

data TenantConnectionConfig = TenantConnectionConfig
  { configProjects :: [ProjectName],
    untrustedProjects :: [ProjectName]
  }
  deriving (Show, Eq, Ord)

newtype TenantConfig = TenantConfig
  { connections :: Data.Map.Map ConnectionName TenantConnectionConfig
  }
  deriving (Show, Eq, Ord)

tenantConfigL :: Lens' TenantConfig (Data.Map.Map ConnectionName TenantConnectionConfig)
tenantConfigL = lens connections (\c s -> c {connections = s})

newtype TenantsConfig = TenantsConfig
  { tenants :: Data.Map.Map TenantName TenantConfig
  }
  deriving (Show, Eq, Ord)

tenantsConfigL :: Lens' TenantsConfig (Data.Map.Map TenantName TenantConfig)
tenantsConfigL = lens tenants (\c s -> c {tenants = s})

decodeTenantsConfig :: ZKSystemConfig -> TenantsConfig
decodeTenantsConfig (ZKSystemConfig value) = case value of
  Data.Aeson.Object hm ->
    let abide = unwrapObject $ getObjValue "unparsed_abide" hm
        tenants = HM.toList $ unwrapObject $ getObjValue "tenants" abide
     in insertTenants (TenantsConfig Data.Map.empty) tenants
  _ -> error $ "Wrong system config format: " <> show value
  where
    insertTenants :: TenantsConfig -> [(Data.Text.Text, Data.Aeson.Value)] -> TenantsConfig
    insertTenants tc assocs = case assocs of
      [] -> tc
      (tName, tData) : xs ->
        let new = over tenantsConfigL (Data.Map.insert (TenantName tName) (decodeTenant tData)) tc
         in insertTenants new xs

    decodeTenant :: Data.Aeson.Value -> TenantConfig
    decodeTenant tenant =
      let source = HM.toList $ unwrapObject $ getObjValue "source" $ unwrapObject tenant
       in insertTenantConnections (TenantConfig Data.Map.empty) source

    insertTenantConnections :: TenantConfig -> [(Data.Text.Text, Data.Aeson.Value)] -> TenantConfig
    insertTenantConnections tc assocs = case assocs of
      [] -> tc
      (cName, cData) : xs ->
        let new = over tenantConfigL (Data.Map.insert (ConnectionName cName) (decodeConnection cData)) tc
         in insertTenantConnections new xs

    decodeConnection :: Data.Aeson.Value -> TenantConnectionConfig
    decodeConnection cnx =
      let configProjects = getProjects "config-projects"
          untrustedProjects = getProjects "untrusted-projects"
       in TenantConnectionConfig {..}
      where
        getProjects :: Data.Text.Text -> [ProjectName]
        getProjects ptype = case HM.lookup ptype $ unwrapObject cnx of
          Just (Data.Aeson.Array vec) -> ProjectName <$> concatMap (HM.keys . unwrapObject) (V.toList vec)
          _ -> []
