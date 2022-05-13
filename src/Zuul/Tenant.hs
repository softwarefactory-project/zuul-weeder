module Zuul.Tenant where

import Control.Lens
import qualified Data.Aeson
import qualified Data.HashMap.Strict as HM (lookup, toList)
import qualified Data.Map
import Data.Maybe (isJust)
import qualified Data.Set
import qualified Data.Text
import qualified Data.Vector as V
import Zuul.Config (ConfigConnections, ConnectionCName (ConnectionCName))
import Zuul.ConfigLoader
  ( CanonicalProjectName (CanonicalProjectName),
    ConnectionName (ConnectionName),
    ProjectName (ProjectName),
    ProviderName (ProviderName),
    TenantName (TenantName),
    decodeAsList,
    getObjValue,
    unwrapObject,
  )
import Zuul.ZKDump (ZKSystemConfig (..))

data ZuulConfigType
  = PipelineT
  | JobT
  | SemaphoreT
  | ProjectT
  | ProjectTemplateT
  | NodesetT
  | SecretT
  deriving (Show, Eq, Ord, Enum, Bounded)

allItems :: Data.Set.Set ZuulConfigType
allItems = Data.Set.fromList [minBound .. maxBound]

toItemType :: Data.Text.Text -> ZuulConfigType
toItemType name = case name of
  "pipeline" -> PipelineT
  "job" -> JobT
  "semaphore" -> SemaphoreT
  "project" -> ProjectT
  "project-template" -> ProjectTemplateT
  "nodeset" -> NodesetT
  "secret" -> SecretT
  _type -> error $ "Unexpected configuration item type: " <> Data.Text.unpack _type

data ProjectNameWithOptions = ProjectNameWithOptions
  { projectName :: ProjectName,
    includedConfigElements :: Data.Set.Set ZuulConfigType
  }
  deriving (Show, Eq, Ord)

type TenantProjects = [(CanonicalProjectName, [ZuulConfigType])]

data TenantConnectionConfig = TenantConnectionConfig
  { configProjects :: [ProjectNameWithOptions],
    untrustedProjects :: [ProjectNameWithOptions]
  }
  deriving (Show, Eq, Ord)

data TenantConfig = TenantConfig
  { defaultParent :: Data.Text.Text,
    connections :: Data.Map.Map ConnectionName TenantConnectionConfig
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
      let tenantObject = unwrapObject tenant
          source = HM.toList $ unwrapObject $ getObjValue "source" tenantObject
          defaultParent = case getObjValue "default-parent" tenantObject of
            Data.Aeson.String txt -> txt
            _ -> "base"
       in insertTenantConnections defaultParent (TenantConfig defaultParent Data.Map.empty) source

    insertTenantConnections :: Data.Text.Text -> TenantConfig -> [(Data.Text.Text, Data.Aeson.Value)] -> TenantConfig
    insertTenantConnections defaultParent tc assocs = case assocs of
      [] -> tc
      (cName, cData) : xs ->
        let new = over tenantConfigL (Data.Map.insert (ConnectionName cName) (decodeConnection cData)) tc
         in insertTenantConnections defaultParent new xs

    decodeConnection :: Data.Aeson.Value -> TenantConnectionConfig
    decodeConnection cnx =
      let configProjects = getProjects "config-projects"
          untrustedProjects = getProjects "untrusted-projects"
       in TenantConnectionConfig {..}
      where
        getProjects :: Data.Text.Text -> [ProjectNameWithOptions]
        getProjects ptype = case HM.lookup ptype $ unwrapObject cnx of
          Just (Data.Aeson.String name) -> [ProjectNameWithOptions (ProjectName name) mempty]
          Just (Data.Aeson.Array vec) -> getProject <$> concatMap (HM.toList . unwrapObject) (V.toList vec)
          _ -> []
        -- TODO: support https://zuul-ci.org/docs/zuul/latest/tenants.html#attr-tenant.untrusted-projects.%3Cproject-group%3E
        getProject :: (Data.Text.Text, Data.Aeson.Value) -> ProjectNameWithOptions
        getProject (name, options') =
          let options = unwrapObject options'
              included = Data.Set.fromList $ toItemType <$> decodeAsList "include" id options
              excluded = Data.Set.fromList $ toItemType <$> decodeAsList "exclude" id options
              includedElements
                | isJust $ HM.lookup "include" options = included
                | isJust $ HM.lookup "exclude" options = Data.Set.difference allItems excluded
                | otherwise = allItems
           in ProjectNameWithOptions (ProjectName name) includedElements

getTenantProjects :: ConfigConnections -> TenantsConfig -> TenantName -> Maybe TenantProjects
getTenantProjects connections tenantsConfig tenantName =
  let tenantConfig = Data.Map.lookup tenantName $ view tenantsConfigL tenantsConfig
      tenantLayout = Data.Map.toList <$> (view tenantConfigL <$> tenantConfig)
   in concatMap extractProject <$> tenantLayout
  where
    extractProject :: (ConnectionName, TenantConnectionConfig) -> TenantProjects
    extractProject (connectionName, TenantConnectionConfig {..}) =
      let projects = configProjects <> untrustedProjects
          providerName = case Data.Map.lookup connectionName connections of
            Just (ConnectionCName pn) -> ProviderName pn
            Nothing -> error "Unable to find project connection's provider name"
       in ( \ProjectNameWithOptions {..} ->
              ( CanonicalProjectName (providerName, projectName),
                Data.Set.toList includedConfigElements
              )
          )
            <$> projects