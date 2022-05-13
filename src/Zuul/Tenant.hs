{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}
module Zuul.Tenant where

import Control.Lens
import qualified Data.Aeson
import qualified Data.HashMap.Strict as HM (lookup, toList)
import qualified Data.List
import qualified Data.Map
import Data.Maybe (isJust)
import qualified Data.Set
import qualified Data.Text
import qualified Data.Vector as V
import Zuul.Config (ConfigConnections, ConnectionCName (ConnectionCName), ConnectionsConfig)
import Zuul.ConfigLoader
  ( CanonicalProjectName (CanonicalProjectName),
    ConfigLoc (clPath, clProject),
    ConfigPath (getConfigPath),
    ConnectionName (ConnectionName),
    JobName (JobName),
    ProjectName (ProjectName),
    ProviderName (ProviderName),
    TenantName (TenantName),
    ZuulConfigType (..),
    decodeAsList,
    getObjValue,
    unwrapObject,
  )
import Zuul.ZKDump (ZKSystemConfig (..))

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
    includedConfigElements :: Data.Set.Set ZuulConfigType,
    configPaths :: [FilePath]
  }
  deriving (Show, Eq, Ord)

type TenantProjects = [(CanonicalProjectName, [ZuulConfigType])]

data TenantConnectionConfig = TenantConnectionConfig
  { configProjects :: [ProjectNameWithOptions],
    untrustedProjects :: [ProjectNameWithOptions]
  }
  deriving (Show, Eq, Ord)

data TenantConfig = TenantConfig
  { defaultParent :: JobName,
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

decodeTenantsConfig :: ZKSystemConfig -> Maybe TenantsConfig
decodeTenantsConfig (ZKSystemConfig value) = case value of
  Data.Aeson.Object hm ->
    let abide = unwrapObject $ getObjValue "unparsed_abide" hm
        tenants = HM.toList $ unwrapObject $ getObjValue "tenants" abide
     in Just $ insertTenants (TenantsConfig Data.Map.empty) tenants
  _ -> Nothing
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
          defaultParent = JobName $ case HM.lookup "default-parent" tenantObject of
            Just (Data.Aeson.String txt) -> txt
            _ -> "base"
       in insertTenantConnections defaultParent (TenantConfig defaultParent Data.Map.empty) source

    insertTenantConnections :: JobName -> TenantConfig -> [(Data.Text.Text, Data.Aeson.Value)] -> TenantConfig
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
        defaultPaths = [".zuul.yaml", "zuul.yaml", ".zuul.d/", "zuul.d/"]
        getProjects :: Data.Text.Text -> [ProjectNameWithOptions]
        getProjects ptype = case HM.lookup ptype $ unwrapObject cnx of
          Just (Data.Aeson.String name) -> [ProjectNameWithOptions (ProjectName name) mempty defaultPaths]
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
              extraConfigPaths = [] -- TODO: decode attribute
           in ProjectNameWithOptions (ProjectName name) includedElements (extraConfigPaths <> defaultPaths)

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

tenantResolver :: TenantsConfig -> ConnectionsConfig -> ConfigLoc -> ZuulConfigType -> [TenantName]
tenantResolver tenants connections configLoc zct = matches
  where
    matches = map fst $ filter (containsProject . snd) $ Data.Map.toList $ view tenantsConfigL tenants
    containsProject :: TenantConfig -> Bool
    containsProject tc = any containsProject' $ Data.Map.toList $ view tenantConfigL tc
    containsProject' :: (ConnectionName, TenantConnectionConfig) -> Bool
    containsProject' (cn, TenantConnectionConfig {..}) = any matchProject $ configProjects <> untrustedProjects
      where
        matchProject :: ProjectNameWithOptions -> Bool
        matchProject ProjectNameWithOptions {..} =
          let providerName = case Data.Map.lookup cn connections of
                Just (ConnectionCName pn) -> ProviderName pn
                Nothing -> error "Unable to find project connection's provider name"
           in and
                [ CanonicalProjectName (providerName, projectName) == clProject configLoc,
                  zct `Data.Set.member` includedConfigElements,
                  any matchPath configPaths
                ]
        matchPath :: FilePath -> Bool
        matchPath fp = fp `Data.List.isPrefixOf` getConfigPath (clPath configLoc)

getTenantDefaultBaseJob :: TenantsConfig -> TenantName -> Maybe JobName
getTenantDefaultBaseJob tenantsConfig tenantName =
  let tenantConfig = Data.Map.lookup tenantName $ view tenantsConfigL tenantsConfig
   in defaultParent <$> tenantConfig
