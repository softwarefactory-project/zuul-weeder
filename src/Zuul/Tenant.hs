{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}
module Zuul.Tenant where

import Control.Lens
import Data.Aeson qualified
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM (lookup, toList)
import Data.List qualified
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Vector qualified as V
import Zuul.Config (ConnectionCName (ConnectionCName), ConnectionsConfig)
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

allItems :: Set.Set ZuulConfigType
allItems = Set.fromList [minBound .. maxBound]

toItemType :: Text -> ZuulConfigType
toItemType name = case name of
  "pipeline" -> PipelineT
  "job" -> JobT
  "semaphore" -> SemaphoreT
  "project" -> ProjectT
  "project-template" -> ProjectTemplateT
  "nodeset" -> NodesetT
  "secret" -> SecretT
  _type -> error $ "Unexpected configuration item type: " <> Text.unpack _type

data ProjectNameWithOptions = ProjectNameWithOptions
  { projectName :: ProjectName,
    includedConfigElements :: Set.Set ZuulConfigType,
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
    connections :: Map.Map ConnectionName TenantConnectionConfig
  }
  deriving (Show, Eq, Ord)

tenantConfigL :: Lens' TenantConfig (Map.Map ConnectionName TenantConnectionConfig)
tenantConfigL = lens connections (\c s -> c {connections = s})

newtype TenantsConfig = TenantsConfig
  { tenants :: Map.Map TenantName TenantConfig
  }
  deriving (Show, Eq, Ord)

tenantsConfigL :: Lens' TenantsConfig (Map.Map TenantName TenantConfig)
tenantsConfigL = lens tenants (\c s -> c {tenants = s})

decodeTenantsConfig :: ZKSystemConfig -> Maybe TenantsConfig
decodeTenantsConfig (ZKSystemConfig value) = case value of
  Data.Aeson.Object hm ->
    let abide = unwrapObject $ getObjValue "unparsed_abide" hm
        tenants = HM.toList $ unwrapObject $ getObjValue "tenants" abide
     in Just $ insertTenants (TenantsConfig Map.empty) tenants
  _ -> Nothing
  where
    insertTenants :: TenantsConfig -> [(Data.Aeson.Key.Key, Data.Aeson.Value)] -> TenantsConfig
    insertTenants tc assocs = case assocs of
      [] -> tc
      (Data.Aeson.Key.toText -> tName, tData) : xs ->
        let new = over tenantsConfigL (Map.insert (TenantName tName) (decodeTenant tData)) tc
         in insertTenants new xs

    decodeTenant :: Data.Aeson.Value -> TenantConfig
    decodeTenant tenant =
      let tenantObject = unwrapObject tenant
          source = HM.toList $ unwrapObject $ getObjValue "source" tenantObject
          defaultParent = JobName $ case HM.lookup "default-parent" tenantObject of
            Just (Data.Aeson.String txt) -> txt
            _ -> "base"
       in insertTenantConnections defaultParent (TenantConfig defaultParent Map.empty) source

    insertTenantConnections :: JobName -> TenantConfig -> [(Data.Aeson.Key.Key, Data.Aeson.Value)] -> TenantConfig
    insertTenantConnections defaultParent tc assocs = case assocs of
      [] -> tc
      (Data.Aeson.Key.toText -> cName, cData) : xs ->
        let new = over tenantConfigL (Map.insert (ConnectionName cName) (decodeConnection cData)) tc
         in insertTenantConnections defaultParent new xs

    decodeConnection :: Data.Aeson.Value -> TenantConnectionConfig
    decodeConnection cnx =
      let configProjects = getProjects "config-projects"
          untrustedProjects = getProjects "untrusted-projects"
       in TenantConnectionConfig {..}
      where
        defaultPaths = [".zuul.yaml", "zuul.yaml", ".zuul.d/", "zuul.d/"]
        getProjects :: Text -> [ProjectNameWithOptions]
        getProjects ptype = case HM.lookup (Data.Aeson.Key.fromText ptype) $ unwrapObject cnx of
          Just (Data.Aeson.String name) -> [ProjectNameWithOptions (ProjectName name) mempty defaultPaths]
          Just (Data.Aeson.Array vec) -> getProject <$> concatMap (HM.toList . unwrapObject) (V.toList vec)
          _ -> []
        -- TODO: support https://zuul-ci.org/docs/zuul/latest/tenants.html#attr-tenant.untrusted-projects.%3Cproject-group%3E
        getProject :: (Data.Aeson.Key.Key, Data.Aeson.Value) -> ProjectNameWithOptions
        getProject (Data.Aeson.Key.toText -> name, options') =
          let options = unwrapObject options'
              included = Set.fromList $ toItemType <$> decodeAsList "include" id options
              excluded = Set.fromList $ toItemType <$> decodeAsList "exclude" id options
              includedElements
                | isJust $ HM.lookup "include" options = included
                | isJust $ HM.lookup "exclude" options = Set.difference allItems excluded
                | otherwise = allItems
              extraConfigPaths = [] -- TODO: decode attribute
           in ProjectNameWithOptions (ProjectName name) includedElements (extraConfigPaths <> defaultPaths)

getTenantProjects :: ConnectionsConfig -> TenantsConfig -> TenantName -> Maybe TenantProjects
getTenantProjects connections tenantsConfig tenantName =
  let tenantConfig = Map.lookup tenantName $ view tenantsConfigL tenantsConfig
      tenantLayout = Map.toList <$> (view tenantConfigL <$> tenantConfig)
   in concatMap extractProject <$> tenantLayout
  where
    extractProject :: (ConnectionName, TenantConnectionConfig) -> TenantProjects
    extractProject (connectionName, TenantConnectionConfig {..}) =
      let projects = configProjects <> untrustedProjects
          providerName = case Map.lookup connectionName connections of
            Just (ConnectionCName pn) -> ProviderName pn
            Nothing -> error "Unable to find project connection's provider name"
       in ( \ProjectNameWithOptions {..} ->
              ( CanonicalProjectName (providerName, projectName),
                Set.toList includedConfigElements
              )
          )
            <$> projects

tenantResolver :: TenantsConfig -> ConnectionsConfig -> ConfigLoc -> ZuulConfigType -> [TenantName]
tenantResolver tenants connections configLoc zct = matches
  where
    matches = map fst $ filter (containsProject . snd) $ Map.toList $ view tenantsConfigL tenants
    containsProject :: TenantConfig -> Bool
    containsProject tc = any containsProject' $ Map.toList $ view tenantConfigL tc
    containsProject' :: (ConnectionName, TenantConnectionConfig) -> Bool
    containsProject' (cn, TenantConnectionConfig {..}) = any matchProject $ configProjects <> untrustedProjects
      where
        matchProject :: ProjectNameWithOptions -> Bool
        matchProject ProjectNameWithOptions {..} =
          let providerName = case Map.lookup cn connections of
                Just (ConnectionCName pn) -> ProviderName pn
                Nothing -> error "Unable to find project connection's provider name"
           in and
                [ CanonicalProjectName (providerName, projectName) == clProject configLoc,
                  zct `Set.member` includedConfigElements,
                  any matchPath configPaths
                ]
        matchPath :: FilePath -> Bool
        matchPath fp = fp `Data.List.isPrefixOf` getConfigPath (clPath configLoc)

getTenantDefaultBaseJob :: TenantsConfig -> TenantName -> Maybe JobName
getTenantDefaultBaseJob tenantsConfig tenantName =
  let tenantConfig = Map.lookup tenantName $ view tenantsConfigL tenantsConfig
   in defaultParent <$> tenantConfig
