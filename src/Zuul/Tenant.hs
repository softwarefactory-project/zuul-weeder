-- |
-- Module      : Zuul.Tenant
-- Description : Helper for Zuul tenant config main.yaml
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
--
-- Maintainer  : tdecacqu@redhat.com, fboucher@redhat.com
-- Stability   : provisional
-- Portability : portable
--
-- The Zuul Tenants configuration (main.yaml)
module Zuul.Tenant
  ( TenantsConfig (..),
    TenantConfig (..),
    TenantConnectionConfig (..),
    TenantProject (..),
    decodeTenantsConfig,
    tenantResolver,
  )
where

import Data.Aeson qualified
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM (lookup, toList)
import Data.Aeson.Types qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as V
import Zuul.Config
import Zuul.ServiceConfig (ServiceConfig (..))
import Zuul.ZooKeeper (ZKTenantsConfig (..))
import ZuulWeeder.Prelude

allItems :: Set ZuulConfigType
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

-- | The project configuration for a tenant.
data TenantProject = TenantProject
  { -- | The project name.
    name :: ProjectName,
    -- | The list of included elements.
    includedConfigElements :: Set ZuulConfigType,
    -- | The list of config location prefix, default to: [".zuul.yaml", "zuul.yaml", "zuul.d", ".zuul.d"]
    configPaths :: [FilePathT]
  }
  deriving (Show, Eq, Ord, Generic)

-- | The tenant connection source configuration.
data TenantConnectionConfig = TenantConnectionConfig
  { -- | The config projects
    configProjects :: [TenantProject],
    -- | The untrusted projects
    untrustedProjects :: [TenantProject]
  }
  deriving (Show, Eq, Ord, Generic)

-- | Single tenant configuration.
data TenantConfig = TenantConfig
  { -- | The default base job name, default to "base".
    defaultParent :: JobName,
    -- | The list of project grouped per source connection.
    connections :: Map ConnectionName TenantConnectionConfig
  }
  deriving (Show, Eq, Ord, Generic)

-- | All the tenants configuration.
newtype TenantsConfig = TenantsConfig
  { tenants :: Map TenantName TenantConfig
  }
  deriving (Show, Eq, Ord, Generic)

-- | Decode the 'TenantsConfig' from a ZK data file.
decodeTenantsConfig :: ZKTenantsConfig -> Maybe TenantsConfig
decodeTenantsConfig (ZKTenantsConfig value) = case value of
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
        let new = tc & #tenants `over` Map.insert (TenantName tName) (decodeTenant tData)
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
        let new = tc & #connections `over` Map.insert (ConnectionName cName) (decodeConnection cData)
         in insertTenantConnections defaultParent new xs

    decodeConnection :: Data.Aeson.Value -> TenantConnectionConfig
    decodeConnection cnx =
      let configProjects = getProjects "config-projects"
          untrustedProjects = getProjects "untrusted-projects"
       in TenantConnectionConfig {configProjects, untrustedProjects}
      where
        defaultPaths = [".zuul.yaml", "zuul.yaml", ".zuul.d/", "zuul.d/"]
        getProjects :: Text -> [TenantProject]
        getProjects ptype = case HM.lookup (Data.Aeson.Key.fromText ptype) $ unwrapObject cnx of
          Just (Data.Aeson.String name) -> [TenantProject (ProjectName name) mempty defaultPaths]
          Just (Data.Aeson.Array vec) -> getProject <$> concatMap decodeProjectItems (V.toList vec)
          _ -> []

        decodeProjectItems :: Data.Aeson.Value -> [(Data.Aeson.Key.Key, Data.Aeson.Value)]
        decodeProjectItems x = case x of
          Data.Aeson.Object o
            | isJust (HM.lookup "projects" o) -> map (getProjectGroup x) (decodeAsList "projects" id o)
            | otherwise -> HM.toList o
          Data.Aeson.String v -> [(Data.Aeson.Key.fromText v, Data.Aeson.Types.emptyObject)]
          _ -> error $ "Invalid object definition: " <> show x

        getProjectGroup :: Data.Aeson.Value -> Text -> (Data.Aeson.Key.Key, Data.Aeson.Value)
        getProjectGroup v n = (Data.Aeson.Key.fromText n, v)

        getProject :: (Data.Aeson.Key.Key, Data.Aeson.Value) -> TenantProject
        getProject (Data.Aeson.Key.toText -> name, options') =
          let options = unwrapObject options'
              included = Set.fromList $ toItemType <$> decodeAsList "include" id options
              excluded = Set.fromList $ toItemType <$> decodeAsList "exclude" id options
              includedElements
                | isJust $ HM.lookup "include" options = included
                | isJust $ HM.lookup "exclude" options = Set.difference allItems excluded
                | otherwise = allItems
              extraConfigPaths = [] -- TODO: decode attribute
           in TenantProject (ProjectName name) includedElements (extraConfigPaths <> defaultPaths)

-- | Helper function to return the list of tenants matching a 'ConfigLoc'
tenantResolver ::
  -- | The zuul.conf for the list of connection names
  ServiceConfig ->
  -- | The main.yaml tenants config
  TenantsConfig ->
  -- | The config location to resolve
  ConfigLoc ->
  -- | The config element type
  ZuulConfigType ->
  -- | The list of tenant depending on the element
  Set TenantName
tenantResolver serviceConfig tenantsConfig configLoc zct =
  Set.fromList $ map fst $ filter (containsProject . snd) $ Map.toList $ tenantsConfig.tenants
  where
    containsProject :: TenantConfig -> Bool
    containsProject tc = any containsProject' $ Map.toList $ tc.connections
    containsProject' :: (ConnectionName, TenantConnectionConfig) -> Bool
    containsProject' (cn, tcc) = any matchProject $ tcc.configProjects <> tcc.untrustedProjects
      where
        matchProject :: TenantProject -> Bool
        matchProject tp =
          let providerName = case Map.lookup cn serviceConfig.connections of
                Just pn -> pn
                Nothing -> error "Unable to find project connection's provider name"
           in and
                [ CanonicalProjectName providerName tp.name == configLoc.project,
                  zct `Set.member` tp.includedConfigElements,
                  any matchPath tp.configPaths
                ]
        matchPath :: FilePathT -> Bool
        matchPath fp = from fp `Text.isPrefixOf` from configLoc.path
