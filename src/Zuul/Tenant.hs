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

import Data.Aeson (Object)
import Data.Aeson qualified
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified as HM (lookup, toList)
import Data.Aeson.Types qualified
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
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
decodeTenantsConfig :: ZKTenantsConfig -> Either Text TenantsConfig
decodeTenantsConfig (ZKTenantsConfig value) = case decoded of
  (Decoder (Right x)) -> Right x
  (Decoder (Left e)) -> Left $ from ("Decode error:" <> show e)
  where
    decoded :: Decoder TenantsConfig
    decoded = case value of
      Data.Aeson.Object hm -> do
        abide <- decodeObject =<< decodeObjectAttribute "unparsed_abide" hm
        tenantsValues <- HM.toList <$> (decodeObject =<< decodeObjectAttribute "tenants" abide)
        tenants <- traverse decodeTenant (first Data.Aeson.Key.toText <$> tenantsValues)
        pure $ TenantsConfig $ Map.fromList tenants
      _ -> decodeFail "Invalid root tenants config" value

    decodeTenant :: (Text, Data.Aeson.Value) -> Decoder (TenantName, TenantConfig)
    decodeTenant (TenantName -> name, v) = do
      obj <- decodeObject v
      config <- decodeTenantConfig obj
      pure (name, config)

    decodeTenantConfig :: Object -> Decoder TenantConfig
    decodeTenantConfig obj = do
      sources <- decodeObject =<< decodeObjectAttribute "source" obj
      connections <- Map.fromList <$> traverse decodeConnection (first Data.Aeson.Key.toText <$> HM.toList sources)
      defaultParent <-
        JobName <$> case HM.lookup "default-parent" obj of
          Just (Data.Aeson.String n) -> pure n
          Just v -> decodeFail "Invalid default-parent value" v
          Nothing -> pure "base"
      pure $ TenantConfig {defaultParent, connections}

    decodeConnection :: (Text, Data.Aeson.Value) -> Decoder (ConnectionName, TenantConnectionConfig)
    decodeConnection (ConnectionName -> cname, v) = do
      va <- decodeObject v
      configProjects <- getProjects "config-projects" va
      untrustedProjects <- getProjects "untrusted-projects" va
      pure (cname, TenantConnectionConfig {configProjects, untrustedProjects})
      where
        defaultPaths = [".zuul.yaml", "zuul.yaml", ".zuul.d/", "zuul.d/"]

        getProjects :: Text -> Object -> Decoder [TenantProject]
        getProjects (Data.Aeson.Key.fromText -> k) o =
          case HM.lookup k o of
            Just projectList ->
              (traverse decodeProjects =<< decodeList projectList)
                >>= traverse decodeProject . concat
            Nothing -> pure []

        -- Decode a single project or a project configuration list
        decodeProjects :: Value -> Decoder [(Text, Value)]
        decodeProjects = \case
          x@(Data.Aeson.Object o)
            -- Project configuration is a list of project name with a shared config
            | isJust (HM.lookup "projects" o) -> map (,x) <$> decodeAsList "projects" id o
            -- It's a single project with a custom config
            | otherwise -> pure $ first Data.Aeson.Key.toText <$> HM.toList o
          -- Project configuration is a single name
          Data.Aeson.String name -> pure [(name, Data.Aeson.Types.emptyObject)]
          anyOther -> decodeFail "Invalid project definition" anyOther

        decodeProject :: (Text, Value) -> Decoder TenantProject
        decodeProject (name, options') = do
          options <- decodeObject options'
          included <- Set.fromList <$> decodeAsList "include" toItemType options
          excluded <- Set.fromList <$> decodeAsList "exclude" toItemType options
          let includedElements
                | isJust $ HM.lookup "include" options = included
                | isJust $ HM.lookup "exclude" options = Set.difference allItems excluded
                | otherwise = allItems
              extraConfigPaths = [] -- TODO: decode attribute
          pure $ TenantProject (ProjectName name) includedElements (extraConfigPaths <> defaultPaths)

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
