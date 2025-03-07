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
module Zuul.Tenant (
  TenantsConfig (..),
  TenantConfig (..),
  TenantConnectionConfig (..),
  TenantProject (..),
  decodeTenantsConfig,
  TenantResolver (..),
  mkResolver,
  getCanonicalProjects,
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
  { name :: ProjectName
  -- ^ The project name.
  , includedConfigElements :: Set ZuulConfigType
  -- ^ The list of included elements.
  , configPaths :: [FilePathT]
  -- ^ The list of config location prefix, default to: [".zuul.yaml", "zuul.yaml", "zuul.d", ".zuul.d"]
  }
  deriving (Show, Eq, Ord, Generic)

-- | The tenant connection source configuration.
data TenantConnectionConfig = TenantConnectionConfig
  { configProjects :: [TenantProject]
  -- ^ The config projects
  , untrustedProjects :: [TenantProject]
  -- ^ The untrusted projects
  }
  deriving (Show, Eq, Ord, Generic)

-- | Single tenant configuration.
data TenantConfig = TenantConfig
  { defaultParent :: JobName
  -- ^ The default base job name, default to "base".
  , connections :: Map ConnectionName TenantConnectionConfig
  -- ^ The list of project grouped per source connection.
  }
  deriving (Show, Eq, Ord, Generic)

-- | All the tenants configuration.
newtype TenantsConfig = TenantsConfig
  { tenants :: Map TenantName TenantConfig
  }
  deriving (Show, Eq, Ord, Generic)

getCanonicalProjects :: Map ConnectionName ProviderName -> TenantsConfig -> Map CanonicalProjectName (Set TenantName)
getCanonicalProjects allConn tcs = foldr addProject mempty allProjects
 where
  addProject :: (TenantName, CanonicalProjectName) -> Map CanonicalProjectName (Set TenantName) -> Map CanonicalProjectName (Set TenantName)
  addProject (tenant, proj) = Map.insertWith Set.union proj (Set.singleton tenant)

  allProjects :: [(TenantName, CanonicalProjectName)]
  allProjects = concatMap getProjList $ Map.toList tcs.tenants
  getProjList :: (TenantName, TenantConfig) -> [(TenantName, CanonicalProjectName)]
  getProjList (tenant, tc) = concatMap (getTenantProjList tenant) $ Map.toList tc.connections

  getTenantProjList :: TenantName -> (ConnectionName, TenantConnectionConfig) -> [(TenantName, CanonicalProjectName)]
  getTenantProjList tenant (conn, tcc) = map mkTenantProj $ tcc.configProjects <> tcc.untrustedProjects
   where
    mkCanon = CanonicalProjectName (fromMaybe (error "unknown conn") (Map.lookup conn allConn))
    mkTenantProj :: TenantProject -> (TenantName, CanonicalProjectName)
    mkTenantProj tp = (tenant, mkCanon tp.name)

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
      tenants <- traverse (decodeTenant . first Data.Aeson.Key.toText) tenantsValues
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
    connections <- Map.fromList <$> traverse (decodeConnection . first Data.Aeson.Key.toText) (HM.toList sources)
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
      x@(Data.Aeson.Object o) -> case HM.lookup "projects" o of
        -- Project configuration is a list of project name with a shared config
        Just (Data.Aeson.Array xs) -> map (,x) <$> traverse decodeProjectsList (V.toList xs)
        Just _ -> decodeFail "Unexpected projects value" x
        -- It's a single project with a custom config
        Nothing -> pure $ first Data.Aeson.Key.toText <$> HM.toList o
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

-- | Tenant information to resolve project location
data TenantResolver = TenantResolver
  { resolveTenants ::
      -- The config location to resolve
      BaseConfigLoc Void ->
      -- The config element type
      ZuulConfigType ->
      -- The list of tenant allowing the element
      Set TenantName
  , resolveProject ::
      -- The project definition config location
      ConfigLoc ->
      -- The project name
      ProjectName ->
      -- The resolved project name
      Maybe CanonicalProjectName
  }

mkResolver ::
  -- | The zuul.conf for the list of connection names
  ServiceConfig ->
  -- | The main.yaml tenants config
  TenantsConfig ->
  TenantResolver
mkResolver sc tc = TenantResolver {resolveTenants = resolveTenant sc tc, resolveProject}
 where
  resolveProject :: ConfigLoc -> ProjectName -> Maybe CanonicalProjectName
  resolveProject loc rawName
    | -- The project is already qualified
      provider `Set.member` allProviders tc.tenants =
        Just $ CanonicalProjectName provider name
    | -- Otherwise look for a matching project in the tenant configs
      otherwise = case Set.toList (Set.filter (\cp -> cp.project == rawName) (allProjects tc.tenants)) of
        [x] -> Just x
        _ -> Nothing
   where
    (ProviderName -> provider, ProjectName . Text.tail -> name) = Text.span (/= '/') (from rawName)

    allTenantsConfig :: Map TenantName TenantConfig -> [TenantConfig]
    allTenantsConfig =
      map snd
        . filter (\(tenant, _) -> tenant `Set.member` loc.tenants)
        . Map.toList

    allProjects :: Map TenantName TenantConfig -> Set CanonicalProjectName
    allProjects =
      Set.fromList
        . mapMaybe getCanonicalName
        . concatMap getProjects
        . allTenantsConfig
     where
      getProjects tenantConfig =
        let tenantProjects :: (ConnectionName, TenantConnectionConfig) -> [(ConnectionName, ProjectName)]
            tenantProjects (conn, tcc) =
              map toProject tcc.configProjects <> map toProject tcc.untrustedProjects
             where
              toProject tp = (conn, tp.name)
         in concatMap tenantProjects $ Map.toList tenantConfig.connections
      getCanonicalName (cn, pname) = case Map.lookup cn sc.connections of
        Just pn -> Just (CanonicalProjectName pn pname)
        Nothing -> Nothing

    -- The list of provider named based on the available tenants connection.
    allProviders :: Map TenantName TenantConfig -> Set ProviderName
    allProviders =
      Set.fromList
        . mapMaybe (\cn -> Map.lookup cn sc.connections)
        . concatMap (\tenantConfig -> Map.keys tenantConfig.connections)
        . allTenantsConfig

resolveTenant ::
  ServiceConfig ->
  TenantsConfig ->
  BaseConfigLoc Void ->
  ZuulConfigType ->
  Set TenantName
resolveTenant serviceConfig tenantsConfig configLoc zct =
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
            Nothing -> error $ "Unable to find project connection's provider name: " <> show cn <> ", for project: " <> show tp
       in and
            [ CanonicalProjectName providerName tp.name == configLoc.project
            , zct `Set.member` tp.includedConfigElements
            , any matchPath tp.configPaths
            ]
    matchPath :: FilePathT -> Bool
    matchPath fp = from fp `Text.isPrefixOf` from configLoc.path

decodeProjectsList :: Value -> Decoder Text
decodeProjectsList = \case
  (Data.Aeson.String v) -> pure v
  v@(Data.Aeson.Object o) -> case HM.toList o of
    [(k, _)] -> pure $ Data.Aeson.Key.toText k
    _ -> decodeFail "Expected a project object with one key" v
  v -> decodeFail "Expected a string" v
