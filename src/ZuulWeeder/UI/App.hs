-- |
-- Module      : ZuulWeeder.UI.App
-- Description : The User Interface entrypoint and API
-- Copyright   : (c) Red Hat, 2022
-- License     : Apache-2.0
module ZuulWeeder.UI.App (app) where

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Lucid
import Servant hiding (Context)
import Servant.HTML.Lucid (HTML)
import Servant.Server.StaticFiles qualified
import Zuul.Config
import Zuul.ConfigLoader (Config (..))
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI
import ZuulWeeder.UI.D3 (D3Graph, toD3Graph)
import ZuulWeeder.UI.Info (infoComponent)
import ZuulWeeder.UI.Tree (treeComponent)

newtype VertexTypeUrl = VTU (Text -> VertexName)

data DecodeProject = DecodeCanonical | DecodeTemplate | DecodeRegex

instance FromHttpApiData VertexTypeUrl where
  parseUrlPiece txt = pure . VTU $ case txt of
    "abstract-job" -> VAbstractJob . JobName
    "job" -> VJob . JobName
    "semaphore" -> VSemaphore . SemaphoreName
    "secret" -> VSecret . SecretName
    "nodeset" -> VNodeset . NodesetName
    "label" -> VNodeLabel . NodeLabelName
    "queue" -> VQueue . QueueName
    "project-config" -> VProject . decodeCanonical
    "project-regex" -> VProjectRegex . ProjectRegex
    "project-template" -> VProjectTemplate . ProjectTemplateName
    "pipeline" -> VPipeline . PipelineName
    "project-pipeline" -> splitPipeline DecodeCanonical
    "regex-pipeline" -> splitPipeline DecodeRegex
    "template-pipeline" -> splitPipeline DecodeTemplate
    "trigger" -> VTrigger . ConnectionName
    "reporter" -> VReporter . ConnectionName
    "repository" -> VRepository . decodeCanonical
    _ -> error $ "Unknown obj type: " <> from txt
    where
      -- Assume the provider name is the first element of a '/' separated path
      decodeCanonical :: Text -> CanonicalProjectName
      decodeCanonical t =
        let (ProviderName -> provider, Text.tail -> name) = Text.span (/= '/') t
         in CanonicalProjectName provider (ProjectName name)

      -- Project/Template Pipeline name is separated by a ':'
      splitPipeline :: DecodeProject -> Text -> VertexName
      splitPipeline dp t =
        let (PipelineName -> pipeline, Text.tail -> name) = Text.span (/= ':') t
         in case dp of
              DecodeCanonical -> VProjectPipeline pipeline (decodeCanonical name)
              DecodeTemplate -> VTemplatePipeline pipeline (ProjectTemplateName name)
              DecodeRegex -> VRegexPipeline pipeline (ProjectRegex name)

newtype VertexNameUrl = VNU {getVNU :: Text}

instance FromHttpApiData VertexNameUrl where
  parseUrlPiece = pure . VNU

newtype TenantsUrl = TNU {getTNU :: Set TenantName}

instance FromHttpApiData TenantsUrl where
  parseUrlPiece piece = pure . TNU $ Set.fromList (TenantName <$> Text.split (== ',') piece)

type GetRequest = Header "HX-Request" Text :> Get '[HTML] (Html ())

-- | The zuul-weeder base HTTP API.
-- The HX-Request header is set by inline navigation, when it is missing, the full body is returned.
type BaseAPI =
  GetRequest
    :<|> "about" :> GetRequest
    :<|> "search" :> GetRequest
    :<|> "info" :> GetRequest
    :<|> "debug" :> GetRequest
    :<|> "export" :> Get '[JSON] Config
    :<|> "object" :> Capture "type" VertexTypeUrl :> CaptureAll "name" VertexNameUrl :> GetRequest
    :<|> "search_results" :> SearchPath
    :<|> "search" :> Capture "query" Text :> Get '[HTML] (Html ())
    :<|> "data.json" :> Get '[JSON] D3Graph

type SearchPath =
  ReqBody '[FormUrlEncoded] SearchForm
    :> Post '[HTML] (Headers '[Header "HX-Push" Text] (Html ()))

type TenantAPI = "tenant" :> Capture "tenant" TenantsUrl :> BaseAPI

type StaticAPI = "dists" :> Raw

type API = StaticAPI :<|> BaseAPI :<|> TenantAPI

newtype CacheRender = CacheRender
  { infoPages :: Map Context CachePage
  }

data CachePage = CachePage
  { _pageAge :: Int64,
    _pageContent :: Html ()
  }

instance Semigroup CacheRender where
  a <> b = CacheRender (a.infoPages <> b.infoPages)

instance Monoid CacheRender where
  mempty = CacheRender mempty

-- | Creates the Web Application Interface (wai).
app ::
  -- | An action to refresh the analysis
  IO AnalysisStatus ->
  MVar CacheRender ->
  -- | The base path of the interface, used to render absolute links
  BasePath ->
  -- | The location of the static files
  FilePath ->
  -- | The application to serve
  Application
app config cache rootURL distPath = serve (Proxy @API) rootServer
  where
    rootServer :: Server API
    rootServer =
      Servant.Server.StaticFiles.serveDirectoryWebApp distPath
        :<|> server (Context rootURL UnScoped)
        :<|> server . Context rootURL . Scoped . getTNU

    server :: Context -> Server BaseAPI
    server ctx =
      indexRoute Nothing "" (pure $ welcomeComponent ctx)
        :<|> indexRoute Nothing "about" (pure aboutComponent)
        :<|> flip searchRoute Nothing
        :<|> indexRoute Nothing "info" infoCache
        :<|> indexRoute Nothing "debug" (debugComponent . analysis <$> liftIO config)
        :<|> exportRoute
        :<|> objectRoute
        :<|> searchResultRoute
        :<|> searchRouteWithQuery
        :<|> d3Route
      where
        infoCache :: Handler (Html ())
        infoCache = do
          now <- liftIO getSec
          liftIO $ modifyMVar cache (go now)
          where
            go now m = case Map.lookup ctx m.infoPages of
              Just (CachePage age render) | now - age < 3600 -> do
                pure (m, render)
              _ -> do
                page <- infoComponent ctx <$> liftIO config
                let newMap = Map.insert ctx (CachePage now page) m.infoPages
                pure (CacheRender newMap, page)

        getAnalysisStatus :: Maybe AnalysisStatus -> Handler AnalysisStatus
        getAnalysisStatus = \case
          Nothing -> liftIO config
          Just analysisStatus -> pure analysisStatus

        indexRoute :: Maybe AnalysisStatus -> Text -> Handler (Html ()) -> Maybe a -> Handler (Html ())
        -- The HX-Request header is missing, return the full body
        indexRoute mAnalysisStatus name component Nothing = do
          analysisStatus <- getAnalysisStatus mAnalysisStatus
          mainBody ctx name analysisStatus <$> component
        -- The HX-Request header is set, return the component and update the nav links
        indexRoute mAnalysisStatus name component (Just _htmxRequest) = do
          analysisStatus <- getAnalysisStatus mAnalysisStatus
          componentHtml <- component
          pure do
            navComponent ctx name analysisStatus
            with div_ [class_ "container grid p-4"] componentHtml

        objectRoute :: VertexTypeUrl -> [VertexNameUrl] -> Maybe Text -> Handler (Html ())
        objectRoute (VTU mkName) name htmxRequest = do
          analysisStatus <- liftIO config
          let vname = mkName $ Text.intercalate "/" $ getVNU <$> name
          let vertices = vertexScope ctx.scope $ Set.filter matchVertex analysisStatus.analysis.vertices
                where
                  matchVertex v = v.name == vname
          let component = case NE.nonEmpty vertices of
                Just xs -> pure (treeComponent ctx xs analysisStatus.analysis)
                Nothing -> pure "not found!"
          indexRoute (Just analysisStatus) "object" component htmxRequest

        -- /search/query does not come from htmx, the body is always served
        searchRouteWithQuery query = searchRoute Nothing (Just query)

        searchRoute htmxRequest queryM = do
          analysisStatus <- liftIO config
          result <- case queryM of
            Just query -> do
              pure . snd $ searchResults ctx query analysisStatus.analysis.names
            Nothing -> pure mempty
          indexRoute (Just analysisStatus) "search" (pure $ searchComponent ctx queryM result) htmxRequest

        searchResultRoute req = do
          analysis <- (.analysis) <$> liftIO config
          let (value, result) = searchResults ctx req.query analysis.names
          pure $ addHeader (maybe "false" (mappend (baseUrl ctx <> "search/")) value) result

        d3Route = do
          analysis <- (.analysis) <$> liftIO config
          let graph = dependencyGraph analysis
          pure (toD3Graph ctx.scope graph)

        exportRoute = do
          analysisStatus <- liftIO config
          -- TODO: filter with tenant scope
          pure (analysisStatus.analysis.config)
