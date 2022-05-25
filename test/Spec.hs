module Main (main) where

import Data.Aeson (eitherDecodeFileStrict, object)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy.Encoding qualified as LText
import Data.Yaml qualified as Y (decodeFileEither)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Zuul.Config
import Zuul.ConfigLoader
import Zuul.ServiceConfig
import Zuul.Tenant
import Zuul.ZooKeeper
import ZuulWeeder qualified
import ZuulWeeder.Graph
import ZuulWeeder.Prelude
import ZuulWeeder.UI

main :: IO ()
main = do
  demo <- ZuulWeeder.demoConfig
  defaultMain (testGroup "Tests" $ tests demo)

fixturesPath :: FilePathT
fixturesPath = "test/fixtures"

loadFixture :: FilePathT -> IO Value
loadFixture name = do
  contentE <- Y.decodeFileEither $ getPath $ fixturesPath </> name <> ".yaml"
  case contentE of
    Left _any -> error $ "Unable to decode fixture " <> getPath name
    Right bs -> pure bs

goldenTest :: Show a => TestName -> FilePathT -> IO a -> TestTree
goldenTest name fp action = goldenVsString name (getPath $ goldenPath fp) do
  res <- action
  pure . LText.encodeUtf8 . pShowNoColor $ res

goldenPath :: FilePathT -> FilePathT
goldenPath name = fixturesPath </> name <> ".golden"

loadJSONFixture :: FilePathT -> IO Value
loadJSONFixture name = do
  contentE <- eitherDecodeFileStrict $ getPath $ fixturesPath </> name <> ".json"
  case contentE of
    Left _any -> error $ "Unable to decode fixture " <> getPath name
    Right bs -> pure bs

tests :: Analysis -> [TestTree]
tests demo =
  [ testGroup
      "ZooKeeper module"
      [ testCase "Extract data from ZK path" extractDataZKPath,
        goldenTest "Decode Jobs config" "jobs" (decodeConfigFixture "jobs"),
        goldenTest "Decode Projects config" "projects" (decodeConfigFixture "projects"),
        goldenTest "Decode Nodesets config" "nodesets" (decodeConfigFixture "nodesets"),
        goldenTest "Decode Project templates config" "project-templates" (decodeConfigFixture "project-templates"),
        goldenTest "Decode Pipeline config" "pipelines" (decodeConfigFixture "pipelines"),
        goldenTest "Decode Tenant config" "system-config" decodeTenants,
        testCase "Decode Connections config" decodeServiceConfig,
        goldenTest "Get Tenant projects" "zuul" testGetTenantProjects,
        testCase "Compute gitweb links" computeGitwebLinks
      ],
    testGroup
      "Integration"
      [ goldenTest "Git Nodelabel test" "vertex-git" validateGitConfig,
        goldenTest "Pipeline jobs are linked" "vertex-pipeline" validatePipelineConfig,
        testCase "Demo graph errors" validateNoGraphError,
        testCase "Demo config errors" validateNoConfigError
      ]
  ]
  where
    validateNoGraphError = assertEqual "graph error" [] demo.graphErrors
    validateNoConfigError = assertBool ("config error: " <> show demo.config.configErrors) (List.null demo.config.configErrors)
    validateGitConfig = pure $ Map.lookup (NodeLabelName "cloud-rhel-7") demo.config.nodeLabels

    validatePipelineConfig = do
      let mkInfo n =
            let f = ZuulWeeder.Graph.findReachableForest Nothing (NE.singleton $ Vertex n (Set.fromList [TenantName "local"]))
             in ( from n <> " is needed by" :: Text,
                  f demo.dependentGraph,
                  from n <> " requires" :: Text,
                  f demo.dependencyGraph
                )
      pure
        [ mkInfo (VPipeline (PipelineName "check")),
          mkInfo (VJob (JobName "wallaby-job"))
        ]

    extractDataZKPath =
      let path = "/tmp/zk-dump/zuul/config/cache/sftests.com%2Fzuul-jobs/master/files/zuul.d%2Fhaskell-jobs.yaml/0000000000/ZKDATA"
          obj = object []
       in assertEqual
            "ZK path is extracted to ZKFile"
            (mkZKFile obj path)
            ( Just
                ( ZKFile
                    { provider = "sftests.com",
                      project = "zuul-jobs",
                      branch = "master",
                      filePath = "zuul.d/haskell-jobs.yaml",
                      fullPath = path,
                      zkJSONData = obj
                    }
                )
            )
    fakeProject = (CanonicalProjectName (ProviderName "") (ProjectName ""), BranchName "")

    decodeConfigFixture :: FilePathT -> IO [ZuulConfigElement]
    decodeConfigFixture fp = do
      json <- loadFixture fp
      pure (unwrap <$> decodeConfig fakeProject json)
      where
        unwrap (Decoder (Left e)) = error $ "Decoding fail: " <> show e
        unwrap (Decoder (Right x)) = x

    decodeTenants = do
      json <- loadJSONFixture "system-config"
      let decoded = fromEither $ decodeTenantsConfig (ZKTenantsConfig json)
      pure $ Map.toList decoded.tenants

    decodeServiceConfig = do
      conf <- fromEither <$> runExceptT (readServiceConfig (readFileText $ fixturesPath </> "zuul.conf"))
      let expected = [(ConnectionName "gerrit", ProviderName "sftests.com")]
      assertEqual "Expect connections extracted from Zuul.conf" expected (Map.toList conf.connections)
      assertEqual "Expect zk conf" (ZKConnection ["localhost", "key.pem", "cert.pem", "ca.pem"]) conf.zookeeper

    testGetTenantProjects = do
      json <- loadJSONFixture "system-config"
      pure $ decodeTenantsConfig (ZKTenantsConfig json)

    computeGitwebLinks = do
      let testConfigLoc =
            ConfigLoc
              { project = CanonicalProjectName (ProviderName "sftests.com") (ProjectName "sf-config"),
                branch = BranchName "main",
                path = FilePathT "zuul.d/pipelines.yaml",
                url = GerritUrl "https://managesf.sftests.com",
                tenants = mempty
              }
          expected = "https://managesf.sftests.com/plugins/gitiles/sf-config/+/refs/heads/main/zuul.d/pipelines.yaml"
      assertEqual "Expect gitweb url" expected $ configLocUrl testConfigLoc
