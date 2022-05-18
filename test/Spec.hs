module Main (main) where

import Data.Aeson (Value, eitherDecodeFileStrict, object)
import Data.Map qualified (toList)
import Data.Map qualified as Map
import Data.Text.Lazy.Encoding qualified as LText
import Data.Yaml qualified as Y (decodeFileEither)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Text.Pretty.Simple (pShowNoColor)
import Zuul.Config
import Zuul.ConfigLoader
import Zuul.ServiceConfig
import Zuul.Tenant
import Zuul.ZooKeeper
import ZuulWeeder.Graph
import ZuulWeeder.Main qualified
import ZuulWeeder.Prelude
import ZuulWeeder.UI

main :: IO ()
main = do
  demo <- ZuulWeeder.Main.demoConfig
  defaultMain (testGroup "Tests" $ tests demo)

fixturesPath :: FilePathT
fixturesPath = "test/fixtures"

loadFixture :: FilePathT -> IO Value
loadFixture name = do
  contentE <- Y.decodeFileEither $ getPath' $ fixturesPath </> name <> ".yaml"
  case contentE of
    Left _any -> error $ "Unable to decode fixture " <> getPath' name
    Right bs -> pure bs

goldenTest :: Show a => TestName -> FilePathT -> IO a -> TestTree
goldenTest name fp action = goldenVsString name (getPath' $ goldenPath fp) do
  res <- action
  pure . LText.encodeUtf8 . pShowNoColor $ res

goldenPath :: FilePathT -> FilePathT
goldenPath name = fixturesPath </> name <> ".golden"

loadJSONFixture :: FilePathT -> IO Value
loadJSONFixture name = do
  contentE <- eitherDecodeFileStrict $ getPath' $ fixturesPath </> name <> ".json"
  case contentE of
    Left _any -> error $ "Unable to decode fixture " <> getPath' name
    Right bs -> pure bs

tests :: Analysis -> [TestTree]
tests demo =
  [ testGroup
      "ZooKeeper module"
      [ testCase "Extract data from ZK path" extractDataZKPath,
        goldenTest "Decode Jobs config" "jobs" decodeJobsConfig,
        goldenTest "Decode Projects config" "projects" decodeProjectsConfig,
        goldenTest "Decode Nodesets config" "nodesets" decodeNodesetsConfig,
        goldenTest "Decode Project templates config" "project-templates" decodeProjectTemplatesConfig,
        goldenTest "Decode Pipeline config" "pipelines" decodeProjectPipeline,
        goldenTest "Decode Tenant config" "system-config" decodeTenants,
        testCase "Decode Connections config" decodeServiceConfig,
        goldenTest "Get Tenant projects" "zuul" testGetTenantProjects,
        testCase "Compute gitweb links" computeGitwebLinks
      ],
    testGroup
      "Integration"
      [goldenTest "Git Nodelabel test" "vertex-git" validateGitConfig]
  ]
  where
    validateGitConfig = pure $ Map.lookup (NodeLabelName "cloud-rhel-7") demo.config.nodeLabels

    extractDataZKPath =
      let path = "/tmp/zk-dump/zuul/config/cache/sftests.com%2Fzuul-jobs/master/files/zuul.d%2Fhaskell-jobs.yaml/0000000000/ZKDATA"
          obj = object []
       in assertEqual
            "ZK path is extracted to ZKConfig"
            (mkZKConfig obj path)
            ( Just
                ( ZKConfig
                    { provider = "sftests.com",
                      project = "zuul-jobs",
                      branch = "master",
                      filePath = "zuul.d/haskell-jobs.yaml",
                      fullPath = path,
                      zkJSONData = obj
                    }
                )
            )
    decodeJobsConfig = do
      json <- loadFixture "jobs"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
      pure $ sort decoded

    decodeProjectsConfig = do
      json <- loadFixture "projects"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
      pure $ sort decoded

    decodeNodesetsConfig = do
      json <- loadFixture "nodesets"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
      pure $ sort decoded

    decodeProjectTemplatesConfig = do
      json <- loadFixture "project-templates"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
      pure $ sort decoded

    decodeProjectPipeline = do
      json <- loadFixture "pipelines"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
      pure $ sort decoded

    decodeTenants = do
      json <- loadJSONFixture "system-config"
      let decoded = fromMaybe (error "oops") $ decodeTenantsConfig (ZKSystemConfig json)
      pure $ Data.Map.toList decoded.tenants

    decodeServiceConfig = do
      conf <- fromEither <$> runExceptT (readServiceConfig (readFileText $ fixturesPath </> "zuul.conf"))
      let expected = [(ConnectionName "gerrit", ProviderName "sftests.com")]
      assertEqual "Expect connections extracted from Zuul.conf" expected (Data.Map.toList conf.connections)
      assertEqual "Expect zk conf" (ZKConnection ["localhost", "key.pem", "cert.pem", "ca.pem"]) conf.zookeeper

    testGetTenantProjects = do
      conf <- fromEither <$> runExceptT (readServiceConfig (readFileText $ fixturesPath </> "zuul.conf"))
      json <- loadJSONFixture "system-config"
      let tenantsConfig = fromMaybe (error "oops") $ decodeTenantsConfig (ZKSystemConfig json)
          tenantConfig = getTenantProjects conf tenantsConfig (TenantName "local")
          tenantConfigAlt = getTenantProjects conf tenantsConfig (TenantName "unknown")
      assertEqual "Expect empty tenant projects" Nothing tenantConfigAlt
      pure tenantConfig

    computeGitwebLinks = do
      let testConfigLoc =
            let project = CanonicalProjectName (ProviderName "sftests.com", ProjectName "sf-config")
                branch = BranchName "main"
                path = FilePathT "zuul.d/pipelines.yaml"
                url = GerritUrl "https://managesf.sftests.com"
                tenants = mempty
             in ConfigLoc {..}
          expected = "https://managesf.sftests.com/plugins/gitiles/sf-config/+/refs/heads/main/zuul.d/pipelines.yaml"
      assertEqual "Expect gitweb url" expected $ configLocUrl testConfigLoc
