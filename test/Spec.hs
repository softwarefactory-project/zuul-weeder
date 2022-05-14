module Main (main) where

import Data.Aeson (Value, eitherDecodeFileStrict, object)
import Data.List (sort)
import Data.Map qualified (toList)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy.Encoding qualified as LText
import Data.Yaml qualified as Y (decodeFileEither)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Text.Pretty.Simple (pShowNoColor)
import Zuul.Config (ConnectionCName (ConnectionCName), readConnections)
import Zuul.ConfigLoader
import Zuul.Tenant
import Zuul.ZKDump (ZKConfig (..), ZKSystemConfig (ZKSystemConfig), mkZKConfig)

main :: IO ()
main = defaultMain (testGroup "Tests" [tests])

fixturesPath :: FilePath
fixturesPath = "test/fixtures"

loadFixture :: FilePath -> IO Value
loadFixture name = do
  contentE <- Y.decodeFileEither $ fixturesPath </> name <> ".yaml"
  case contentE of
    Left _any -> error $ "Unable to decode fixture " <> name
    Right bs -> pure bs

goldenTest :: Show a => TestName -> FilePath -> IO a -> TestTree
goldenTest name fp action = goldenVsString name (goldenPath fp) do
  res <- action
  pure . LText.encodeUtf8 . pShowNoColor $ res

goldenPath :: String -> FilePath
goldenPath name = fixturesPath </> name <> ".golden"

loadJSONFixture :: FilePath -> IO Value
loadJSONFixture name = do
  contentE <- eitherDecodeFileStrict $ fixturesPath </> name <> ".json"
  case contentE of
    Left _any -> error $ "Unable to decode fixture " <> name
    Right bs -> pure bs

tests :: TestTree
tests =
  testGroup
    "ZKDump module"
    [ testCase "Extract data from ZK path" extractDataZKPath,
      goldenTest "Decode Jobs config" "jobs" decodeJobsConfig,
      goldenTest "Decode Projects config" "projects" decodeProjectsConfig,
      goldenTest "Decode Nodesets config" "nodesets" decodeNodesetsConfig,
      goldenTest "Decode Project templates config" "project-templates" decodeProjectTemplatesConfig,
      goldenTest "Decode Pipeline config" "pipelines" decodePipelineConfig,
      goldenTest "Decode Tenant config" "system-config" decodeTenants,
      testCase "Decode Connections config" decodeConnections,
      goldenTest "Get Tenant projects" "zuul" testGetTenantProjects
    ]
  where
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

    decodePipelineConfig = do
      json <- loadFixture "pipelines"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
      pure $ sort decoded

    decodeTenants = do
      json <- loadJSONFixture "system-config"
      let decoded = fromMaybe (error "oops") $ decodeTenantsConfig (ZKSystemConfig json)
      pure $ Data.Map.toList decoded.tenants

    decodeConnections = do
      conns <- readConnections $ fixturesPath </> "zuul.conf"
      let expected = [(ConnectionName "gerrit", ConnectionCName "sftests.com")]
      assertEqual "Expect connections extracted from Zuul.conf" expected (Data.Map.toList conns)

    testGetTenantProjects = do
      conns <- readConnections $ fixturesPath </> "zuul.conf"
      json <- loadJSONFixture "system-config"
      let tenantsConfig = fromMaybe (error "oops") $ decodeTenantsConfig (ZKSystemConfig json)
          tenantConfig = getTenantProjects conns tenantsConfig (TenantName "local")
          tenantConfigAlt = getTenantProjects conns tenantsConfig (TenantName "unknown")
      assertEqual "Expect empty tenant projects" Nothing tenantConfigAlt
      pure tenantConfig
