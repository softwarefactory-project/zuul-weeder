module Main (main) where

import Data.Aeson (Value, eitherDecodeFileStrict, object)
import Data.List (sort)
import Data.Map qualified (fromList, toList)
import Data.Maybe (fromMaybe)
import Data.Set qualified (fromList)
import Data.Yaml qualified as Y (decodeFileEither)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
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
      testCase "Decode Jobs config" decodeJobsConfig,
      testCase "Decode Projects config" decodeProjectsConfig,
      testCase "Decode Nodesets config" decodeNodesetsConfig,
      testCase "Decode Project templates config" decodeProjectTemplatesConfig,
      testCase "Decode Pipeline config" decodePipelineConfig,
      testCase "Decode Tenant config" decodeTenants,
      testCase "Decode Connections config" decodeConnections,
      testCase "Get Tenant projects" testGetTenantProjects
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
          expected =
            [ ZJob (Job {name = JobName "base", parent = Nothing, nodeset = Just (JobAnonymousNodeset [NodeLabelName "pod-centos-7"]), branches = [], dependencies = []}),
              ZJob (Job {name = JobName "config-check", parent = Just (JobName "base"), nodeset = Just (JobAnonymousNodeset []), branches = [BranchName "master"], dependencies = [JobName "job1", JobName "job2"]}),
              ZJob (Job {name = JobName "config-update", parent = Just (JobName "base"), nodeset = Just (JobAnonymousNodeset []), branches = [BranchName "master"], dependencies = []}),
              ZJob (Job {name = JobName "wait-for-changes-ahead", parent = Nothing, nodeset = Just (JobNodeset (NodesetName "nodeset1")), branches = [], dependencies = []})
            ]

      assertEqual "Expect data extracted from Jobs Config elements" (sort expected) (sort decoded)

    decodeProjectsConfig = do
      json <- loadFixture "projects"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ ZProjectPipeline (ProjectPipeline {name = PName (ProjectName "config"), templates = [], pipelines = Data.Set.fromList [PPipeline {name = PipelineName "check", jobs = [PJName (JobName "config-check")]}, PPipeline {name = PipelineName "gate", jobs = [PJName (JobName "config-check")]}, PPipeline {name = PipelineName "post", jobs = [PJName (JobName "config-update")]}]}),
              ZProjectPipeline (ProjectPipeline {name = PNameCannonical (CanonicalProjectName (ProviderName "", ProjectName "")), templates = [TemplateName "sf-ci-jobs"], pipelines = Data.Set.fromList [PPipeline {name = PipelineName "check", jobs = [PJJob (Job {name = JobName "linters", parent = Nothing, nodeset = Just (JobNodeset (NodesetName "linters-pod")), branches = [], dependencies = []})]}, PPipeline {name = PipelineName "experimental", jobs = [PJName (JobName "sf-rpm-build"), PJJob (Job {name = JobName "sf-ci-openshift-integration", parent = Nothing, nodeset = Nothing, branches = [], dependencies = [JobName "sf-rpm-build"]})]}, PPipeline {name = PipelineName "gate", jobs = [PJJob (Job {name = JobName "linters", parent = Nothing, nodeset = Just (JobNodeset (NodesetName "linters-pod")), branches = [], dependencies = []})]}]})
            ]

      assertEqual "Expect data extracted from Projects Config elements" (sort expected) (sort decoded)

    decodeNodesetsConfig = do
      json <- loadFixture "nodesets"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ ZNodeset (Nodeset {name = NodesetName "nodeset1", labels = [NodeLabelName "compute-label", NodeLabelName "compute-label", NodeLabelName "controller-label"]})
            ]

      assertEqual "Expect data extracted from Nodesets Config elements" (sort expected) (sort decoded)

    decodeProjectTemplatesConfig = do
      json <- loadFixture "project-templates"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ZProjectTemplate (ProjectPipeline {name = TName (TemplateName "sf-ci-jobs"), templates = [], pipelines = Data.Set.fromList [PPipeline {name = PipelineName "check", jobs = [PJName (JobName "sf-rpm-build"), PJJob (Job {name = JobName "sf-ci-functional-minimal", parent = Nothing, nodeset = Nothing, branches = [], dependencies = [JobName "sf-rpm-build"]}), PJJob (Job {name = JobName "sf-ci-functional-allinone", parent = Nothing, nodeset = Nothing, branches = [], dependencies = [JobName "sf-rpm-build"]}), PJJob (Job {name = JobName "sf-tenants", parent = Nothing, nodeset = Nothing, branches = [], dependencies = [JobName "sf-rpm-build"]})]}, PPipeline {name = PipelineName "gate", jobs = [PJName (JobName "sf-rpm-build"), PJJob (Job {name = JobName "sf-ci-functional-minimal", parent = Nothing, nodeset = Nothing, branches = [], dependencies = [JobName "sf-rpm-build"]}), PJJob (Job {name = JobName "sf-ci-functional-allinone", parent = Nothing, nodeset = Nothing, branches = [], dependencies = [JobName "sf-rpm-build"]}), PJJob (Job {name = JobName "wait-for-changes-ahead", parent = Nothing, nodeset = Nothing, branches = [], dependencies = [JobName "sf-ci-functional-allinone", JobName "sf-ci-functional-minimal"]})]}]})]

      assertEqual "Expect data extracted from Project templates Config elements" (sort expected) (sort decoded)

    decodePipelineConfig = do
      json <- loadFixture "pipelines"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ ZPipeline (Pipeline {name = PipelineName "check", triggers = [PipelineTrigger {connectionName = ConnectionName "my_gerrit"}, PipelineTrigger {connectionName = ConnectionName "my_gitlab"}]})
            ]

      assertEqual "Expect data extracted from Pipeline Config elements" (sort expected) (sort decoded)

    decodeTenants = do
      json <- loadJSONFixture "system-config"
      let decoded = fromMaybe (error "oops") $ decodeTenantsConfig (ZKSystemConfig json)
          expected =
            [(TenantName "local",TenantConfig {defaultParent = JobName "base", connections = Data.Map.fromList [(ConnectionName "gerrit",TenantConnectionConfig {configProjects = [TenantProject {projectName = ProjectName "config", includedConfigElements = Data.Set.fromList [PipelineT,JobT,SemaphoreT,ProjectT,ProjectTemplateT,NodesetT,SecretT,QueueT], configPaths = [".zuul.yaml","zuul.yaml",".zuul.d/","zuul.d/"]}], untrustedProjects = [TenantProject {projectName = ProjectName "sf-jobs", includedConfigElements = Data.Set.fromList [JobT,SemaphoreT,ProjectT,ProjectTemplateT,NodesetT,SecretT,QueueT], configPaths = [".zuul.yaml","zuul.yaml",".zuul.d/","zuul.d/"]},TenantProject {projectName = ProjectName "zuul-jobs", includedConfigElements = Data.Set.fromList [JobT], configPaths = [".zuul.yaml","zuul.yaml",".zuul.d/","zuul.d/"]},TenantProject {projectName = ProjectName "zuul-distro-jobs", includedConfigElements = Data.Set.fromList [], configPaths = [".zuul.yaml","zuul.yaml",".zuul.d/","zuul.d/"]}]})]})]
      assertEqual "Expect data extracted from Pipeline Config elements" expected (Data.Map.toList decoded.tenants)

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
          expected = Just [(CanonicalProjectName (ProviderName "sftests.com",ProjectName "config"),[PipelineT,JobT,SemaphoreT,ProjectT,ProjectTemplateT,NodesetT,SecretT,QueueT]),(CanonicalProjectName (ProviderName "sftests.com",ProjectName "sf-jobs"),[JobT,SemaphoreT,ProjectT,ProjectTemplateT,NodesetT,SecretT,QueueT]),(CanonicalProjectName (ProviderName "sftests.com",ProjectName "zuul-jobs"),[JobT]),(CanonicalProjectName (ProviderName "sftests.com",ProjectName "zuul-distro-jobs"),[])]
      assertEqual "Expect tenant projects" expected tenantConfig
      assertEqual "Expect empty tenant projects" Nothing tenantConfigAlt
