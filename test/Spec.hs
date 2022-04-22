module Main (main) where

import Data.Aeson (Value, object)
import Data.List (sort)
import Data.Set (fromList)
import qualified Data.Yaml as Y (decodeFileEither)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Zuul.ConfigLoader
  ( BranchName (BranchName),
    CanonicalProjectName (CanonicalProjectName),
    ConnectionName (ConnectionName),
    Job (Job, jobBranches, jobDependencies, jobName, jobNodeset, jobParent),
    JobName (JobName),
    JobNodeset (JobAnonymousNodeset, JobNodeset),
    NodeLabelName (NodeLabelName),
    Nodeset (Nodeset, nodesetLabels, nodesetName),
    NodesetName (NodesetName),
    PPipeline (PPipeline, pPipelineJobs, pPipelineName),
    Pipeline (Pipeline, pipelineName, pipelineTriggers),
    PipelineJob (PJJob, PJName),
    PipelineName (PipelineName),
    PipelineTrigger (PipelineTrigger, connectionName),
    Project (PName, PNameCannonical, TName),
    ProjectName (ProjectName),
    ProjectPipeline (ProjectPipeline, pName, pipelinePipelines, pipelineTemplates),
    ProviderName (ProviderName),
    TemplateName (TemplateName),
    ZuulConfigElement (ZJob, ZNodeset, ZPipeline, ZProjectPipeline, ZProjectTemplate),
    decodeConfig,
  )
import Zuul.ZKDump (ZKConfig (..), mkZKConfig)

main :: IO ()
main = defaultMain (testGroup "Tests" [tests])

loadFixture :: FilePath -> IO Value
loadFixture name = do
  contentE <- Y.decodeFileEither $ fixturesPath </> name <> ".yaml"
  case contentE of
    Left _any -> error $ "Unable to decode fixture " <> name
    Right bs -> pure bs
  where
    fixturesPath = "test/fixtures"

tests :: TestTree
tests =
  testGroup
    "ZKDump module"
    [ testCase "Extract data from ZK path" extractDataZKPath,
      testCase "Decode Jobs config" decodeJobsConfig,
      testCase "Decode Projects config" decodeProjectsConfig,
      testCase "Decode Nodesets config" decodeNodesetsConfig,
      testCase "Decode Project templates config" decodeProjectTemplatesConfig,
      testCase "Decode Pipeline config" decodePipelineConfig
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
            [ ZJob (Job {jobName = JobName "base", jobParent = Nothing, jobNodeset = Just (JobAnonymousNodeset [NodeLabelName "pod-centos-7"]), jobBranches = [], jobDependencies = []}),
              ZJob (Job {jobName = JobName "config-check", jobParent = Just (JobName "base"), jobNodeset = Just (JobAnonymousNodeset []), jobBranches = [BranchName "master"], jobDependencies = [JobName "job1", JobName "job2"]}),
              ZJob (Job {jobName = JobName "config-update", jobParent = Just (JobName "base"), jobNodeset = Just (JobAnonymousNodeset []), jobBranches = [BranchName "master"], jobDependencies = []}),
              ZJob (Job {jobName = JobName "wait-for-changes-ahead", jobParent = Nothing, jobNodeset = Just (JobNodeset (NodesetName "nodeset1")), jobBranches = [], jobDependencies = []})
            ]

      assertEqual "Expect data extracted from Jobs Config elements" (sort expected) (sort decoded)

    decodeProjectsConfig = do
      json <- loadFixture "projects"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ ZProjectPipeline (ProjectPipeline {pName = PName (ProjectName "config"), pipelineTemplates = [], pipelinePipelines = fromList [PPipeline {pPipelineName = PipelineName "check", pPipelineJobs = [PJName (JobName "config-check")]}, PPipeline {pPipelineName = PipelineName "gate", pPipelineJobs = [PJName (JobName "config-check")]}, PPipeline {pPipelineName = PipelineName "post", pPipelineJobs = [PJName (JobName "config-update")]}]}),
              ZProjectPipeline (ProjectPipeline {pName = PNameCannonical (CanonicalProjectName (ProviderName "", ProjectName "")), pipelineTemplates = [TemplateName "sf-ci-jobs"], pipelinePipelines = fromList [PPipeline {pPipelineName = PipelineName "check", pPipelineJobs = [PJJob (Job {jobName = JobName "linters", jobParent = Nothing, jobNodeset = Just (JobNodeset (NodesetName "linters-pod")), jobBranches = [], jobDependencies = []})]}, PPipeline {pPipelineName = PipelineName "experimental", pPipelineJobs = [PJName (JobName "sf-rpm-build"), PJJob (Job {jobName = JobName "sf-ci-openshift-integration", jobParent = Nothing, jobNodeset = Nothing, jobBranches = [], jobDependencies = [JobName "sf-rpm-build"]})]}, PPipeline {pPipelineName = PipelineName "gate", pPipelineJobs = [PJJob (Job {jobName = JobName "linters", jobParent = Nothing, jobNodeset = Just (JobNodeset (NodesetName "linters-pod")), jobBranches = [], jobDependencies = []})]}]})
            ]

      assertEqual "Expect data extracted from Projects Config elements" (sort expected) (sort decoded)

    decodeNodesetsConfig = do
      json <- loadFixture "nodesets"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ ZNodeset (Nodeset {nodesetName = NodesetName "nodeset1", nodesetLabels = [NodeLabelName "compute-label", NodeLabelName "compute-label", NodeLabelName "controller-label"]})
            ]

      assertEqual "Expect data extracted from Nodesets Config elements" (sort expected) (sort decoded)

    decodeProjectTemplatesConfig = do
      json <- loadFixture "project-templates"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ZProjectTemplate (ProjectPipeline {pName = TName (TemplateName "sf-ci-jobs"), pipelineTemplates = [], pipelinePipelines = fromList [PPipeline {pPipelineName = PipelineName "check", pPipelineJobs = [PJName (JobName "sf-rpm-build"), PJJob (Job {jobName = JobName "sf-ci-functional-minimal", jobParent = Nothing, jobNodeset = Nothing, jobBranches = [], jobDependencies = [JobName "sf-rpm-build"]}), PJJob (Job {jobName = JobName "sf-ci-functional-allinone", jobParent = Nothing, jobNodeset = Nothing, jobBranches = [], jobDependencies = [JobName "sf-rpm-build"]}), PJJob (Job {jobName = JobName "sf-tenants", jobParent = Nothing, jobNodeset = Nothing, jobBranches = [], jobDependencies = [JobName "sf-rpm-build"]})]}, PPipeline {pPipelineName = PipelineName "gate", pPipelineJobs = [PJName (JobName "sf-rpm-build"), PJJob (Job {jobName = JobName "sf-ci-functional-minimal", jobParent = Nothing, jobNodeset = Nothing, jobBranches = [], jobDependencies = [JobName "sf-rpm-build"]}), PJJob (Job {jobName = JobName "sf-ci-functional-allinone", jobParent = Nothing, jobNodeset = Nothing, jobBranches = [], jobDependencies = [JobName "sf-rpm-build"]}), PJJob (Job {jobName = JobName "wait-for-changes-ahead", jobParent = Nothing, jobNodeset = Nothing, jobBranches = [], jobDependencies = [JobName "sf-ci-functional-allinone", JobName "sf-ci-functional-minimal"]})]}]})]

      assertEqual "Expect data extracted from Project templates Config elements" (sort expected) (sort decoded)

    decodePipelineConfig = do
      json <- loadFixture "pipelines"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ ZPipeline (Pipeline {pipelineName = PipelineName "check", pipelineTriggers = [PipelineTrigger {connectionName = ConnectionName "my_gerrit"}, PipelineTrigger {connectionName = ConnectionName "my_gitlab"}]})
            ]

      assertEqual "Expect data extracted from Pipeline Config elements" (sort expected) (sort decoded)
