module Main (main) where

import Data.Aeson (Value, object)
import Data.List (sort)
import qualified Data.Yaml as Y (decodeFileEither)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Zuul.ConfigLoader
  ( BranchName (BranchName),
    CanonicalProjectName (CanonicalProjectName),
    Job (Job, jobBranches, jobDependencies, jobName, jobNodeset, jobParent),
    JobName (JobName),
    JobNodeset (JobAnonymousNodeset, JobNodeset),
    NodeLabelName (NodeLabelName),
    Nodeset (Nodeset, nodesetLabels, nodesetName),
    NodesetName (NodesetName),
    Pipeline (Pipeline, pipelineJobs, pipelineName),
    PipelineJob (PJJob, PJName),
    PipelineName (PipelineName),
    Project (PName, PNameCannonical),
    ProjectName (ProjectName),
    ProjectPipeline (ProjectPipeline, pipelinePipelines, pipelineTemplates, projectName),
    ProviderName (ProviderName),
    TemplateName (TemplateName),
    ZuulConfigElement (ZJob, ZNodeset, ZProjectPipeline),
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
      testCase "Decode Nodesets config" decodeNodesetsConfig
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
            [ ZProjectPipeline (ProjectPipeline {projectName = PName (ProjectName "config"), pipelineTemplates = [], pipelinePipelines = [Pipeline {pipelineName = PipelineName "check", pipelineJobs = [PJName (JobName "config-check")]}, Pipeline {pipelineName = PipelineName "gate", pipelineJobs = [PJName (JobName "config-check")]}, Pipeline {pipelineName = PipelineName "post", pipelineJobs = [PJName (JobName "config-update")]}]}),
              ZProjectPipeline (ProjectPipeline {projectName = PNameCannonical (CanonicalProjectName (ProviderName "", ProjectName "")), pipelineTemplates = [TemplateName "sf-ci-jobs"], pipelinePipelines = [Pipeline {pipelineName = PipelineName "check", pipelineJobs = [PJJob (Job {jobName = JobName "linters", jobParent = Nothing, jobNodeset = Just (JobNodeset (NodesetName "linters-pod")), jobBranches = [], jobDependencies = []})]}, Pipeline {pipelineName = PipelineName "experimental", pipelineJobs = [PJJob (Job {jobName = JobName "sf-ci-openshift-integration", jobParent = Nothing, jobNodeset = Nothing, jobBranches = [], jobDependencies = [JobName "sf-rpm-build"]}), PJName (JobName "sf-rpm-build")]}, Pipeline {pipelineName = PipelineName "gate", pipelineJobs = [PJJob (Job {jobName = JobName "linters", jobParent = Nothing, jobNodeset = Just (JobNodeset (NodesetName "linters-pod")), jobBranches = [], jobDependencies = []})]}]})
            ]

      assertEqual "Expect data extracted from Projects Config elements" (sort expected) (sort decoded)

    decodeNodesetsConfig = do
      json <- loadFixture "nodesets"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ ZNodeset (Nodeset {nodesetName = NodesetName "nodeset1", nodesetLabels = [NodeLabelName "controller-label", NodeLabelName "compute-label", NodeLabelName "compute-label"]})
            ]

      assertEqual "Expect data extracted from Nodesets Config elements" (sort expected) (sort decoded)
