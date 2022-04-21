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
    Job (Job, branches, dependencies, jobName, nodeset, parent),
    JobName (JobName),
    JobNodeset (JobAnonymousNodeset, JobNodeset),
    NodeLabelName (NodeLabelName),
    Nodeset (Nodeset, nodesetLabels, nodesetName),
    NodesetName (NodesetName),
    Pipeline (Pipeline, pipelineJobs, pipelineName),
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
      testCase "Decode Zuul JSON config" decodeZuulJSONConfig
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
    decodeZuulJSONConfig = do
      json <- loadFixture "dataset1"
      let decoded = decodeConfig (CanonicalProjectName (ProviderName "", ProjectName ""), BranchName "") json
          expected =
            [ ZJob (Job {jobName = JobName "base", parent = Nothing, nodeset = Just (JobAnonymousNodeset [NodeLabelName "pod-centos-7"]), branches = [], dependencies = []}),
              ZJob (Job {jobName = JobName "config-check", parent = Just (JobName "base"), nodeset = Just (JobAnonymousNodeset []), branches = [BranchName "master"], dependencies = [JobName "job1", JobName "job2"]}),
              ZJob (Job {jobName = JobName "config-update", parent = Just (JobName "base"), nodeset = Just (JobAnonymousNodeset []), branches = [BranchName "master"], dependencies = []}),
              ZJob (Job {jobName = JobName "wait-for-changes-ahead", parent = Nothing, nodeset = Just (JobNodeset (NodesetName "nodeset1")), branches = [], dependencies = []}),
              ZProjectPipeline (ProjectPipeline {projectName = PName (ProjectName "sf-jobs"), pipelineTemplates = [], pipelinePipelines = [Pipeline {pipelineName = PipelineName "check", pipelineJobs = [JobName "linters"]}, Pipeline {pipelineName = PipelineName "gate", pipelineJobs = [JobName "linters"]}]}),
              ZProjectPipeline (ProjectPipeline {projectName = PName (ProjectName "zuul-jobs"), pipelineTemplates = [TemplateName "project-template"], pipelinePipelines = [Pipeline {pipelineName = PipelineName "check", pipelineJobs = [JobName "noop"]}, Pipeline {pipelineName = PipelineName "gate", pipelineJobs = [JobName "noop"]}]}),
              ZProjectPipeline (ProjectPipeline {projectName = PNameCannonical (CanonicalProjectName (ProviderName "", ProjectName "")), pipelineTemplates = [], pipelinePipelines = [Pipeline {pipelineName = PipelineName "check", pipelineJobs = [JobName "config-check"]}, Pipeline {pipelineName = PipelineName "gate", pipelineJobs = [JobName "config-check"]}, Pipeline {pipelineName = PipelineName "post", pipelineJobs = [JobName "config-update"]}]}),
              ZNodeset (Nodeset {nodesetName = NodesetName "nodeset1", nodesetLabels = [NodeLabelName "controller-label", NodeLabelName "compute-label", NodeLabelName "compute-label"]})
            ]
      assertEqual "Expect data extracted from Config elements" (sort expected) (sort decoded)
