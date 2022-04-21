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
    Job (Job, branches, jobName, nodeset, parent),
    JobName (JobName),
    JobNodeset (JobAnonymousNodeset, JobNodeset),
    NodeLabelName (NodeLabelName),
    Nodeset (Nodeset, nodesetLabels, nodesetName),
    NodesetName (NodesetName),
    PipelineName (PipelineName),
    Project (PName, PNameCannonical),
    ProjectName (ProjectName),
    ProjectPipeline (ProjectPipeline, pPipelineName, pipelineJobs, pipelineTemplates, projectName),
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
            [ ZJob (Job {jobName = JobName "base", parent = Nothing, nodeset = Just (JobAnonymousNodeset [NodeLabelName "pod-centos-7"]), branches = []}),
              ZJob (Job {jobName = JobName "config-check", parent = Just (JobName "base"), nodeset = Just (JobAnonymousNodeset []), branches = [BranchName "master"]}),
              ZJob (Job {jobName = JobName "config-update", parent = Just (JobName "base"), nodeset = Just (JobAnonymousNodeset []), branches = [BranchName "master"]}),
              ZJob (Job {jobName = JobName "wait-for-changes-ahead", parent = Nothing, nodeset = Just (JobNodeset (NodesetName "nodeset1")), branches = []}),
              ZProjectPipeline (ProjectPipeline {projectName = PName (ProjectName "sf-jobs"), pPipelineName = PipelineName "check", pipelineTemplates = [], pipelineJobs = [JobName "linters"]}),
              ZProjectPipeline (ProjectPipeline {projectName = PName (ProjectName "sf-jobs"), pPipelineName = PipelineName "gate", pipelineTemplates = [], pipelineJobs = [JobName "linters"]}),
              ZProjectPipeline (ProjectPipeline {projectName = PName (ProjectName "zuul-jobs"), pPipelineName = PipelineName "check", pipelineTemplates = [TemplateName "project-template"], pipelineJobs = [JobName "noop"]}),
              ZProjectPipeline (ProjectPipeline {projectName = PName (ProjectName "zuul-jobs"), pPipelineName = PipelineName "gate", pipelineTemplates = [TemplateName "project-template"], pipelineJobs = [JobName "noop"]}),
              ZProjectPipeline (ProjectPipeline {projectName = PNameCannonical (CanonicalProjectName (ProviderName "", ProjectName "")), pPipelineName = PipelineName "check", pipelineTemplates = [], pipelineJobs = [JobName "config-check"]}),
              ZProjectPipeline (ProjectPipeline {projectName = PNameCannonical (CanonicalProjectName (ProviderName "", ProjectName "")), pPipelineName = PipelineName "gate", pipelineTemplates = [], pipelineJobs = [JobName "config-check"]}),
              ZProjectPipeline (ProjectPipeline {projectName = PNameCannonical (CanonicalProjectName (ProviderName "", ProjectName "")), pPipelineName = PipelineName "post", pipelineTemplates = [], pipelineJobs = [JobName "config-update"]}),
              ZNodeset (Nodeset {nodesetName = NodesetName "nodeset1", nodesetLabels = [NodeLabelName "controller-label", NodeLabelName "compute-label", NodeLabelName "compute-label"]})
            ]
      assertEqual "Expect data extracted from Config elements" (sort expected) (sort decoded)
