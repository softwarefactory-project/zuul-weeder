module Main (main) where

import Data.Aeson (Value, object)
import Data.List (sort)
import qualified Data.Yaml as Y (decodeFileEither)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Zuul.ConfigLoader
  ( Job (Job, jobName, parent),
    JobName (JobName),
    PipelineName (PipelineName),
    ProjectName (ProjectName),
    ProjectPipeline (ProjectPipeline, pPipelineName, pipelineJobs, projectName),
    ZuulConfigElement (ZJob, ZProjectPipeline),
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
      let decoded = decodeConfig json
          expected =
            [ ZJob (Job {jobName = JobName "base", parent = Nothing}),
              ZJob (Job {jobName = JobName "config-check", parent = Just (JobName "base")}),
              ZJob (Job {jobName = JobName "config-update", parent = Just (JobName "base")}),
              ZProjectPipeline (ProjectPipeline {projectName = ProjectName "config", pPipelineName = PipelineName "post", pipelineJobs = [JobName "config-update"]}),
              ZProjectPipeline (ProjectPipeline {projectName = ProjectName "config", pPipelineName = PipelineName "check", pipelineJobs = [JobName "config-check"]}),
              ZProjectPipeline (ProjectPipeline {projectName = ProjectName "config", pPipelineName = PipelineName "gate", pipelineJobs = [JobName "config-check"]}),
              ZProjectPipeline (ProjectPipeline {projectName = ProjectName "zuul-jobs", pPipelineName = PipelineName "check", pipelineJobs = [JobName "noop"]}),
              ZProjectPipeline (ProjectPipeline {projectName = ProjectName "zuul-jobs", pPipelineName = PipelineName "gate", pipelineJobs = [JobName "noop"]}),
              ZProjectPipeline (ProjectPipeline {projectName = ProjectName "sf-jobs", pPipelineName = PipelineName "check", pipelineJobs = [JobName "linters"]}),
              ZProjectPipeline (ProjectPipeline {projectName = ProjectName "sf-jobs", pPipelineName = PipelineName "gate", pipelineJobs = [JobName "linters"]}),
              ZJob (Job {jobName = JobName "wait-for-changes-ahead", parent = Nothing})
            ]

      assertEqual "Expect data extracted from Config elements" (sort expected) (sort decoded)
