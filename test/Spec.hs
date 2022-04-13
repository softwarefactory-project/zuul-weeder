module Main (main) where

import Data.Aeson (Value, object)
import qualified Data.Yaml as Y (decodeFileEither)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Zuul.ConfigLoader (Job (Job, jobName, parent), JobName (JobName), ZuulConfigElement (ZJob), decodeConfig)
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
              ZJob (Job {jobName = JobName "wait-for-changes-ahead", parent = Nothing})
            ]
      assertEqual "Expect data extracted from Config elements" expected decoded
