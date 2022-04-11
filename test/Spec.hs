module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Zuul.ZKDump (ZKConfig (..), mkZKConfig)

main :: IO ()
main = defaultMain (testGroup "Tests" [tests])

tests :: TestTree
tests =
  testGroup
    "ZKDump module"
    [ testCase "Extract data from ZK path" $ do
        let path = "/tmp/zk-dump/zuul/config/cache/sftests.com%2Fzuul-jobs/master/files/zuul.d%2Fhaskell-jobs.yaml/0000000000/ZKDATA"
         in assertEqual
              "ZK path is extracted to ZKConfig"
              (mkZKConfig path)
              ( Just
                  ( ZKConfig
                      { provider = "sftests.com",
                        project = "zuul-jobs",
                        branch = "master",
                        filePath = "zuul.d/haskell-jobs.yaml",
                        zkData = Nothing
                      }
                  )
              )
    ]
