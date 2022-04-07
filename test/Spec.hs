module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Tests" [tests])

tests :: TestTree
tests =
  testGroup
    "FromJSON"
    [ testCase "Decode dump" $ assertBool "Dump is decoded" True
    ]
