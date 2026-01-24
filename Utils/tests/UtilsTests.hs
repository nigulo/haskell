module Main where

import Test.Tasty

import qualified Utils.MiscTest
import qualified Utils.ConcurrentTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Utils Tests"
    [ Utils.MiscTest.tests
    , Utils.ConcurrentTest.tests
    ]
