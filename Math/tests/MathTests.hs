module Main where

import Test.Tasty

import qualified Math.ExpressionTest
import qualified Math.FunctionTest
import qualified Math.LinearEquationsTest
import qualified Math.IOLinearEquationsTest
import qualified Math.IODoubleLinearEquationsTest
import qualified Math.IODoubleVectorTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Math Tests"
    [ Math.ExpressionTest.tests
    , Math.FunctionTest.tests
    , Math.LinearEquationsTest.tests
    , Math.IOLinearEquationsTest.tests
    , Math.IODoubleLinearEquationsTest.tests
    , Math.IODoubleVectorTest.tests
    ]
