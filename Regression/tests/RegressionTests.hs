module Main where

import Test.Tasty

import qualified Regression.RegressionTest
import qualified Regression.PolynomTest
import qualified Regression.AnalyticDataTest
import qualified Regression.LeastSquaresTest
import qualified Regression.DataTest
import qualified Regression.BayesTest
import qualified Regression.RBFTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Regression Tests"
    [ Regression.RegressionTest.tests
    , Regression.PolynomTest.tests
    , Regression.AnalyticDataTest.tests
    , Regression.LeastSquaresTest.tests
    , Regression.DataTest.tests
    , Regression.BayesTest.tests
    , Regression.RBFTest.tests
    ]
