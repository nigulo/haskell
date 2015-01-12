{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Regression.RegressionTest
import {-@ HTF_TESTS @-} Regression.PolynomTest
import {-@ HTF_TESTS @-} Regression.AnalyticDataTest
import {-@ HTF_TESTS @-} Regression.LeastSquaresTest

main = htfMain htf_importedTests