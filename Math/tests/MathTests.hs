{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework
import {-@ HTF_TESTS @-} Math.StatisticsTest
import {-@ HTF_TESTS @-} Math.ExpressionTest
import {-@ HTF_TESTS @-} Math.FunctionTest


main :: IO()
main = htfMain htf_importedTests
