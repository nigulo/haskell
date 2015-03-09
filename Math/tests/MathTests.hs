{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework
import {-@ HTF_TESTS @-} Math.ExpressionTest
import {-@ HTF_TESTS @-} Math.FunctionTest
import {-@ HTF_TESTS @-} Math.LinearEquationsTest
import {-@ HTF_TESTS @-} Math.IOLinearEquationsTest
import {-@ HTF_TESTS @-} Math.IODoubleLinearEquationsTest

main :: IO()
main = htfMain htf_importedTests
