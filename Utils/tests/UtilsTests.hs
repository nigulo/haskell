{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework
import {-@ HTF_TESTS @-} Utils.MiscTest
import {-@ HTF_TESTS @-} Utils.ConcurrentTest

main :: IO()
main = htfMain htf_importedTests
