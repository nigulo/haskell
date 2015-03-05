{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework
--import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Ephem.TypesTest
import {-@ HTF_TESTS @-} Ephem.CoordsTest
import {-@ HTF_TESTS @-} Ephem.TimeTest
import {-@ HTF_TESTS @-} Ephem.UtilsTest
import {-@ HTF_TESTS @-} Ephem.SunTest
import {-@ HTF_TESTS @-} Ephem.CelestialBodyTest
import {-@ HTF_TESTS @-} Ephem.MoonTest

main = htfMain htf_importedTests