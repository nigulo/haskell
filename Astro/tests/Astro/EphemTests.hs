{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework
--import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Astro.Ephem.TypesTest
import {-@ HTF_TESTS @-} Astro.Ephem.CoordsTest
import {-@ HTF_TESTS @-} Astro.Ephem.TimeTest
import {-@ HTF_TESTS @-} Astro.Ephem.UtilsTest
import {-@ HTF_TESTS @-} Astro.Ephem.SunTest
import {-@ HTF_TESTS @-} Astro.Ephem.CelestialBodyTest
import {-@ HTF_TESTS @-} Astro.Ephem.MoonTest

main = htfMain htf_importedTests