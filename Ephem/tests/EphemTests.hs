module Main where

import Test.Tasty

import qualified Ephem.TypesTest
import qualified Ephem.CoordsTest
import qualified Ephem.TimeTest
import qualified Ephem.UtilsTest
import qualified Ephem.SunTest
import qualified Ephem.CelestialBodyTest
import qualified Ephem.MoonTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Ephem Tests"
    [ Ephem.TypesTest.tests
    , Ephem.CoordsTest.tests
    , Ephem.TimeTest.tests
    , Ephem.UtilsTest.tests
    , Ephem.SunTest.tests
    , Ephem.CelestialBodyTest.tests
    , Ephem.MoonTest.tests
    ]
