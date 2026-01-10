module Ephem.SunTest (tests) where

import Ephem.Types
import Ephem.Time
import Ephem.Coords
import Ephem.OrbitalElements
import Ephem.Sun
import Ephem.TestUtils
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Sun Tests"
    [ testCase "calcSunLongitude" test_calcSunLongitude
    , testCase "calcSunRiseSet" test_calcSunRiseSet
    , testCase "calcSunRiseSet'" test_calcSunRiseSet'
    , testCase "calcSunRiseSet'2" test_calcSunRiseSet'2
    , testCase "calcSunRiseSet'3" test_calcSunRiseSet'3
    ]

test_calcSunLongitude :: Assertion
test_calcSunLongitude = do
    let
        date = ymd 1978 7 27
        (sunLong, _) = calcSun earth1980 date
        tilt = calcObliquityOfEcliptic date
        (sunRA, sunDec) = eclToEqu sunLong (Deg 0) tilt
    assertEqualAngle (DMS 123 37 47.70698608487464) sunLong
    assertEqualHours (HMS 8 23 46.01736145217259) sunRA
    assertEqualAngle (DMS 19 20 38.43024784019206) sunDec

test_calcSunRiseSet :: Assertion
test_calcSunRiseSet = do
    let
        date = ymd 1979 9 7
        lat = Lat (Deg 52) N
        long = Long (Deg 0) W
        maybeRiseSet = calcSunRiseSet date earth1980 lat long
    case maybeRiseSet of
        Nothing ->
            assertFailure "calcSunRiseSet failed"
        Just ((gmtRise, aziRise), (gmtSet, aziSet)) -> do
            assertEqualHours (HMS 5 20 20.941854) gmtRise
            assertEqualHours (HMS 18 34 58.819801999999996) gmtSet

test_calcSunRiseSet' :: Assertion
test_calcSunRiseSet' = do
    let
        date = ymd 1979 9 7
        tilt = calcObliquityOfEcliptic date
        lat = Lat (Deg 52) N
        long = Long (Deg 0) W
        maybeRiseSet = calcSunRiseSet' date earth1980 lat True
    case maybeRiseSet of
        Nothing ->
            assertFailure "calcSunRiseSet failed"
        Just (lstRise, lstSet) -> do
            assertEqualHours (Hrs 4.381976917099999) lstRise
            assertEqualHours (Hrs 17.66469518556) lstSet

            let
                gstRise = lstToGST lstRise long
                gstSet = lstToGST lstSet long
                gmtRise = gstToGMT gstRise date
                gmtSet = gstToGMT gstSet date
            assertEqualHours (HMS 5 20 7.727189999999999) gmtRise
            assertEqualHours (HMS 18 34 54.970400999999995) gmtSet

test_calcSunRiseSet'2 :: Assertion
test_calcSunRiseSet'2 = do
    let
        date = ymd 2013 7 24
        lat = Lat (DMS 58 22 47) N
        long = Long (DMS 26 43 18) E
        maybeRiseSet = calcSunRiseSet' date earth2000a lat True
    case maybeRiseSet of
        Nothing ->
            assertFailure "calcSunRiseSet failed"
        Just (lstRise, lstSet) -> do
            assertEqualHours (Hrs 23.776147349899997) lstRise
            assertEqualHours (Hrs 16.78680324471) lstSet

            let
                gstRise = lstToGST lstRise long
                gstSet = lstToGST lstSet long
                gmtRise = gstToGMT gstRise date
                gmtSet = gstToGMT gstSet date
            assertEqualHours (HMS 1 51 51.652992999999995) gmtRise
            assertEqualHours (HMS 18 49 42.833487999999996) gmtSet

test_calcSunRiseSet'3 :: Assertion
test_calcSunRiseSet'3 = do
    let
        date = ymd 2013 3 19
        lat = Lat (DMS 58 22 47) N
        long = Long (DMS 26 43 18) E
        maybeRiseSet = calcSunRiseSet' date earth2000 lat True
    case maybeRiseSet of
        Nothing ->
            assertFailure "calcSunRiseSet failed"
        Just (lstRise, lstSet) -> do
            assertEqualHours (Hrs 17.90462887806) lstRise
            assertEqualHours (Hrs 6.1173861898799995) lstSet

            let
                gstRise = lstToGST lstRise long
                gstSet = lstToGST lstSet long
                gmtRise = gstToGMT gstRise date
                gmtSet = gstToGMT gstSet date
            assertEqualHours (HMS 4 19 52.396257) gmtRise
            assertEqualHours (HMS 16 30 38.2956) gmtSet
