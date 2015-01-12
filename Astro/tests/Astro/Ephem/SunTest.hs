{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Astro.Ephem.SunTest where

import Astro.Ephem.Types
import Astro.Ephem.Time
import Astro.Ephem.Coords
import Astro.Ephem.OrbitalElements
import Astro.Ephem.Sun
import Astro.Ephem.TestUtils
import Test.Framework
import Debug.Trace

test_calcSunLongitude = do
    let
        date = ymd 1978 7 27
        (sunLong, _) = calcSun earth1980 date
        tilt = calcObliquityOfEcliptic date
        (sunRA, sunDec) = eclToEqu sunLong (Deg 0) tilt
    assertEqualAngle (DMS 123 37 47.70698608487464) sunLong
    assertEqualHours (HMS 8 23 46.01736145217259) sunRA
    assertEqualAngle (DMS 19 20 38.43024784019206) sunDec

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

test_calcSunRiseSet' = do
    let
        date = ymd 1979 9 7
        --(sunLong, _) = calcSun earth1980 date
        --sunLong = Deg 163.778867 
        tilt = calcObliquityOfEcliptic date
        lat = Lat (Deg 52) N
        long = Long (Deg 0) W
        maybeRiseSet = calcSunRiseSet' date earth1980 lat True
    --assertEqualAngle (Deg 163.778867) sunLong
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

test_calcSunRiseSet'2 = do
    let
        date = ymd 2013 7 24
        --(sunLong, _) = calcSun earth2000 date
        --tilt = calcObliquityOfEcliptic date
        lat = Lat (DMS 58 22 47) N
        long = Long (DMS 26 43 18) E
        maybeRiseSet = calcSunRiseSet' date earth2000 lat True
    case maybeRiseSet of
        Nothing -> 
            assertFailure "calcSunRiseSet failed"
        Just (lstRise, lstSet) -> do
            assertEqualHours (Hrs 23.92575457277) lstRise
            assertEqualHours (Hrs 16.83551150825) lstSet
            
            let
                gstRise = lstToGST lstRise long
                gstSet = lstToGST lstSet long
                gmtRise = gstToGMT gstRise date
                gmtSet = gstToGMT gstSet date
            assertEqualHours (HMS 2 0 48.768656) gmtRise
            assertEqualHours (HMS 18 52 37.704532) gmtSet

test_calcSunRiseSet'3 = do
    let
        date = ymd 2013 3 19
        --(sunLong, _) = calcSun earth2000 date
        --tilt = calcObliquityOfEcliptic date
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

