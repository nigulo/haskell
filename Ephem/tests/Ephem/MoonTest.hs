module Ephem.MoonTest (tests) where

import Ephem.OrbitalElements
import Ephem.CelestialBody
import Ephem.Sun
import Ephem.Moon
import Ephem.Types
import Ephem.Coords
import Ephem.Time
import Ephem.TestUtils
import Test.Tasty
import Test.Tasty.HUnit
import Debug.Trace

test_calcMoon1980 :: Assertion
test_calcMoon1980 = do
    let
        date = ymd 1979 2 26.6666667
        sun@(sunLong, _) = calcSun earth1980 date
        (moonLong, moonLat, _, l'') = calcMoon moon1980 sun date
        tilt = calcObliquityOfEcliptic date
        (moonRA, moonDec) = eclToEqu moonLong moonLat tilt
        
        phase = calcMoonPhase sunLong l''

    assertEqualAngle (Deg 336.2712914536) moonLong
    assertEqualAngle (Deg 1.0571632870999998) moonLat
    assertEqualHours (HMS 22 30 34.16718007662) moonRA
    assertEqualAngle (DMS (-8) 13 44.675993286259995) moonDec
    
    assertEqualDouble 1.1746293999999999e-4 phase

test_calcMoon1980_1 :: Assertion
test_calcMoon1980_1 = do
    let
        date = ymd 1990 4 19
        sun@(sunLong, _) = calcSun earth1980 date
        (moonLong, moonLat, _, l'') = calcMoon moon1980 sun date

    assertEqualAngle (Deg 308.76751518332) moonLong
    assertEqualAngle (Deg (-0.34329209165999997)) moonLat

{-
test_calcMoon1980_2 = do
    let
        date = ymd 1990 4 19
        sun@(sunLong, _) = calcSun earth1980 date
        (moonLong, moonLat, _) = calcMoon2 moon1980 sun date

    assertEqualAngle (Deg 309.292597545) moonLong
    assertEqualAngle (Deg (-0.37284184465)) moonLat

test_calcMoon1980_3 = do
    let
        date = ymd 1979 2 26.6666667
        sun@(sunLong, _) = calcSun earth1980 date
        (moonLong, moonLat, _) = calcMoon2 moon1980 sun date

    assertEqualAngle (Deg 336.2712914536) moonLong
    assertEqualAngle (Deg 1.0571632870999998) moonLat
-}

test_calcMoon2000 :: Assertion
test_calcMoon2000 = do
    let
        date = ymd 1990 4 19
        sun@(sunLong, _) = calcSun earth2000 date
        (moonLong, moonLat, dist) = calcMoon2 moon2000 sun date
        EarthRadii moonDist = toEarthRadii dist

    assertEqualAngle (Deg 306.91840665099) moonLong
    assertEqualAngle (Deg (-0.57782468703)) moonLat
    assertEqualDouble 60.72590740718999 moonDist

test_calcMoon2000_1 :: Assertion
test_calcMoon2000_1 = do
    let
        date = ymd 1979 2 26.6666667
        sun@(sunLong, _) = calcSun earth2000 date
        (moonLong, moonLat, _) = calcMoon2 moon2000 sun date

    assertEqualAngle (Deg 336.85740495579) moonLong
    assertEqualAngle (Deg 0.9542138930299999) moonLat

{-
test_calcMoon2000_2 = do
    let
        date = ymd 1979 2 26.6666667
        sun@(sunLong, _) = calcSun earth2000 date
        (moonLong, moonLat, _, _) = calcMoon moon2000 sun date

    assertEqualAngle (Deg 336.7528637305) moonLong
    assertEqualAngle (Deg 0.9694900409599999) moonLat

test_calcMoon2000_3 = do
    let
        date = ymd 1990 4 19
        sun@(sunLong, _) = calcSun earth2000 date
        (moonLong, moonLat, _, l'') = calcMoon moon2000 sun date

    assertEqualAngle (Deg 308.76751518332) moonLong
    assertEqualAngle (Deg (-0.34329209165999997)) moonLat
-}

test_calcMoon2013 :: Assertion
test_calcMoon2013 = do
    let
        date = ymd 2013 7 29
        sun@(sunLong, _) = calcSun earth2000 date
        (moonLong, moonLat, _) = calcMoon2 moon2000 sun date
        tilt = calcObliquityOfEcliptic date
        (moonRA, moonDec) = eclToEqu moonLong moonLat tilt

    assertEqualAngle (Deg 27.432735591599997) moonLong
    assertEqualAngle (Deg 1.32429474496) moonLat
    assertEqualHours (HMS 1 39 55.273399) moonRA
    assertEqualAngle (DMS 11 47 40.008463) moonDec


test_calcPositionAngle :: Assertion
test_calcPositionAngle = do
    let
        sunRA = HMS 3 40 38
        sunDec = DMS 19 35 16
        moonRA = HMS 21 56 32
        moonDec = DMS (-10) 57 08
        posAngle = calcPositionAngle (sunRA, sunDec) (moonRA, moonDec)
    assertEqualAngle (Deg 70.02850835221) posAngle

test_calcMoonDistance :: Assertion
test_calcMoonDistance = do
    let
        date = ymd 1979 2 26.6666667
        mM' = Deg (-359.735278)
        dist = calcMoonDistance moon1980 mM' date
    assertEqualDouble 0.9451006465399999 dist
    
test_calcMoonAngularDiameter :: Assertion
test_calcMoonAngularDiameter = do
    let
        dist = 0.945101
        diam = calcMoonAngularDiameter dist
    assertEqualAngle (DMS 0 32 53.503361) diam
    
    
test_calcMoonHorizontalParallax :: Assertion
test_calcMoonHorizontalParallax = do
    let
        dist = 0.945101
        parallax = calcMoonHorizontalParallax dist
    assertEqualAngle (DMS 1 00 21.327244) parallax

test_calcMoonRiseSet :: Assertion
test_calcMoonRiseSet = do
    let
        date = YMD 1979 9 6
        long = Long (Deg 0) E
        lat = Lat (Deg 52) N
        maybeRiseSet = calcMoonRiseSet date moon1980 earth1980 lat long 0 1
    case maybeRiseSet of
        Nothing -> 
            assertFailure "calcSunMoonSet failed"
        Just (lstRise, lstSet) -> do
            assertEqualHours (Hrs 17.659648879829998) lstRise
            assertEqualHours (Hrs 4.01063710782) lstSet
            let
                gstRise = lstToGST lstRise long
                gstSet = lstToGST lstSet long
                gmtRise = gstToGMT gstRise date
                gmtSet = gstToGMT gstSet date
            assertEqualHours (HMS 18 38 32.653841) $ getHours gmtRise
            assertEqualHours (HMS 5 1 50.374631) $ getHours gmtSet

tests :: TestTree
tests = testGroup "Ephem.MoonTest"
    [ testCase "calcMoon1980" test_calcMoon1980
    , testCase "calcMoon1980_1" test_calcMoon1980_1
    , testCase "calcMoon2000" test_calcMoon2000
    , testCase "calcMoon2000_1" test_calcMoon2000_1
    , testCase "calcMoon2013" test_calcMoon2013
    , testCase "calcPositionAngle" test_calcPositionAngle
    , testCase "calcMoonDistance" test_calcMoonDistance
    , testCase "calcMoonAngularDiameter" test_calcMoonAngularDiameter
    , testCase "calcMoonHorizontalParallax" test_calcMoonHorizontalParallax
    , testCase "calcMoonRiseSet" test_calcMoonRiseSet
    ]
