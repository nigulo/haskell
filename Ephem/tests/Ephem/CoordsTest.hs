module Ephem.CoordsTest (tests) where

import Ephem.Types
import Ephem.Time
import Ephem.Coords
import Ephem.TestUtils
import Test.Tasty
import Test.Tasty.HUnit
import Debug.Trace

test_raToLHA :: Assertion
test_raToLHA = do
    let
        lst = Hrs 0.401436
        ra = HMS 18 32 21
    assertEqualHours (HMS 5 51 44.169600000001594) (raToLHA ra lst) 

test_lhaToRA :: Assertion
test_lhaToRA = do
    let
        lst = Hrs 0.401436
        lha = HMS 5 51 44
    assertEqualHours (HMS 18 32 21.16960000000205) (lhaToRA lha lst) 


test_equToHor :: Assertion
test_equToHor = do
    let
        lha = HMS 5 51 44
        dec = DMS 23 13 10
        lat = Lat (Deg 52) N
        (alt, azi) = equToHor lha dec lat
    assertEqualAngle (DMS 19 20 3.642807769706735) alt
    assertEqualAngle (DMS 283 16 15.69816218970118) azi

test_horToEqu :: Assertion
test_horToEqu = do
    let
        alt = DMS 19 20 3.642807769706735
        azi = DMS 283 16 15.69816218970118
        lat = Lat (Deg 52) N
        (lha, dec) = horToEqu alt azi lat
    assertEqualHours (HMS 5 51 44.00000000000858) lha
    assertEqualAngle (DMS 23 13 10.000000000129887) dec

test_calcObliquityOfEcliptic :: Assertion
test_calcObliquityOfEcliptic = do
    assertEqualAngle (DMS 23 26 30.814884088487698) (calcObliquityOfEcliptic (toValidYMD (YMD 1980 1 0.0)))  

test_eclToEqu :: Assertion
test_eclToEqu = do
    let
        long = DMS 139 41 10
        lat = DMS 4 52 31
        tilt = Deg 23.441884
        (ra, dec) = eclToEqu long lat tilt
    assertEqualHours (HMS 9 34 53.58428229916811) ra
    assertEqualAngle (DMS 19 32 14.16675910415279) dec

test_equToEcl :: Assertion
test_equToEcl = do
    let
        ra = HMS 9 34 53.58428
        dec = DMS 19 32 14.16676
        tilt = Deg 23.441884
        (long, lat) = equToEcl ra dec tilt
    assertEqualAngle (DMS 139 41 9.99996882669848) long
    assertEqualAngle (DMS 4 52 30.9999903866948) lat

test_calcRiseSet :: Assertion
test_calcRiseSet = do
    let
        date = YMD 1980 8 24
        ra = HMS 23 39 20
        dec = DMS 21 42 0
        long = Long (Deg 64) E
        lat = Lat (Deg 30) N
        maybeRiseSet = calcRiseSet ra dec lat False
    case maybeRiseSet of
        Nothing -> 
            assertFailure "calcRiseSet failed"
        Just ((lstRise, aziRise), (lstSet, aziSet)) -> do
            assertEqualHours (Hrs 16.770043424849998) lstRise
            assertEqualAngle (DMS 64 43 33.777932) aziRise
            assertEqualHours (Hrs 6.54106768626) lstSet
            assertEqualAngle (DMS 295 16 26.222068) aziSet
            
            let
                gstRise = lstToGST lstRise long
                gstSet = lstToGST lstSet long
                gmtRise = gstToGMT gstRise date
                gmtSet = gstToGMT gstSet date
            assertEqualHours (Hrs 12.503376758189999) gstRise
            assertEqualHours (Hrs 2.27440101959) gstSet
            assertEqualHours (HMS 14 18 8.908318) gmtRise
            assertEqualHours (HMS 4 6 5.126033) gmtSet
    let
        maybeRiseSet = calcRiseSet ra dec lat True
    case maybeRiseSet of
        Nothing -> 
            assertFailure "calcRiseSet failed"
        Just ((lstRise, aziRise), (lstSet, aziSet)) -> do
            assertEqualHours (Hrs 16.721803425239997) lstRise
            assertEqualAngle (DMS 64 21 51.255472999999995) aziRise
            assertEqualHours (Hrs 6.58930768587) lstSet
            assertEqualAngle (DMS 295 38 8.744527) aziSet
            
test_calcGeoParallax :: Assertion
test_calcGeoParallax = do
    let
        gmt = HMS 16 45 0
        date = YMD 1979 2 26
        ra = HMS 22 35 9
        dec = DMS (-7) 41 13
        long = Long (Deg 100) W
        lat = Lat (Deg 50) N
        height = 60
        p = DMS 1 1 9
        (ra', dec') = calcGeoParallax gmt date ra dec (Right p) lat long height
    assertEqualHours (HMS 22 36 33.121067) ra'
    assertEqualAngle (DMS (-8) 32 17.530164) dec'
    let
        ra = HMS 22 36 44
        dec = DMS (-8) 44 24
        dist = AU 0.9901
        (ra', dec') = calcGeoParallax gmt date ra dec (Left dist) lat long height
    assertEqualHours (HMS 22 36 44.208639999999995) ra'
    assertEqualAngle (DMS (-8) 44 31.582535999999998) dec'

tests :: TestTree
tests = testGroup "Ephem.CoordsTest"
    [ testCase "raToLHA" test_raToLHA
    , testCase "lhaToRA" test_lhaToRA
    , testCase "equToHor" test_equToHor
    , testCase "horToEqu" test_horToEqu
    , testCase "calcObliquityOfEcliptic" test_calcObliquityOfEcliptic
    , testCase "eclToEqu" test_eclToEqu
    , testCase "equToEcl" test_equToEcl
    , testCase "calcRiseSet" test_calcRiseSet
    , testCase "calcGeoParallax" test_calcGeoParallax
    ]
