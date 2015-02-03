{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Astro.Ephem.TypesTest where

import Astro.Ephem.Types
import Astro.Ephem.TestUtils
import Test.Framework

test_toDMS = do 
    assertEqualAngle (DMS 17 35 12.1) (toDMS (DMS 17 35 12.1))
    assertEqualAngle (DMS 256 52 34.82400000009875) (toDMS (Deg 256.87634))
    assertEqualAngle (DMS (-256) 52 34.82400000009875) (toDMS (Deg (-256.87634)))
    assertEqualAngle (DMS 0 (-52) 34.82399999999984) (toDMS (Deg (-0.87634)))
    assertEqualAngle (DMS 0 0 (-34.8239988)) (toDMS (Deg (-0.009673333)))
    assertEqualAngle (DMS (166) 24 53.65456701650601) (toDMS (Rad (2.904488)))
    assertEqualAngle (DMS 256 52 34.82400000009875) (toDMS (Sec 924754.824))

test_toDeg = do 
    assertEqualAngle (Deg 17.586666666666667) (toDeg (DMS 17 35 12))
    assertEqualAngle (Deg (-256.87634)) (toDeg (DMS (-256) 52 34.82400000009875))
    assertEqualAngle (Deg (-0.87634)) (toDeg (DMS 0 (-52) 34.82399999999984))
    assertEqualAngle (Deg (-0.009673333)) (toDeg (DMS 0 0 (-34.8239988)))
    assertEqualAngle (Deg 256.87634) (toDeg (Deg 256.87634))
    assertEqualAngle (Deg 166.41490404639347) (toDeg (Rad 2.904488))
    assertEqualAngle (Deg 256.87634) (toDMS (Sec 924754.824))

test_toRad = do 
    assertEqualAngle (Rad 0.3069452377840694) (toRad (DMS 17 35 12))
    assertEqualAngle (Rad 4.4833379034724108) (toRad (Deg 256.87634))
    assertEqualAngle (Rad 2.904488) (toRad (Rad 2.904488))
    assertEqualAngle (Rad 4.4833379034724108) (toRad (Sec 924754.824))

test_toSec = do 
    assertEqualAngle (Sec 63312.000000000001) (toSec (DMS 17 35 12))
    assertEqualAngle (Sec 924754.8240000001) (toSec (Deg 256.87634))
    assertEqualAngle (Sec 599093.6545670165) (toSec (Rad 2.904488))
    assertEqualAngle (Sec 924754.824) (toSec (Sec 924754.824))

test_angleToHours = do
    assertEqualHours (HMS 1 10 20.79999999999984) (angleToHours (DMS 17 35 12))
    assertEqualHours (Hrs 17.1250893333333335) (angleToHours (Deg 256.87634))
    assertEqualHours (Hrs 11.094326936426231) (angleToHours (Rad 2.904488))

test_hoursToAngle = do
    assertEqualAngle (Deg 203.2335345) (hoursToAngle (Hrs 13.5489023))
    assertEqualAngle (DMS 85 52 15.000000000013642) (hoursToAngle (HMS 5 43 29))

test_absAngle = do 
    assertEqualAngle (DMS 17 35 12) (absAngle (DMS 17 35 12))
    assertEqualAngle (DMS 256 52 34.82400000009875) (absAngle (DMS (-256) 52 34.82400000009875))
    assertEqualAngle (DMS 0 52 34.82399999999984) (absAngle (DMS 0 (-52) 34.82399999999984))
    assertEqualAngle (DMS 0 0 34.8239988) (absAngle (DMS 0 0 (-34.8239988)))
    assertEqualAngle (Deg 256.87634) (absAngle (Deg 256.87634))
    assertEqualAngle (Deg 256.87634) (absAngle (Deg (-256.87634)))
    assertEqualAngle (Rad 2.904488) (absAngle (Rad 2.904488))
    assertEqualAngle (Rad 2.904488) (absAngle (Rad (-2.904488)))
    assertEqualAngle (Sec 924754.824) (absAngle (Sec 924754.824))
    assertEqualAngle (Sec 924754.824) (absAngle (Sec (-924754.824)))

test_sgnAngle = do 
    assertEqual 1 (sgnAngle (DMS 17 35 12))
    assertEqual (-1) (sgnAngle (DMS (-256) 52 34.82400000009875))
    assertEqual (-1) (sgnAngle (DMS 0 (-52) 34.82399999999984))
    assertEqual (-1) (sgnAngle (DMS 0 0 (-34.8239988)))
    assertEqual 0 (sgnAngle (DMS 0 0 0))
    assertEqual 1 (sgnAngle (Deg 256.87634))
    assertEqual (-1) (sgnAngle (Deg (-256.87634)))
    assertEqual 0 (sgnAngle (Deg 0))
    assertEqual 1 (sgnAngle (Rad 2.904488))
    assertEqual (-1) (sgnAngle (Rad (-2.904488)))
    assertEqual 0 (sgnAngle (Rad 0))
    assertEqual 1 (sgnAngle (Sec 924754.824))
    assertEqual (-1) (sgnAngle (Sec (-924754.824)))
    assertEqual 0 (sgnAngle (Sec 0))

test_toHMS = do 
    assertEqualHours (HMS 18 31 27) (toHMS (HMS 18 31 27))
    assertEqualHours (HMS 18 31 27.012000000005685) (toHMS (Hrs 18.52417))

test_toHrs = do 
    assertEqualHours (Hrs 18.52416666666666) (toHrs (HMS 18 31 27))
    assertEqualHours (Hrs 18.52417) (toHrs (Hrs 18.52417))

--------------------------------------------------------------------------------
test_toValidYMD = do
    assertEqual (YMD 1980 1 13) (toValidYMD (YMD 1979 15 (-47)))  
    assertEqual (YMD 1977 10 17) (toValidYMD (YMD 1979 (-15) 47))  
    
test_ymdToJD = do
    assertEqual (JD 2446113.75) (toJD (YMD 1985 2 17.25))
    assertEqual (JD 2444238.50) (toJD (toValidYMD (YMD 1980 1 0.0)))

test_ymdToTropicalYears = do
    assertEqual (TropicalYears 1986.112) (toTropicalYears (YMD 1986 2 9.9071264))

test_ymdToRJD = do
    assertEqual (RJD 46113.75) (toRJD (YMD 1985 2 17.25))

test_ymdToMJD = do
    assertEqual (MJD 46113.25) (toMJD (YMD 1985 2 17.25))

test_ymdToTJD = do
    assertEqual (TJD 6113.25) (toTJD (YMD 1985 2 17.25))

test_tropicalYearsToYMD = do
    assertEqual (YMD 1986 2 9.907126400030393) (toYMD (TropicalYears 1986.112))

test_JDToYMD = do
    assertEqual (YMD 1985 2 17.25) (toYMD (JD 2446113.75))

test_RJDToYMD = do
    assertEqual (YMD 1985 2 17.25) (toYMD (RJD 46113.75))

test_MJDToYMD = do
    assertEqual (YMD 1985 2 17.25) (toYMD (MJD 46113.25))

test_TJDToYMD = do
    assertEqual (YMD 1985 2 17.25) (toYMD (TJD 6113.25))

test_diffDays = do
    assertEqual (-522) (diffDays (YMD 1978 7 27) (ymd 1980 1 0))
    
test_toLatitude = do
    assertEqual (Lat (Deg 30) N) (toLatitude (Deg 30))
    assertEqual (Lat (Deg 30) S) (toLatitude (Deg (-30)))

test_toLongitude = do
    assertEqual (Long (Deg 30) E) (toLongitude (Deg 30))
    assertEqual (Long (Deg 30) W) (toLongitude (Deg (-30)))
    