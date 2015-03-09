{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Ephem.CelestialBodyTest where

import Ephem.OrbitalElements
import Ephem.CelestialBody
import Ephem.Types
import Ephem.Time
import Ephem.Coords
import Ephem.TestUtils
import Test.Framework
import Debug.Trace

test_calcEclCoordsPlanets = do
    let
        date = YMD 1980 11 22
        earthHelCoords@(earthHelLong, earthHelDist) = calcHelLongAndDist earth1980 date
        mercuryHelCoords@(mercuryHelLong, mercuryHelDist) = calcHelLongAndDist mercury1980 date
        (mercuryLong, mercuryLat) = calcEclCoords mercury1980 earth1980 mercuryHelCoords earthHelCoords date
        tilt = calcObliquityOfEcliptic date
        (mercuryRA, mercuryDec) = eclToEqu mercuryLong mercuryLat tilt
        mercuryPhase = calcPhase mercuryHelLong mercuryLong 
        mercuryDist = calcDistance mercuryHelCoords earthHelCoords
        mercuryDiam = calcAngularDiameter (angularDiameter mercury1980) mercuryDist
        mercuryMag = calcMagnitude mercuryHelDist mercuryDist mercuryPhase (albedo mercury1980)

        jupiterHelCoords@(jupiterHelLong, jupiterHelDist) = calcHelLongAndDist jupiter1980 date
        (jupiterLong, jupiterLat) = calcEclCoords jupiter1980 earth1980 jupiterHelCoords earthHelCoords date
        (jupiterRA, jupiterDec) = eclToEqu jupiterLong jupiterLat tilt
        jupiterPhase = calcPhase jupiterHelLong jupiterLong
        jupiterDist = calcDistance jupiterHelCoords earthHelCoords
        jupiterDiam = calcAngularDiameter (angularDiameter jupiter1980) jupiterDist
        jupiterMag = calcMagnitude jupiterHelDist jupiterDist jupiterPhase (albedo jupiter1980)
        AU earthHelDistAU = toAU earthHelDist
        AU mercuryHelDistAU = toAU mercuryHelDist
        AU jupiterHelDistAU = toAU jupiterHelDist
        AU mercuryDistAU = toAU mercuryDist
        AU jupiterDistAU = toAU jupiterDist
        
    assertEqualAngle (Rad 1.0443811531766871) earthHelLong
    assertEqualDouble 0.987597878333591 earthHelDistAU

    assertEqualAngle (Rad 2.63673763517258) mercuryHelLong
    assertEqualDouble 0.350764379258833 mercuryHelDistAU
    assertEqualAngle (DMS 220 33 50.708973010529995) mercuryLong
    assertEqualAngle (DMS 2 15 46.40517747824) mercuryLat
    assertEqualHours (HMS 14 35 29.00097953577) mercuryRA
    assertEqualAngle (DMS (-12) 50 33.08007636265) mercuryDec
    assertEqualDouble 0.67518422124 mercuryPhase
    assertEqualDouble 1.05514053461 mercuryDistAU
    assertEqualAngle (Sec 6.3877746886799995) mercuryDiam
    assertEqualDouble 0.15383022923999998 mercuryMag

    assertEqualAngle (Rad 3.07043201920662) jupiterHelLong
    assertEqualDouble 5.44101337609076 jupiterHelDistAU
    assertEqualAngle (DMS 184 30 24.52926699912723) jupiterLong
    assertEqualAngle (DMS 1 09 25.59650044727917) jupiterLat
    assertEqualHours (HMS 12 18 22.87719014020454) jupiterRA
    assertEqualAngle (DMS 0 (-43) 44.96676821002864) jupiterDec
    assertEqualDouble 0.99439902726 jupiterPhase
    assertEqualDouble 5.9418316989 jupiterDistAU
    assertEqualAngle (Sec 33.11100178694) jupiterDiam
    assertEqualDouble (-0.64452679731) jupiterMag

test_calcPositionAngle = do
    let
        sunRA = HMS 15 50 37
        sunDec = DMS (-20) 7 4
        mercuryRA = HMS 14 35 2
        mercuryDec = DMS (-12) 45 46
        posAngle = calcPositionAngle(sunRA, sunDec) (mercuryRA, mercuryDec)
    assertEqualAngle (Deg 114.59571316847999) posAngle


-- Elliptical
test_calcEclCoordsHalley = do 
    let
        date = ymd 1986 1 0
        earthHelCoords@(earthHelLong, earthHelDist) = calcHelLongAndDist earth1980 date
        halleyHelCoords@(halleyHelLong, halleyHelDist) = calcHelLongAndDist halley date
        (halleyLong, halleyLat) = calcEclCoords halley earth1980 halleyHelCoords earthHelCoords date
        tilt = calcObliquityOfEcliptic date
        (halleyRA, halleyDec) = eclToEqu halleyLong halleyLat tilt
        AU earthHelDistAU = toAU earthHelDist
        AU halleyHelDistAU = toAU halleyHelDist

    assertEqualAngle (Rad 1.7308970607309) earthHelLong
    assertEqualDouble 0.98331085106086 earthHelDistAU

    assertEqualAngle (Rad 1.52275868024746) halleyHelLong
    assertEqualDouble 1.0289428442785 halleyHelDistAU
    assertEqualAngle (DMS 336 7 54.41908304695) halleyLong
    assertEqualAngle (DMS 7 40 24.359088379019997) halleyLat
    assertEqualHours (HMS 22 20 19.66376905559) halleyRA
    assertEqualAngle (DMS (-2) 7 17.26739888136) halleyDec

-- Parabolic
test_calcEclCoordsKohler1977m = do
    let
        date = YMD 1977 12 25
        earthHelCoords@(earthHelLong, earthHelDist) = calcHelLongAndDist earth1980 date
        kohlerHelCoords@(kohlerHelLong, kohlerHelDist) = calcHelLongAndDist kohler1977m date
        (kohlerLong, kohlerLat) = calcEclCoords kohler1977m earth1980 kohlerHelCoords earthHelCoords date
        tilt = calcObliquityOfEcliptic date
        (kohlerRA, kohlerDec) = eclToEqu kohlerLong kohlerLat tilt
        AU earthHelDistAU = toAU earthHelDist
        AU kohlerHelDistAU = toAU kohlerHelDist

    assertEqualAngle (Rad 1.62515260709) earthHelLong
    assertEqualDouble 0.9835029501499999 earthHelDistAU

    assertEqualAngle (Rad 0.67894298261) kohlerHelLong
    assertEqualDouble 1.24347674541 kohlerHelDistAU
    assertEqualAngle (DMS 336 5 47.58205230503) kohlerLong
    assertEqualAngle (DMS (-26) 35 14.94873976047) kohlerLat
    assertEqualHours (HMS 23 17 13.439912127609999) kohlerRA
    assertEqualAngle (DMS (-33) 41 42.33512859685) kohlerDec

-- Hyperbolic
test_calcEclCoordsPanstarrsC2011L4 = do
    let
        date = YMD 2013 03 19
        earthHelCoords@(earthHelLong, earthHelDist) = calcHelLongAndDist earth1980 date

        panstarrsHelCoords@(panstarrsHelLong, panstarrsDist) = calcHelLongAndDist panstarrsC2011L4 date
        (panstarrsLong, panstarrsLat) = calcEclCoords panstarrsC2011L4 earth1980 panstarrsHelCoords earthHelCoords date

        tilt = calcObliquityOfEcliptic date
        (panstarrsRA, panstarrsDec) = eclToEqu panstarrsLong panstarrsLat tilt

    --assertEqualHours (HMS 0 35 17.6) panstarrsRA
    --assertEqualAngle (DMS 15 52 53) panstarrsDec
    assertEqualHours (HMS 0 34 20.95961957262) panstarrsRA
    assertEqualAngle (DMS 15 48 46.36899256674) panstarrsDec

test_calcEclCoordsMcNaughtC2009R1 = do
    let
        date = YMD 2013 06 17
        earthHelCoords@(earthHelLong, earthHelDist) = calcHelLongAndDist earth2000 date

        mcNaughtHelCoords@(mcNaughtHelLong, mcNaughtDist) = calcHelLongAndDist mcNaughtC2009R1 date
        (mcNaughtLong, mcNaughtLat) = calcEclCoords mcNaughtC2009R1 earth2000 mcNaughtHelCoords earthHelCoords date
        tilt = calcObliquityOfEcliptic date
        (mcNaughtRA, mcNaughtDec) = eclToEqu mcNaughtLong mcNaughtLat tilt

    assertEqualHours (HMS 2 47 3.01465) mcNaughtRA
    assertEqualAngle (DMS (-79) 38 53.783736999999995) mcNaughtDec

test_calcPlanetRiseSet = do
    let
        date = ymd 2012 4 1
        lat = Lat (DMS 58 22 47) N
        long = Long (DMS 26 43 18) E
        maybeRiseSet = calcPlanetRiseSet date venus2000 earth2000 lat long
    case maybeRiseSet of
        Nothing -> 
            assertFailure "calcPlanetRiseSet failed"
        Just ((gmtRise, aziRise), (gmtSet, aziSet)) -> do
            assertEqualHours (HMS 4 11 31.847156) gmtRise
            assertEqualHours (HMS 22 24 22.521236) gmtSet
            assertEqualAngle (DMS 39 19 27.727452) aziRise
            assertEqualAngle (DMS 321 14 17.536600999999997) aziSet
            