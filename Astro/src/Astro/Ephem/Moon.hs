module Astro.Ephem.Moon (
    calcMoon,
    calcMoon2,
    calcMoonPhase,
    calcMoonDistance,
    calcMoonAngularDiameter,
    calcMoonHorizontalParallax,
    calcMoonRiseSet
    ) where

import Astro.Ephem.Utils
import Astro.Ephem.CelestialBody
import Astro.Ephem.OrbitalElements
import Astro.Ephem.Types
import Astro.Ephem.Coords
import Astro.Ephem.Time
import Astro.Ephem.Sun
import Debug.Trace

calcMoon ::
    OrbitalElements -- ^ moon
    -> (EcLong, Angle) -- ^ sun
    -> Date
    -> (EcLong, EcLat, Angle, Angle) -- ^ ecliptic longitude, latitude, corrected mean anomaly, corrected orbital longitude
calcMoon moonElements (sunLong, sunMean) date = 
    let
        d = diffDays date (epoch moonElements)
        ctys = d / 36525
        MeanLongitude longitude = epochData moonElements
        longAscNode = calcAngleElement (longitudeOfAscedingNode moonElements) d
        periapsis =
            case periapsisOrientation moonElements of
                LongitudeOfPeriapsis long -> calcAngleElement long d
                ArgumentOfPeriapsis arg -> (calcAngleElement arg d) `add` longAscNode
        Deg l0 = toDeg $ calcAngleElement longitude d
        l = clipAngleDeg $ l0
        Deg p0 = toDeg periapsis
        -- mean anomaly of the moon
        mM = clipAngleDeg $ l - p0
        Deg n0 = toDeg longAscNode
        n = clipAngleDeg n0
        Rad lS = toRad $ sunLong
        Rad mS = toRad $ sunMean
        ev = clipAngleDeg $ 1.2739 * sin (2 * (l * pi / 180 - lS) - mM) -- evection
        sinmS = sin mS
        ae = 0.1858 * sinmS -- yearly equation
        a3 = 0.37 * sinmS
        -- corrected mean anomaly
        mM' = (trace ("m=" ++ show mM) mM) + ev + ae + a3
        mM'Rad = mM' * pi / 180
        ec = 6.2886 * sin mM'Rad
        a4 = 0.214 * sin (2 * mM'Rad)
        -- corrected longitude
        l' = l + ev + ec - ae + a4
        v = 0.6583 * sin (l' * pi / 180 - lS) -- variation
        l'' = (l' + v)
        n' = n - 0.16 * sinmS
        l''minusN' = (l'' - n') * pi / 180
        sinl''minusN' = sin l''minusN'
        Rad i = toRad $ calcAngleElement (inclination moonElements) ctys
        y = sinl''minusN' * cos i
        x = cos l''minusN'
        long' = atan2 y x
        long = if long' < 0 then long' + 2 * pi else long'
        eclLong = clipAngleRad $ long + n' * pi / 180
        eclLat = asin (sinl''minusN' * sin i)
    in
        (Rad eclLong, Rad eclLat, Deg mM', Deg l'')

-- | More precise calculation of Moon's ecliptic coordinates
calcMoon2 ::
    OrbitalElements -- ^ moon
    -> (EcLong, Angle) -- ^ sun
    -> Date
    -> (EcLong, EcLat, Distance) -- ^ ecliptic longitude, latitude, distance
calcMoon2 moonElements (sunLong, sunMean) date = 
    let
        d = diffDays date (epoch moonElements)
        ctys = d / 36525
        longAscNode = calcAngleElement (longitudeOfAscedingNode moonElements) d
        Rad periapsis = toRad $
            case periapsisOrientation moonElements of
                LongitudeOfPeriapsis long -> calcAngleElement long d
                ArgumentOfPeriapsis arg -> (calcAngleElement arg d) `add` longAscNode
        w = clipAngleRad periapsis -- Arg. of perigee
        Rad node = toRad longAscNode
        n = clipAngleRad node -- Long asc node
        MeanLongitude longitude = epochData moonElements
        Rad meanLongitude = toRad $ calcAngleElement longitude (trace ("d=" ++ show d) d)
        m = clipAngleRad meanLongitude -- Mean anomaly
        Rad i = toRad $ calcAngleElement (inclination moonElements) ctys
        lM = clipAngleRad $ n + w + m
        Rad lS = toRad $ sunLong -- Sun's mean longitude
        Rad mS = toRad $ sunMean -- Sun's mean anomaly
        me = clipAngleRad $ lM - lS -- Moon's mean elongation
        f = clipAngleRad $ lM - n -- Moon's argument of latitude
        
        e = calcElement (eccentricity moonElements) ctys
        eccAnomaly = calcEccAnomaly (trace ("m=" ++ show m ) m) e
        trueAnomaly = calcTrueAnomaly eccAnomaly e
        ClosedOrbitElements (a, _) _ = elementsByOrbitType moonElements 
        EarthRadii aInEarthRadii = toEarthRadii a
        dist = aInEarthRadii * (1 - e * e) / (1 + e * cos trueAnomaly)
        moonEclLong = calcLongitude trueAnomaly (w + n)
        moonEclLat = asin ((sin (moonEclLong - n)) * sin i)
        moonDist = dist  * cos moonEclLat 
        
        d2 = 2 * me
        
        longPerturbations = Deg $
            -1.274 * sin (m - d2) -- evection
            +0.658 * sin d2 -- variation
            -0.186 * sin mS -- ywarly ecuation
            -0.059 * sin (2 * m - d2)
            -0.057 * sin (m - d2 + mS)
            +0.053 * sin (m + d2)
            +0.046 * sin (d2 - mS)
            +0.041 * sin (m - mS)
            -0.035 * sin me -- parallactic equation
            -0.031 * sin (m + mS)
            -0.015 * sin (2 * f - d2)
            +0.011 * sin (m - 2 * d2)
            
        latPerturbations = Deg $ 
            -0.173 * sin (f - d2)
            -0.055 * sin (m - f - d2)
            -0.046 * sin (m + f - d2)
            +0.033 * sin (f + d2)
            +0.017 * sin (2 * m + f)
        
        distPerturbations = 
            -0.58 * cos (m - d2)
            -0.46 * cos d2
        
    in
        (Rad moonEclLong `add` longPerturbations, Rad moonEclLat `add` latPerturbations, EarthRadii (moonDist + distPerturbations))

 
calcMoonPhase :: 
    EcLong -- ^ longitude of the sun 
    -> Angle -- ^ orbital longitude of the moon
    -> Double
calcMoonPhase sunLong l'' =
    let
        Rad lS = toRad $ sunLong
        Rad lM = toRad $ l''
    in
        (1 - cos (lM - lS)) / 2

calcMoonDistance ::
    OrbitalElements -- ^ moon
    -> Angle -- ^ corrected mean anomaly
    -> Date
    -> Double -- ^ distance in the units of semi-major axis
calcMoonDistance moon mM' date =
    let
        d = diffDays date (epoch moon)
        ctys = d / 36525
        Rad mM'Rad = toRad mM'
        e = calcElement (eccentricity moon) ctys
        ec = 6.2886 * sin mM'Rad * pi / 180
        dist = (1 - e * e) / (1 + e * cos (mM'Rad + ec))
    in
        dist
        
calcMoonAngularDiameter ::
    Double -- ^ distance in the units of semi-major axis
    -> Angle
calcMoonAngularDiameter dist = 
    angularDiameter moon1980 `mul` (1 / dist)

calcMoonHorizontalParallax ::
    Double -- ^ distance in the units of semi-major axis
    -> Angle
calcMoonHorizontalParallax dist =
    let
        p0 = Deg 0.9507 -- ^ parallax at the distance of semi-major axis
    in
        p0 `mul`  (1 / dist)

calcMoonRiseSet ::
    Date
    -> OrbitalElements -- ^ moon
    -> OrbitalElements -- ^ earth
    -> Lat
    -> Long
    -> Double -- ^ altitude in meters above sea level
    -> Int -- ^ number of iterations 
    -> Maybe (LST, LST) -- ^ (rise time, set time)   
calcMoonRiseSet date moon earth lat long height numIterations =
    let
        YMD y m d = toYMD date
        date1 = YMD y m (fromIntegral (floor d)) 
        tilt = calcObliquityOfEcliptic date1
        sun1 = calcSun earth date1
        (moonLong1, moonLat1, mM'1, _) = calcMoon moon1980 sun1 date1
        (moonRA1, moonDec1) = eclToEqu moonLong1 moonLat1 tilt
        date2 = YMD y m ((fromIntegral (floor d)) + 0.5)
        sun2 = calcSun earth date2
        (moonLong2, moonLat2, mM'2, _) = calcMoon moon1980 sun2 date2
        (moonRA2, moonDec2) = eclToEqu moonLong2 moonLat2 tilt
        ctys1 = diffDays date1 (epoch moon) / 36525
        ctys2 = diffDays date2 (epoch moon) / 36525
        ClosedOrbitElements (semiMajorAxis, _) _ = elementsByOrbitType moon
        Km semiMajorAxisInKm = toKm semiMajorAxis
        a1 = semiMajorAxisInKm
        a2 = semiMajorAxisInKm
        dist1 = calcMoonDistance moon mM'1 date1
        dist2 = calcMoonDistance moon mM'2 date2
        meanAngularDiameter = (calcMoonAngularDiameter dist1 `add` calcMoonAngularDiameter dist2) `mul` 0.5 
        (moonRA1', moonDec1') = calcGeoParallax (HMS 0 0 0) date moonRA1 moonDec1 (Left (Km (a1 * dist1))) lat long height 
        (moonRA2', moonDec2') = calcGeoParallax (HMS 12 0 0) date moonRA2 moonDec2 (Left (Km (a2 * dist2))) lat long height 
        riseSet = case calcRiseSet moonRA1' moonDec1' lat False of
            Just ((Hrs st1r, _), (Hrs st1s, _)) ->
                case calcRiseSet moonRA2' moonDec2' lat False of
                    Just ((Hrs st2r, _), (Hrs st2s, _)) ->
                        let
                            tr = 12.03 * st1r / (12.03 + st1r - st2r)
                            ts = 12.03 * st1s / (12.03 + st1s - st2s)
                            x = Deg 0.56666666667 `add` meanAngularDiameter 
                            (Hrs deltaT, _) = calcRiseSetCorrection ((moonDec1' `add` moonDec2') `mul` 0.5)  lat x
                        in
                            Just (Hrs (clipHour (tr - deltaT)), Hrs (clipHour (ts + deltaT)))
                    Nothing -> Nothing
            Nothing -> Nothing

    in
        riseSet            