module Ephem.Moon (
    calcMoon,
    calcMoon2,
    calcMoonMeeus,
    calcMoonPhase,
    calcMoonElongation,
    MoonPhase(..),
    getMoonPhaseName,
    calcMoonDistance,
    calcMoonAngularDiameter,
    calcMoonHorizontalParallax,
    calcMoonRiseSet,
    calcMoonRiseSetMeeus
    ) where

import Ephem.Utils
import Ephem.CelestialBody
import Ephem.OrbitalElements
import Ephem.Types
import Ephem.Coords
import Ephem.Time
import Ephem.Sun
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
        mM' = mM + ev + ae + a3
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
        Rad meanLongitude = toRad $ calcAngleElement longitude d
        m = clipAngleRad meanLongitude -- Mean anomaly
        Rad i = toRad $ calcAngleElement (inclination moonElements) ctys
        lM = clipAngleRad $ n + w + m
        Rad lS = toRad $ sunLong -- Sun's mean longitude
        Rad mS = toRad $ sunMean -- Sun's mean anomaly
        me = clipAngleRad $ lM - lS -- Moon's mean elongation
        f = clipAngleRad $ lM - n -- Moon's argument of latitude
        
        e = calcElement (eccentricity moonElements) ctys
        eccAnomaly = calcEccAnomaly m e
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
            -0.186 * sin mS -- yearly equation
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

-- | High precision calculation of Moon's position using Meeus "Astronomical Algorithms" Chapter 47
-- Based on the ELP-2000/82 theory with the principal periodic terms
-- Reference: Jean Meeus, "Astronomical Algorithms", 2nd Edition, Chapter 47
calcMoonMeeus ::
    Date
    -> (EcLong, EcLat, Distance) -- ^ ecliptic longitude, latitude, distance in km
calcMoonMeeus date =
    let
        -- Julian centuries from J2000.0
        JD jd = toJD date
        t = (jd - 2451545.0) / 36525.0
        t2 = t * t
        t3 = t2 * t
        t4 = t3 * t

        -- Fundamental arguments (in degrees) - Meeus Table 47.a
        -- L' = Moon's mean longitude (mean equinox of date)
        lPrime = clipAngleDeg $ 218.3164477 + 481267.88123421 * t
                 - 0.0015786 * t2 + t3 / 538841.0 - t4 / 65194000.0

        -- D = Mean elongation of the Moon
        d = clipAngleDeg $ 297.8501921 + 445267.1114034 * t
            - 0.0018819 * t2 + t3 / 545868.0 - t4 / 113065000.0

        -- M = Sun's mean anomaly
        m = clipAngleDeg $ 357.5291092 + 35999.0502909 * t
            - 0.0001536 * t2 + t3 / 24490000.0

        -- M' = Moon's mean anomaly
        mPrime = clipAngleDeg $ 134.9633964 + 477198.8675055 * t
                 + 0.0087414 * t2 + t3 / 69699.0 - t4 / 14712000.0

        -- F = Moon's argument of latitude
        f = clipAngleDeg $ 93.2720950 + 483202.0175233 * t
            - 0.0036539 * t2 - t3 / 3526000.0 + t4 / 863310000.0

        -- Additional arguments for planetary perturbations
        a1 = clipAngleDeg $ 119.75 + 131.849 * t      -- Venus
        a2 = clipAngleDeg $ 53.09 + 479264.290 * t    -- Jupiter
        a3 = clipAngleDeg $ 313.45 + 481266.484 * t

        -- Eccentricity of Earth's orbit
        e = 1.0 - 0.002516 * t - 0.0000074 * t2
        e2 = e * e

        -- Convert to radians for trig functions
        dRad = d * pi / 180.0
        mRad = m * pi / 180.0
        mPrimeRad = mPrime * pi / 180.0
        fRad = f * pi / 180.0
        a1Rad = a1 * pi / 180.0
        a2Rad = a2 * pi / 180.0
        a3Rad = a3 * pi / 180.0

        -- Sum of longitude terms (Table 47.A) - coefficients in 0.000001 degrees
        -- Each term: (D, M, M', F, coeffSinL, coeffCosR)
        -- Terms with M are multiplied by E, terms with 2M by E²
        sigmaL = sum $ map (\(dCoef, mCoef, mPrimeCoef, fCoef, sinCoef, _) ->
            let arg = dCoef * dRad + mCoef * mRad + mPrimeCoef * mPrimeRad + fCoef * fRad
                eCorrection = case abs mCoef of
                    1 -> e
                    2 -> e2
                    _ -> 1.0
            in eCorrection * sinCoef * sin arg
            ) table47A

        -- Sum of distance terms (Table 47.A) - coefficients in 0.001 km
        sigmaR = sum $ map (\(dCoef, mCoef, mPrimeCoef, fCoef, _, cosCoef) ->
            let arg = dCoef * dRad + mCoef * mRad + mPrimeCoef * mPrimeRad + fCoef * fRad
                eCorrection = case abs mCoef of
                    1 -> e
                    2 -> e2
                    _ -> 1.0
            in eCorrection * cosCoef * cos arg
            ) table47A

        -- Sum of latitude terms (Table 47.B) - coefficients in 0.000001 degrees
        sigmaB = sum $ map (\(dCoef, mCoef, mPrimeCoef, fCoef, sinCoef) ->
            let arg = dCoef * dRad + mCoef * mRad + mPrimeCoef * mPrimeRad + fCoef * fRad
                eCorrection = case abs mCoef of
                    1 -> e
                    2 -> e2
                    _ -> 1.0
            in eCorrection * sinCoef * sin arg
            ) table47B

        -- Additional additive terms for longitude
        addL = 3958.0 * sin a1Rad           -- Venus
             + 1962.0 * sin (lPrime * pi / 180.0 - fRad)  -- Flattening of Earth
             + 318.0 * sin a2Rad            -- Jupiter

        -- Additional additive terms for latitude
        addB = -2235.0 * sin (lPrime * pi / 180.0)
             + 382.0 * sin a3Rad
             + 175.0 * sin (a1Rad - fRad)
             + 175.0 * sin (a1Rad + fRad)
             + 127.0 * sin (lPrime * pi / 180.0 - mPrimeRad)
             - 115.0 * sin (lPrime * pi / 180.0 + mPrimeRad)

        -- Final coordinates
        longitude = lPrime + (sigmaL + addL) / 1000000.0
        latitude = (sigmaB + addB) / 1000000.0
        distance = 385000.56 + sigmaR / 1000.0  -- in km

    in
        (Deg longitude, Deg latitude, Km distance)

-- | Table 47.A from Meeus - Periodic terms for Moon's longitude and distance
-- Format: (D, M, M', F, Σl coefficient, Σr coefficient)
-- Σl in units of 0.000001 degree, Σr in units of 0.001 km
table47A :: [(Double, Double, Double, Double, Double, Double)]
table47A =
    [ (0, 0, 1, 0, 6288774, -20905355)
    , (2, 0, -1, 0, 1274027, -3699111)
    , (2, 0, 0, 0, 658314, -2955968)
    , (0, 0, 2, 0, 213618, -569925)
    , (0, 1, 0, 0, -185116, 48888)
    , (0, 0, 0, 2, -114332, -3149)
    , (2, 0, -2, 0, 58793, 246158)
    , (2, -1, -1, 0, 57066, -152138)
    , (2, 0, 1, 0, 53322, -170733)
    , (2, -1, 0, 0, 45758, -204586)
    , (0, 1, -1, 0, -40923, -129620)
    , (1, 0, 0, 0, -34720, 108743)
    , (0, 1, 1, 0, -30383, 104755)
    , (2, 0, 0, -2, 15327, 10321)
    , (0, 0, 1, 2, -12528, 0)
    , (0, 0, 1, -2, 10980, 79661)
    , (4, 0, -1, 0, 10675, -34782)
    , (0, 0, 3, 0, 10034, -23210)
    , (4, 0, -2, 0, 8548, -21636)
    , (2, 1, -1, 0, -7888, 24208)
    , (2, 1, 0, 0, -6766, 30824)
    , (1, 0, -1, 0, -5163, -8379)
    , (1, 1, 0, 0, 4987, -16675)
    , (2, -1, 1, 0, 4036, -12831)
    , (2, 0, 2, 0, 3994, -10445)
    , (4, 0, 0, 0, 3861, -11650)
    , (2, 0, -3, 0, 3665, 14403)
    , (0, 1, -2, 0, -2689, -7003)
    , (2, 0, -1, 2, -2602, 0)
    , (2, -1, -2, 0, 2390, 10056)
    , (1, 0, 1, 0, -2348, 6322)
    , (2, -2, 0, 0, 2236, -9884)
    , (0, 1, 2, 0, -2120, 5751)
    , (0, 2, 0, 0, -2069, 0)
    , (2, -2, -1, 0, 2048, -4950)
    , (2, 0, 1, -2, -1773, 4130)
    , (2, 0, 0, 2, -1595, 0)
    , (4, -1, -1, 0, 1215, -3958)
    , (0, 0, 2, 2, -1110, 0)
    , (3, 0, -1, 0, -892, 3258)
    , (2, 1, 1, 0, -810, 2616)
    , (4, -1, -2, 0, 759, -1897)
    , (0, 2, -1, 0, -713, -2117)
    , (2, 2, -1, 0, -700, 2354)
    , (2, 1, -2, 0, 691, 0)
    , (2, -1, 0, -2, 596, 0)
    , (4, 0, 1, 0, 549, -1423)
    , (0, 0, 4, 0, 537, -1117)
    , (4, -1, 0, 0, 520, -1571)
    , (1, 0, -2, 0, -487, -1739)
    , (2, 1, 0, -2, -399, 0)
    , (0, 0, 2, -2, -381, -4421)
    , (1, 1, 1, 0, 351, 0)
    , (3, 0, -2, 0, -340, 0)
    , (4, 0, -3, 0, 330, 0)
    , (2, -1, 2, 0, 327, 0)
    , (0, 2, 1, 0, -323, 1165)
    , (1, 1, -1, 0, 299, 0)
    , (2, 0, 3, 0, 294, 0)
    , (2, 0, -1, -2, 0, 8752)
    ]

-- | Table 47.B from Meeus - Periodic terms for Moon's latitude
-- Format: (D, M, M', F, Σb coefficient)
-- Σb in units of 0.000001 degree
table47B :: [(Double, Double, Double, Double, Double)]
table47B =
    [ (0, 0, 0, 1, 5128122)
    , (0, 0, 1, 1, 280602)
    , (0, 0, 1, -1, 277693)
    , (2, 0, 0, -1, 173237)
    , (2, 0, -1, 1, 55413)
    , (2, 0, -1, -1, 46271)
    , (2, 0, 0, 1, 32573)
    , (0, 0, 2, 1, 17198)
    , (2, 0, 1, -1, 9266)
    , (0, 0, 2, -1, 8822)
    , (2, -1, 0, -1, 8216)
    , (2, 0, -2, -1, 4324)
    , (2, 0, 1, 1, 4200)
    , (2, 1, 0, -1, -3359)
    , (2, -1, -1, 1, 2463)
    , (2, -1, 0, 1, 2211)
    , (2, -1, -1, -1, 2065)
    , (0, 1, -1, -1, -1870)
    , (4, 0, -1, -1, 1828)
    , (0, 1, 0, 1, -1794)
    , (0, 0, 0, 3, -1749)
    , (0, 1, -1, 1, -1565)
    , (1, 0, 0, 1, -1491)
    , (0, 1, 1, 1, -1475)
    , (0, 1, 1, -1, -1410)
    , (0, 1, 0, -1, -1344)
    , (1, 0, 0, -1, -1335)
    , (0, 0, 3, 1, 1107)
    , (4, 0, 0, -1, 1021)
    , (4, 0, -1, 1, 833)
    , (0, 0, 1, -3, 777)
    , (4, 0, -2, 1, 671)
    , (2, 0, 0, -3, 607)
    , (2, 0, 2, -1, 596)
    , (2, -1, 1, -1, 491)
    , (2, 0, -2, 1, -451)
    , (0, 0, 3, -1, 439)
    , (2, 0, 2, 1, 422)
    , (2, 0, -3, -1, 421)
    , (2, 1, -1, 1, -366)
    , (2, 1, 0, 1, -351)
    , (4, 0, 0, 1, 331)
    , (2, -1, 1, 1, 315)
    , (2, -2, 0, -1, 302)
    , (0, 0, 1, 3, -283)
    , (2, 1, 1, -1, -229)
    , (1, 1, 0, -1, 223)
    , (1, 1, 0, 1, 223)
    , (0, 1, -2, -1, -220)
    , (2, 1, -1, -1, -220)
    , (1, 0, 1, 1, -185)
    , (2, -1, -2, -1, 181)
    , (0, 1, 2, 1, -177)
    , (4, 0, -2, -1, 176)
    , (4, -1, -1, -1, 166)
    , (1, 0, 1, -1, -164)
    , (4, 0, 1, -1, 132)
    , (1, 0, -1, -1, -119)
    , (4, -1, 0, -1, 115)
    , (2, -2, 0, 1, 107)
    ]

-- | Calculate moon illumination fraction (0 = new moon, 1 = full moon)
-- Note: This returns the same value for first quarter and last quarter (both ~0.5)
-- Use 'getMoonPhaseName' with 'calcMoonElongation' to distinguish waxing/waning phases
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

-- | Calculate moon elongation angle (0-360 degrees)
-- This is the angle between the moon and sun as seen from Earth
-- 0° = new moon, 90° = first quarter, 180° = full moon, 270° = last quarter
calcMoonElongation ::
    EcLong -- ^ longitude of the sun
    -> Angle -- ^ orbital longitude of the moon
    -> Angle -- ^ elongation in degrees (0-360)
calcMoonElongation sunLong moonLong =
    let
        Deg lS = toDeg sunLong
        Deg lM = toDeg moonLong
        elongation = lM - lS
        -- Normalize to 0-360 range
        normalizedElong = if elongation < 0 then elongation + 360 else elongation
    in
        Deg (if normalizedElong >= 360 then normalizedElong - 360 else normalizedElong)

-- | Moon phase names
data MoonPhase
    = NewMoon
    | WaxingCrescent
    | FirstQuarter
    | WaxingGibbous
    | FullMoon
    | WaningGibbous
    | LastQuarter
    | WaningCrescent
    deriving (Eq, Show)

-- | Get moon phase name from elongation angle
-- The elongation is the angle between moon and sun (0-360°)
getMoonPhaseName ::
    Angle -- ^ elongation angle from 'calcMoonElongation'
    -> MoonPhase
getMoonPhaseName elongation =
    let
        Deg e = toDeg elongation
        -- Normalize to 0-360
        en = if e < 0 then e + 360 else if e >= 360 then e - 360 else e
    in
        if en < 22.5 then NewMoon
        else if en < 67.5 then WaxingCrescent
        else if en < 112.5 then FirstQuarter
        else if en < 157.5 then WaxingGibbous
        else if en < 202.5 then FullMoon
        else if en < 247.5 then WaningGibbous
        else if en < 292.5 then LastQuarter
        else if en < 337.5 then WaningCrescent
        else NewMoon

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
        (moonRA1', moonDec1') = calcGeoParallax date1 moonRA1 moonDec1 (Left (Km (a1 * dist1))) lat long height
        (moonRA2', moonDec2') = calcGeoParallax date2 moonRA2 moonDec2 (Left (Km (a2 * dist2))) lat long height
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

-- | Calculate moon rise and set times using Meeus "Astronomical Algorithms" Chapter 15
-- This algorithm uses interpolation of the Moon's position at three times (0h, 12h, 24h)
-- and iteratively refines the rise/set times
-- Reference: Jean Meeus, "Astronomical Algorithms", 2nd Edition, Chapter 15
calcMoonRiseSetMeeus ::
    Date
    -> Lat
    -> Long
    -> Int -- ^ number of iterations for refinement
    -> Maybe (LST, LST) -- ^ (rise time, set time) in hours
calcMoonRiseSetMeeus date lat long numIterations =
    let
        YMD y mo dy = toYMD date
        day0 = YMD y mo (fromIntegral (floor dy))

        -- Calculate Moon's position at three times: 0h, 12h, 24h UT
        (moonLong0, moonLat0, Km dist0) = calcMoonMeeus day0
        (moonLong1, moonLat1, Km dist1) = calcMoonMeeus (YMD y mo (fromIntegral (floor dy) + 0.5))
        (moonLong2, moonLat2, Km dist2) = calcMoonMeeus (YMD y mo (fromIntegral (floor dy) + 1.0))

        -- Calculate obliquity and convert to equatorial coordinates
        tilt = calcObliquityOfEcliptic day0
        (ra0, dec0) = eclToEqu moonLong0 moonLat0 tilt
        (ra1, dec1) = eclToEqu moonLong1 moonLat1 tilt
        (ra2, dec2) = eclToEqu moonLong2 moonLat2 tilt

        -- Extract values in degrees/hours
        Hrs alpha0 = toHrs ra0
        Hrs alpha1 = toHrs ra1
        Hrs alpha2 = toHrs ra2
        Deg delta0 = toDeg dec0
        Deg delta1 = toDeg dec1
        Deg delta2 = toDeg dec2

        -- Handle RA discontinuity at 24h/0h boundary
        alpha1' = if abs (alpha1 - alpha0) > 12
                  then if alpha1 < alpha0 then alpha1 + 24 else alpha1 - 24
                  else alpha1
        alpha2' = if abs (alpha2 - alpha1') > 12
                  then if alpha2 < alpha1' then alpha2 + 24 else alpha2 - 24
                  else alpha2

        -- Observer's latitude in radians
        Lat latAngle latType = lat
        Rad latRadAbs = toRad latAngle
        latRad = case latType of
            N -> latRadAbs
            S -> -latRadAbs

        -- Longitude in hours (east positive)
        Long lonAngle lonType = long
        Deg lonDegAbs = toDeg lonAngle
        lonDeg = case lonType of
            E -> lonDegAbs
            W -> -lonDegAbs
        lonHrs = lonDeg / 15.0

        -- Greenwich mean sidereal time at 0h UT
        JD jd0 = toJD day0
        t = (jd0 - 2451545.0) / 36525.0
        theta0Deg = 280.46061837 + 360.98564736629 * (jd0 - 2451545.0)
                    + 0.000387933 * t * t - t * t * t / 38710000.0
        theta0 = clipAngleDeg theta0Deg / 15.0  -- Convert to hours

        -- Moon's horizontal parallax (average of the three distances)
        avgDist = (dist0 + dist1 + dist2) / 3.0
        -- Parallax in degrees: sin(π) = 6378.14 / distance
        parallaxDeg = asin (6378.14 / avgDist) * 180.0 / pi

        -- Standard altitude for Moon: h0 = 0.7275 * π - 0°34'
        -- where π is the horizontal parallax
        -- The 0.7275 factor accounts for the Moon's average apparent radius
        -- The -34' accounts for atmospheric refraction
        h0 = 0.7275 * parallaxDeg - 34.0 / 60.0

        -- Calculate approximate hour angle at rise/set
        -- cos(H0) = (sin(h0) - sin(φ)sin(δ)) / (cos(φ)cos(δ))
        cosH0 = (sin (h0 * pi / 180.0) - sin latRad * sin (delta1 * pi / 180.0))
                / (cos latRad * cos (delta1 * pi / 180.0))

        -- Check if Moon rises and sets (cosH0 must be between -1 and 1)
        result = if abs cosH0 > 1
            then Nothing
            else
                let
                    h0Hrs = acos cosH0 * 180.0 / pi / 15.0  -- Hour angle in hours

                    -- Approximate transit time (Meeus Eq. 15.2)
                    -- m0 = (α - L - Θ₀) / 24, where L is east-positive longitude in hours
                    m0 = (alpha1' - lonHrs - theta0) / 24.0
                    m0' = m0 - fromIntegral (floor m0)  -- Normalize to [0, 1)
                    m0'' = if m0' < 0 then m0' + 1 else m0'

                    -- Approximate rise and set times
                    m1Init = m0'' - h0Hrs / 24.0
                    m2Init = m0'' + h0Hrs / 24.0

                    -- Normalize to [0, 1)
                    normalize m = let m' = m - fromIntegral (floor m)
                                  in if m' < 0 then m' + 1 else m'

                    -- Iterative refinement
                    refine mVal isRise iter
                        | iter <= 0 = mVal
                        | otherwise =
                            let
                                -- Sidereal time at time m
                                thetaM = theta0 + 360.985647 * mVal / 15.0

                                -- Interpolate RA and Dec at time m using quadratic interpolation
                                -- Positions are at n=0 (0h), n=0.5 (12h), n=1 (24h)
                                -- Lagrange interpolation for points at 0, 0.5, 1:
                                -- L0(n) = (n-0.5)(n-1) / (0.5)  = 2(n-0.5)(n-1)
                                -- L1(n) = n(n-1) / (-0.25)     = -4n(n-1)
                                -- L2(n) = n(n-0.5) / (0.5)      = 2n(n-0.5)
                                n = mVal
                                l0 = 2.0 * (n - 0.5) * (n - 1.0)
                                l1 = -4.0 * n * (n - 1.0)
                                l2 = 2.0 * n * (n - 0.5)
                                alphaM = alpha0 * l0 + alpha1' * l1 + alpha2' * l2

                                deltaM = delta0 * l0 + delta1 * l1 + delta2 * l2

                                -- Local hour angle (H = LST - RA = GST + longitude - RA)
                                hM = thetaM + lonHrs - alphaM
                                hMNorm = hM - 24.0 * fromIntegral (floor (hM / 24.0 + 0.5))

                                -- Altitude at time m
                                sinAlt = sin latRad * sin (deltaM * pi / 180.0)
                                       + cos latRad * cos (deltaM * pi / 180.0) * cos (hMNorm * 15.0 * pi / 180.0)
                                altM = asin sinAlt * 180.0 / pi

                                -- Correction
                                deltaM' = (altM - h0) / (cos (deltaM * pi / 180.0) * cos latRad * sin (hMNorm * 15.0 * pi / 180.0))
                                deltaMHrs = deltaM' / 360.0

                                mNew = mVal + deltaMHrs
                                mNorm = normalize mNew
                            in
                                refine mNorm isRise (iter - 1)

                    -- Refine rise and set times
                    riseM = refine (normalize m1Init) True numIterations
                    setM = refine (normalize m2Init) False numIterations

                    -- Convert from fraction of day to hours
                    riseHrs = riseM * 24.0
                    setHrs = setM * 24.0
                in
                    Just (Hrs riseHrs, Hrs setHrs)
    in
        result