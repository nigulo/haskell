module Ephem.CelestialBody (
    calcHelLongAndDist,
    calcEclCoords,
    calcDistance,
    calcAngularDiameter,
    calcPhase,
    calcPositionAngle,
    calcMagnitude,
    calcMeanAnomaly,
    calcEccAnomaly,
    calcTrueAnomaly,
    calcLongitude,
    calcPlanetRiseSet,
    calcPlanetTransit,
    calcRADec
    ) where


import Ephem.Utils
import Ephem.OrbitalElements
import Ephem.Types
import Ephem.Time
import Ephem.Coords
import Debug.Trace

--    pi2 * (angleBy2Pi - fromIntegral (floor angleBy2Pi :: Int)) where
--        angleBy2Pi =  angle / pi2

calcMeanAnomaly :: Either (Double, Double, Double) Double -> Double -> Double
calcMeanAnomaly (Left (daysSinceEpoch, period, longitudeAtEpoch)) longitudeOfPeriapsis =
    clipAngleRad $ pi2 * daysSinceEpoch / (365.2422 * period) + longitudeAtEpoch - longitudeOfPeriapsis
calcMeanAnomaly (Right meanLongitude) longitudeOfPeriapsis =
    clipAngleRad $ meanLongitude - longitudeOfPeriapsis

calcEccAnomaly :: Double -- ^ mean anomaly
    -> Double -- ^ eccentricity
    -> Double
calcEccAnomaly meanAnomaly e =
    let
        startingValue =
            if e > 0.8 && meanAnomaly < pi / 3 || e > 1 then
               let
                     trial = meanAnomaly / (e - 1)
                 in
                   if trial * trial > 6 * (e - 1) then
                       if meanAnomaly < pi then (6 * meanAnomaly) ** (1/3) else asinh (meanAnomaly / e)
                     else
                        trial
            else
                meanAnomaly

        calcEccAnomaly' ea =
            let
                ea1 =
                    if e < 1
                        then
                            meanAnomaly + e * sin ea
                        else
                            ea - (e * sinh ea - ea - meanAnomaly) / (e * cosh ea - 1);
            in
               if abs (ea1 - ea) < 1e-14 then ea1 else calcEccAnomaly' ea1
    in
        calcEccAnomaly' startingValue

calcTrueAnomaly :: Double -> Double -> Double
calcTrueAnomaly eccAnomaly e =
    if e < 1
        then
            2 * atan (sqrt ((1 + e) / (1 - e)) * tan (eccAnomaly / 2))
        else
            2 * atan (sqrt ((e + 1) / (e - 1)) * tanh (eccAnomaly / 2))

calcLongitude :: Double -> Double -> Double
calcLongitude trueAnomaly longitudeOfPeriapsis =
    pi2 * (l' - fromIntegral ((floor l') :: Int)) where
        l' = (trueAnomaly + longitudeOfPeriapsis) / pi2


-- | Calculates heliocentric longitude and distance to the sun for the celestial body
calcHelLongAndDist ::
    OrbitalElements
    -> Date
    -> (Angle, Distance)
calcHelLongAndDist elements date =
    let
        d = diffDays date (epoch elements)
        ctys = d / 36525
        e = calcElement (eccentricity elements) ctys
        Rad omega = toRad $ case periapsisOrientation elements of
            ArgumentOfPeriapsis arg -> (calcAngleElement arg ctys) `add` (calcAngleElement (longitudeOfAscedingNode elements) ctys)
            LongitudeOfPeriapsis lng -> calcAngleElement lng ctys
        (v, r) = case elementsByOrbitType elements of
            orbit@(ClosedOrbitElements _ _) ->
                let
                    meanAnomaly =
                        case epochData elements of
                            LongitudeAtEpoch longitude ->
                                let
                                    Rad l0 = toRad $ calcAngleElement longitude ctys
                                in
                                    calcMeanAnomaly (Left (d, (period orbit), l0)) omega
                            MeanLongitude longitude ->
                                let
                                    Rad l0 = toRad $ calcAngleElement longitude ctys
                                in
                                    calcMeanAnomaly (Right l0) omega
                            PerihelionPassage perihelionPassage ->
                                calcMeanAnomaly (Left (diffDays date perihelionPassage, (period orbit), 0)) 0
                    eccAnomaly = calcEccAnomaly meanAnomaly e
                    v' = calcTrueAnomaly eccAnomaly e
                    (aDist, aRateDist) = semiMajorAxis orbit
                    AU aAU = toAU aDist
                    AU aRateAU = toAU aRateDist
                    a = calcElement (aAU, aRateAU) ctys
                    r' = a * (1 - e * e) / (1 + e * cos v')
                in
                    (v', r')
            OpenOrbitElements q ->
                let
                    PerihelionPassage perihelionPassage = epochData elements
                    d = diffDays date perihelionPassage
                    k = 39.487 -- AU^3/yr^2
                in
                    if e > 1
                        then -- hyperbolic trajectory
                            let
                                a = q / (e - 1)
                                --p = a * (e * e - 1)
                                meanAnomaly = clipAngleRad $ (trace ("d=" ++ show d) d) / 365.2422 * sqrt (k / a) / a
                                eccAnomaly = calcEccAnomaly meanAnomaly e
                                v' = calcTrueAnomaly (trace ("F=" ++ show eccAnomaly) eccAnomaly) e
                                r' = q * (1 + e) / (1 + e * cos v')
                            in
                                (trace ("v=" ++ show v' ++ ", r=" ++ show r') v', r')

                        else -- parabolic trajectory
                            let
                                k1 = 0.0364911624 -- 3 * sqrt (k / 2) / 365.2422
                                w = k1 / (q * sqrt q) * d
                                calcS s =
                                    let
                                        s2 = s * s
                                        s3 = s2 * s
                                        delta = s3 + 3 * s - w
                                    in
                                     if delta < 1e-15 then s else calcS ((2 * s3 + w) / (3 * (s2 + 1)))
                                s' = calcS (w / 3)
                                v' = 2 * atan s'
                                r' = q * (1 + s' * s')
                            in
                                (v', r')
        l = calcLongitude v omega
    in
        (Rad l, AU r)



-- | Calculates the ecliptical longitude and latitude of one celestial body
-- | (planet 1) relative to another (planet 2, usually Earth).
-- | The orbital elements for both planets must be for the same epoch.
calcEclCoords ::
    OrbitalElements -- ^ planet 1
    -> OrbitalElements -- ^ planet 2
    -> (Angle, Distance) -- ^ planet 1 heloicentric longitude and distance
    -> (Angle, Distance) -- ^ planet 2 heliocentric longitude and distance
    -> Date
    -> (EcLong, EcLat)
calcEclCoords p1OrbitalElements p2OrbitalElements (p1HelLong, p1Dist) (p2HelLong, p2Dist) date =
    let
        d = diffDays date (epoch p1OrbitalElements)
        ctys = d / 36525
        Rad p1Incl = toRad $ calcAngleElement (inclination p1OrbitalElements) ctys
        p1LongAsc@(Rad p1LongAscRad) = toRad $ calcAngleElement (longitudeOfAscedingNode p1OrbitalElements) ctys
        sinHelLongMinusLongAsc1 = sine (p1HelLong `subtr` p1LongAsc)
        p1HelLat = asin (sinHelLongMinusLongAsc1 * sin p1Incl)
        l1 = atan2 (sinHelLongMinusLongAsc1 * cos p1Incl) (cosine (p1HelLong `subtr` p1LongAsc)) + p1LongAscRad
        AU p1DistAU = toAU p1Dist
        r1 = p1DistAU  * cos p1HelLat

        Rad p2Incl = toRad $ calcAngleElement (inclination p2OrbitalElements) ctys
        p2LongAsc@(Rad p2LongAscRad) = toRad $ calcAngleElement (longitudeOfAscedingNode p2OrbitalElements) ctys
        sinHelLongMinusLongAsc2 = sine (p2HelLong `subtr` p2LongAsc)
        p2HelLat = asin (sinHelLongMinusLongAsc2 * sin p2Incl)
        l2 = atan2 (sinHelLongMinusLongAsc2 * cos p2Incl) (cosine (p2HelLong `subtr` p2LongAsc)) + p2LongAscRad
        AU p2DistAU = toAU p2Dist
        r2 = p2DistAU * cos p2HelLat


        l1Minusl2 = l1 - l2
        sinl1Minusl2 = sin l1Minusl2
        cosl1Minusl2 = cos l1Minusl2
        pGeoLong' = if r1 > r2
            then
               atan (r2 * sinl1Minusl2 / (r1 - r2 * cosl1Minusl2)) + l1
            else
               pi + l2 + atan (r1 * (-sinl1Minusl2) / (r2 - r1 * cosl1Minusl2))
        pGeoLong = clipAngleRad pGeoLong'
        pGeoLat = atan ((r1 * tan p1HelLat - r2 * tan p2HelLat) * sin (pGeoLong' - l1) / (r2 * sinl1Minusl2))
    in
        (Rad pGeoLong, Rad pGeoLat)

calcDistance ::
    (Angle, Distance) -- ^ planet's heliocentric longitude and distance from sun
    -> (Angle, Distance) -- ^ earth's longitude and distance from sun
    -> Distance
calcDistance (pHelLong, rPlanet) (eHelLong, rEarth) =
    let
        AU rPlanetAU = toAU rPlanet
        AU rEarthAU = toAU rEarth
    in
        AU $ sqrt (rEarthAU * rEarthAU + rPlanetAU * rPlanetAU - 2 * rEarthAU * rPlanetAU * cosine (pHelLong `subtr` eHelLong) )

calcAngularDiameter ::
    Angle -- ^ angular diameter at 1 A.U.
    -> Distance -- ^ distance
    -> Angle
calcAngularDiameter diam (AU dist) =
        diam `mul` (1 / dist)

calcPhase ::
    Angle
    -> Angle
    -> Double
calcPhase pEclLong pGeoLong =
    let
        Rad eclLong = toRad $ pEclLong
        Rad geoLong = toRad $ pGeoLong
    in
        (1 + cos (geoLong - eclLong)) / 2

calcPositionAngle ::
    (RA, Dec) -- ^ sun
    -> (RA, Dec) -- ^ planet
    -> Angle
calcPositionAngle (sunRA, sunDec) (ra, dec) =
    let
        deltaRA = (hoursToAngle sunRA) `subtr` (hoursToAngle ra)
        cosSunDec = cosine sunDec
        y = cosSunDec * sine deltaRA
        x = cosine dec * sine sunDec - sine dec * cosSunDec * cosine deltaRA
        posAngle' = atan2 y x
        posAngle = if posAngle' < 0 then posAngle' + 2 * pi else posAngle'
    in
        Rad posAngle

-- Very approximate
calcMagnitude ::
    Distance -- ^ distance to sun
    -> Distance -- ^ distance to earth
    -> Double -- ^ phase
    -> Double -- ^ albedo
    -> Double
calcMagnitude r q f a =
    let
        AU rAU = toAU r
        AU qAU = toAU q
    in
        5 * logBase 10 (rAU * qAU / (a * sqrt f)) - 26.7

calcRADec ::
    Date
    -> OrbitalElements -- ^ planet
    -> OrbitalElements -- ^ earth
    -> Lat
    -> Long
    -> Bool
    -> (RA, Dec)
calcRADec date planet earth lat long geoParallax =
    let
        earthHelCoords = calcHelLongAndDist earth date
        planetHelCoords = calcHelLongAndDist planet date
        (planetLong, planetLat) = calcEclCoords planet earth planetHelCoords earthHelCoords date
        tilt = calcObliquityOfEcliptic date
        (planetRA, planetDec) = eclToEqu planetLong planetLat tilt
    in
        if geoParallax
        then
            let
                planetDist = calcDistance planetHelCoords earthHelCoords
            in
                calcGeoParallax date planetRA planetDec (Left planetDist) lat long 0
        else (planetRA, planetDec)

calcPlanetRiseSet ::
    Date
    -> OrbitalElements -- ^ planet
    -> OrbitalElements -- ^ earth
    -> Lat
    -> Long
    -> Maybe ((GMT, Angle), (GMT, Angle)) -- ^ (rise time and azimuth, set time and azimuth)
calcPlanetRiseSet date planet earth lat long =
    let
        (planetRA, planetDec) = calcRADec date planet earth lat long False
        maybeRiseSet = calcRiseSet planetRA planetDec lat False
    in
        case maybeRiseSet of
            Just ((lstRise, _), (lstSet, _)) ->
                let
                    gstRise = lstToGST lstRise long
                    gstSet = lstToGST lstSet long
                    gmtRise = gstToGMT gstRise date
                    gmtSet = gstToGMT gstSet date

                    eastWest = case long of
                        Long _ E -> True
                        _ -> False
                    Hrs gmtRiseHrs = getHours gmtRise
                    gmtRise' = if eastWest && gmtRiseHrs > 12 then addDays (-1) gmtRise else gmtRise
                    Hrs gmtSetHrs = getHours gmtSet
                    gmtSet' = if not eastWest && gmtSetHrs < 12 then addDays 1 gmtSet else gmtSet

                    (planetRA1, planetDec1) = calcRADec gmtRise' planet earth lat long True
                    (planetRA2, planetDec2) = calcRADec gmtSet' planet earth lat long True
                in
                    case calcRiseSet planetRA1 planetDec1 lat True of
                        Just ((lstRise1, riseAzi1), _) ->
                            case calcRiseSet planetRA2 planetDec2 lat True of
                                Just (_, (lstSet1, setAzi1)) ->
                                    let
                                        gstRise1 = lstToGST lstRise1 long
                                        gstSet1 = lstToGST lstSet1 long
                                        gmtRise1 = gstToGMT gstRise1 date
                                        Hrs gmtRiseHrs1 = getHours gmtRise1
                                        gmtRise1' = if eastWest && gmtRiseHrs1 > 12 then addDays (1-siderealDayLength) gmtRise1 else gmtRise1
                                        gmtSet1 = gstToGMT gstSet1 date
                                        Hrs gmtSetHrs1 = getHours gmtSet1
                                        gmtSet1' = if not eastWest && gmtSetHrs1 < 12 then addDays (-1+siderealDayLength) gmtSet1 else gmtSet1
                                    in
                                        Just ((gmtRise1', riseAzi1), (gmtSet1', setAzi1))
                                Nothing -> Nothing
                        Nothing -> Nothing
            Nothing -> Nothing

-- | Calculate planet transit (transit) time and altitude
-- Transit is when the planet crosses the meridian (highest point in sky)
-- Azimuth at transit is always ~180° (south) so not returned
calcPlanetTransit ::
    Date
    -> OrbitalElements -- ^ planet
    -> OrbitalElements -- ^ earth
    -> Lat
    -> Long
    -> Maybe (GMT, Alt) -- ^ (transit time, altitude)
calcPlanetTransit date planet earth lat long =
    let
        (planetRA, _) = calcRADec date planet earth lat long False
        gst = lstToGST planetRA long
        gmt = gstToGMT gst date

        (planetRA', planetDec') = calcRADec gmt planet earth lat long True
        gst' = lstToGST planetRA' long
        gmtTransit = gstToGMT gst' date

        eastWest = case long of
            Long _ E -> True
            _ -> False
        --transitHrs = getHours gmtTransit
        --gmtTransit' = if eastWest && gmtRiseHrs > 12 then addDays (-1) gmtRise else gmtRise
        --Hrs gmtSetHrs = getHours gmtSet
        --gmtSet' = if not eastWest && gmtSetHrs < 12 then addDays 1 gmtSet else gmtSet

        -- Adjust day if transit crosses midnight
        --gmtTransit' = if transitHrs >= 24
        --    then addDays 1 gmtTransit
        --    else gmtTransit

        -- At transit, LHA = 0 (object on meridian)
        -- Calculate altitude using equToHor with LHA = 0
        (alt, _) = equToHor (Hrs 0) planetDec' lat
    in
        let
            Deg altDeg = toDeg alt
        in
            if altDeg > 0
            then Just (gmtTransit, alt)
            else Nothing
