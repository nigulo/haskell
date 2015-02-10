module Astro.Ephem.Sun (
    calcSun,
    calcSunRiseSet,
    calcSunRiseSet',
    sunRisesAndSets
    ) where
    
import Astro.Ephem.OrbitalElements
import Astro.Ephem.CelestialBody
import Astro.Ephem.Coords
import Astro.Ephem.Types
import Astro.Ephem.Time
import Astro.Ephem.Utils
import Debug.Trace
import Data.Time.Calendar hiding (diffDays)
import Data.Time.Calendar.MonthDay

-- | Calculates the ecliptical longitude and mean anomaly of the sun
calcSun :: 
    OrbitalElements
    -> Date
    -> (EcLong, Angle) -- ^ longitude and mean anomaly
calcSun elements date = 
    let
        d = diffDays date (epoch elements)
        ctys = d / 36525
        LongitudeOfPeriapsis lp  = periapsisOrientation elements
        Rad omega = Rad pi `add` toRad (calcAngleElement lp ctys)
        meanAnomaly = 
            case epochData elements of
                LongitudeAtEpoch longitude ->
                    let 
                        Rad l0 = Rad pi `add` toRad (calcAngleElement longitude ctys)
                    in
                        calcMeanAnomaly (Left (d, 1, l0)) omega
                MeanLongitude longitude ->
                    let 
                        Rad l0 = Rad pi `add` (toRad (calcAngleElement longitude ctys))
                    in
                        calcMeanAnomaly (Right l0) omega
        e = calcElement (eccentricity elements) ctys
        eccAnomaly = calcEccAnomaly meanAnomaly e
        v = calcTrueAnomaly eccAnomaly e
        l = calcLongitude v omega 
    in
        (Rad l, Rad meanAnomaly)

calcSunRiseSet ::
    Date
    -> OrbitalElements -- ^ earth
    -> Lat
    -> Long
    -> Maybe ((GMT, Angle), (GMT, Angle)) -- ^ (rise time and azimuth, set time and azimuth)   
calcSunRiseSet date earth lat long =
    let
        maybeRiseSet = calcSunRiseSet' date earth lat False
    in
        case maybeRiseSet of
            Just (lstRise, lstSet) ->
                let
                    gstRise = lstToGST lstRise long
                    gstSet = lstToGST lstSet long
                    Hrs gmtRise = toHrs $ gstToGMT gstRise date
                    Hrs gmtSet = toHrs $ gstToGMT gstSet date
                    YMD y m d = toYMD date
                    date1 = YMD y m (fromIntegral (floor d) + gmtRise / 24)
                    tilt1 = calcObliquityOfEcliptic date1
                    (sunLong1, _) = calcSun earth date1
                    date2 = YMD y m (fromIntegral (floor d) + gmtSet / 24)
                    (sunLong2, _) = calcSun earth date2
                    tilt2 = calcObliquityOfEcliptic date2
                    (sunRA1, sunDec1) = eclToEqu sunLong1 (Deg 0) tilt1
                    (sunRA2, sunDec2) = eclToEqu sunLong2 (Deg 0) tilt2
                    sunRadius = Deg (0.533 / 2)
                    horizontalParallax = Sec 8.79 
                    x = sunRadius `add` horizontalParallax `add` (Deg 0.56666666667)
                    (Hrs deltaTRise, Rad deltaARise) = calcRiseSetCorrection sunDec1 lat x
                    (Hrs deltaTSet, Rad deltaASet) = calcRiseSetCorrection sunDec2 lat x
                in
                    case calcRiseSet sunRA1 sunDec1 lat False of
                        Just ((Hrs rise1, Rad riseAzi1), _) ->
                            case calcRiseSet sunRA2 sunDec2 lat False of
                                Just (_, (Hrs set1, Rad setAzi1)) ->
                                    let
                                        lstRise1 = Hrs $ clipHour (rise1 - deltaTRise)
                                        lstSet1 = Hrs $ clipHour (set1 + deltaTSet)
                                        gstRise1 = lstToGST lstRise1 long
                                        gstSet1 = lstToGST lstSet1 long
                                        gmtRise1 = gstToGMT gstRise1 date
                                        gmtSet1 = gstToGMT gstSet1 date
                                    in
                                        Just ((gmtRise1, Rad $ clipAngleRad (riseAzi1 - deltaARise)), (gmtSet1, Rad $ clipAngleRad (setAzi1 + deltaASet)))
                        Nothing -> Nothing
            Nothing -> Nothing

calcSunRiseSet' ::
    Date
    -> OrbitalElements -- ^ earth
    -> Lat
    -> Bool -- ^ use correction
    -> Maybe (LST, LST) -- ^ (rise time, set time)   
calcSunRiseSet' date earth lat useCorrection =
    let
        YMD y m d = toYMD date
        date1 = YMD y m (fromIntegral (floor d)) 
        tilt1 = calcObliquityOfEcliptic date1
        (sunLong1, _) = calcSun earth date1
        date2 = YMD y m ((fromIntegral (floor d)) + 1)
        (sunLong2, _) = calcSun earth date2
        tilt2 = calcObliquityOfEcliptic date2
        (sunRA1, sunDec1) = eclToEqu sunLong1 (Deg 0) tilt1
        --sunLong2 = sunLong `add` Deg 0.985647 
        (sunRA2, sunDec2) = eclToEqu sunLong2 (Deg 0) tilt2
        sunRadius = Deg (0.533 / 2)
        horizontalParallax = Sec 8.79 
        x = sunRadius `add` horizontalParallax `add` (Deg 0.56666666667)
        deltaT = if useCorrection 
            then
                let 
                    (Hrs dT, _) = calcRiseSetCorrection ((sunDec1 `add` sunDec2) `mul` 0.5) lat x
                in
                  dT
            else
                0
        riseSet = case calcRiseSet sunRA1 sunDec1 lat False of
            Just ((Hrs st1r, _), (Hrs st1s, _)) ->
                case calcRiseSet sunRA2 sunDec2 lat False of
                    Just ((Hrs st2r, _), (Hrs st2s, _)) ->
                        let 
                            diffT st1 st2 =
                                if st2 < st1 then 
                                    if st2 + 24 - st1 < st1 - st2 then st1 - st2 - 24 else (st1 - st2)
                                    else if st1 + 24 - st2 < st2 - st1 then (st1 + 24 - st2) else (st1 - st2)
                            -- this stuff here must be reconsidered, the number 12 is chosen arbitrarily, the only constraint is that it must be 
                            -- greather than the maximum possible difference between st1 and st2
                            --(st2r', st1r') = 
                            --    if st2r - st1r < -12 then (st2r + 24, st1r) 
                            --        else if st2r - st1r > 12 then (st2r, st1r + 24)
                            --        else (st2r, st1r)
                            --(st2s', st1s') =
                            --    if st2s - st1s < -12 then (st2s + 24, st1s) 
                            --        else if st2s - st1s > 12 then (st2s, st1s + 24)
                            --        else (st2s, st1s)
                        in
                            Just (Hrs (clipHour (24.07 * st1r / (24.07 + (diffT st1r st2r)) - deltaT)), Hrs (clipHour (24.07 * st1s / (24.07 + (diffT st1s st2s)) + deltaT)))
                    Nothing -> Nothing
            Nothing -> Nothing
    in
        riseSet

sunRisesAndSets :: Integer -- ^ year
    -> Maybe (Int, Maybe Int) -- ^ month, day
    -> OrbitalElements -- ^ earth
    -> Lat -- ^ latitude
    -> Long -- ^ longitude
    -> Int -- ^ time zone
    -> [(Date, Maybe (((Hours, Angle), (Hours, Angle))))] -- ^ sun rise and set
sunRisesAndSets year maybeMonthAndDay elements lat long timeZone =
    let
        leapYear = isLeapYear year
        dates = 
            case maybeMonthAndDay of
                Just (month, maybeDay) -> case maybeDay of
                    Just day -> [YMD year month (fromIntegral day)]
                    Nothing -> map (\day -> YMD year month (fromIntegral day)) [1 .. monthLength leapYear month]
                Nothing -> concatMap (\month -> map (\day -> YMD year month (fromIntegral day)) [1 .. monthLength leapYear month]) [1 .. 12]
        gmtToLTMapOp (date, riseSet) =
            case riseSet of
                Just ((rise, riseAzi), (set, setAzi)) -> (date, Just ((gmtToLT rise timeZone, riseAzi), (gmtToLT set timeZone, setAzi))) 
                Nothing -> (date, Nothing)
        sunRiseSets = map (gmtToLTMapOp) $ map (\date -> (date, calcSunRiseSet date elements lat long)) dates
        
    in
        sunRiseSets

