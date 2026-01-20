module Ephem.Time (
    GMT (..),
    GST (..),
    LST (..),
    LT (..),
    gmtToLT,
    gmtToGST,
    gstToGMT,
    gstToLST,
    lstToGST,
    siderealDayLength
    ) where
import Debug.Trace
import Ephem.Types
import Ephem.Utils
import Data.Time.Calendar as Cal

type GMT = Date
type GST = Hours
type LST = Hours
type LT = Hours
    

gmtToLT :: GMT -> Int -> LT
gmtToLT gmt tz =
    let
        (_, Hrs gmtHrs) = splitDayAndTime gmt
    in
        Hrs (clipHour (gmtHrs + fromIntegral tz))

gmtToGST :: GMT -> GST
gmtToGST gmt =
    let (day, Hrs time) = splitDayAndTime gmt
        Hrs t = solarSiderealTimesDiff day
        time' = clipHour $ time/siderealDayLength + t
    in
        Hrs time'

gstToGMT :: GST -> Date -> Date
gstToGMT gst date =
    let
        Hrs hours = toHrs gst
        (day, time) = dayTime date hours
        Hrs t = solarSiderealTimesDiff (JD day)
        deltaT = clipHour $ time - t
        time' = deltaT * siderealDayLength
    in
        JD $ day + time'/24
    where
        dayTime date hours
            | hours < 0   = (day-1, hours+24)
            | hours >= 24 = (day+1, hours-24)
            | otherwise = (day, hours)
            where (JD day, _) = splitDayAndTime date

siderealDayLength = hours / 24
    where Hrs hours = toHrs $ HMS 23 56 4.0916

solarSiderealTimesDiff :: Date -> Hours
solarSiderealTimesDiff d =
    let
        t = numCenturies j2000 d
    in
        Hrs $ clipHour $ 6.697374558 + 2400.051336*t + 0.000025862*t*t

gstToLST :: GST -> Long -> LST
gstToLST hours (Long longitude ew) =
    let
        Hrs gstHrs = toHrs hours
        Hrs longHrs = toHrs $ angleToHours longitude
        t0 = 
            case ew of
                E -> gstHrs + longHrs
                W -> gstHrs - longHrs
        (t1, t1Fract) = properFraction (t0 + 24)
        lst = fromIntegral (t1 `mod` 24) + t1Fract  -- add 24 if < 0, subtract 24 if > 24
    in
        Hrs lst
        
lstToGST :: LST -> Long -> GST
lstToGST hours (Long longitude ew) =
    let
        Hrs lstHrs = toHrs hours
        Hrs longHrs = toHrs $ angleToHours longitude
        t0 = 
            case ew of
                E -> lstHrs - longHrs
                W -> lstHrs + longHrs
        (t1, t1Fract) = properFraction (t0 + 24)
        gst = fromIntegral (t1 `mod` 24) + t1Fract  -- add 24 if < 0, subtract 24 if > 24
    in
        Hrs gst