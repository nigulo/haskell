module Astro.Ephem.Time (
    GMT (..),
    GST (..),
    LST (..),
    LT (..),
    gmtToLT,
    gmtToGST,
    calcB, -- don't use this function direclty (exposed only for testing purposes)
    gstToGMT,
    gstToLST,
    lstToGST
    ) where
import Debug.Trace
import Astro.Ephem.Types
import Astro.Ephem.Utils
import Data.Time.Calendar as Cal

type GMT = Hours
type GST = Hours
type LST = Hours
type LT = Hours
    

gmtToLT :: GMT -> Int -> LT
gmtToLT gmt tz =
    let
        Hrs gmtHrs = toHrs gmt
    in
        Hrs (clipHour (gmtHrs + fromIntegral tz))

gmtToGST :: GMT -> Date -> GST
gmtToGST hours date =
    let
        ymd@(YMD y m _) = toYMD date
        a = 0.0657098
        b = calcB y
        c = 1.002738
        daysSinceJan0 = Cal.diffDays (fromGregorian y m (getDayOfMonth ymd)) (fromGregorian (y - 1) 12 31)
        t0 = fromIntegral daysSinceJan0 * a - b
        Hrs hrs = toHrs hours
        (t1, t1Fract) = properFraction (hrs * c + t0 + 24)
        gst = fromIntegral (t1 `mod` 24) + t1Fract  -- add 24 if < 0, subtract 24 if > 24
    in
        Hrs gst
        
calcB year = 
    let
        JD jd =  toJD $ YMD (year - 1) 12 31
        s = jd - 2415020
        t = s / 36525
        r = 6.6460656 + (2400.051262 + 0.00002581 * t) * t
        u = r - 24 * (fromIntegral year - 1900)
        b = 24 - u
    in
        b  

gstToGMT :: GST -> Date -> GMT
gstToGMT hours date =
    let 
        ymd@(YMD y m _) = toYMD date
        a = 0.0657098
        b = calcB y
        d = 0.997270
        daysSinceJan0 = Cal.diffDays (fromGregorian y m (getDayOfMonth ymd)) (fromGregorian (y - 1) 12 31)
        t0 = fromIntegral daysSinceJan0 * a - b
        t1 = t0 + (if t0 < 0 then 24 else 0)
        Hrs hrs = toHrs hours
        t2 = hrs - t1
        t3 = t2 + (if t2 < 0 then 24 else 0)
        gmt = t3 * d
    in
        Hrs gmt

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