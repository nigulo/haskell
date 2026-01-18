module Ephem.Time (
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
import Ephem.Types
import Ephem.Utils
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

-- TODO Replace with Meeus "Astronomical Algorithms" formula
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


-- | Convert Greenwich Sidereal Time to Greenwich Mean Time
-- This is the inverse of gmtToGST, using the same calcB formula for consistency.
-- Based on the algorithm from "Practical Astronomy with your Calculator" by Duffett-Smith.
--
-- Key fix: At day boundaries (when siderealDiff is close to 0), the traditional algorithm
-- has a ~4 minute discontinuity because it adds 24 sidereal hours (= 23.93 solar hours).
-- For boundary cases only, we convert to solar time first, then add 24 solar hours.
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
        t2 = hrs - t1  -- Sidereal difference (can be negative)
        -- Boundary detection: if |t2| < 1 hour, we're near midnight
        nearBoundary = abs t2 < 1.0 || abs (t2 + 24) < 1.0 || abs (t2 - 24) < 1.0
        gmt
            | nearBoundary =
                -- Near boundary: convert to solar FIRST, then add 24 solar hours if needed
                let gmtRaw = t2 * d
                in if gmtRaw < 0 then gmtRaw + 24 else if gmtRaw >= 24 then gmtRaw - 24 else gmtRaw
            | t2 < 0 =
                -- Far from boundary, negative: traditional algorithm (add 24 sidereal, then convert)
                (t2 + 24) * d
            | otherwise =
                -- Positive: no wrapping needed
                t2 * d
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