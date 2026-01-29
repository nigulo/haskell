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
    siderealDayLength,
    -- Equinox and Solstice calculations (Meeus algorithm)
    calcSpringEquinox,
    calcAutumnEquinox,
    calcSummerSolstice,
    calcWinterSolstice,
    SeasonEvent(..)
    ) where
import Debug.Trace
import Ephem.Types
import Ephem.Utils
import Data.Time.Calendar as Cal

-- | Seasonal event type
data SeasonEvent = SpringEquinox | SummerSolstice | AutumnEquinox | WinterSolstice
    deriving (Show, Eq)

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

--------------------------------------------------------------------------------
-- Equinox and Solstice Calculations (Meeus Algorithm)
-- Based on Jean Meeus "Astronomical Algorithms", Chapter 27
--------------------------------------------------------------------------------

-- | Calculate the Julian Ephemeris Day of the spring (March/vernal) equinox
-- for a given year. Returns JDE in Dynamical Time.
calcSpringEquinox :: Integer -> Date
calcSpringEquinox year = calcSeasonEvent SpringEquinox year

-- | Calculate the Julian Ephemeris Day of the autumn (September) equinox
-- for a given year. Returns JDE in Dynamical Time.
calcAutumnEquinox :: Integer -> Date
calcAutumnEquinox year = calcSeasonEvent AutumnEquinox year

-- | Calculate the Julian Ephemeris Day of the summer (June) solstice
-- for a given year. Returns JDE in Dynamical Time.
calcSummerSolstice :: Integer -> Date
calcSummerSolstice year = calcSeasonEvent SummerSolstice year

-- | Calculate the Julian Ephemeris Day of the winter (December) solstice
-- for a given year. Returns JDE in Dynamical Time.
calcWinterSolstice :: Integer -> Date
calcWinterSolstice year = calcSeasonEvent WinterSolstice year

-- | Calculate the Julian Ephemeris Day for a seasonal event
calcSeasonEvent :: SeasonEvent -> Integer -> Date
calcSeasonEvent event year =
    let
        -- Step 1: Calculate mean equinox/solstice JDE0
        jde0 = calcMeanSeasonJDE event year

        -- Step 2: Calculate T (Julian centuries from J2000.0)
        t = (jde0 - 2451545.0) / 36525.0

        -- Step 3: Calculate W (degrees)
        w = degToRad (35999.373 * t - 2.47)

        -- Step 4: Calculate Δλ (delta lambda)
        deltaLambda = 1 + 0.0334 * cos w + 0.0007 * cos (2 * w)

        -- Step 5: Sum the 24 periodic terms
        s = sumPeriodicTerms t

        -- Step 6: Final JDE
        jde = jde0 + (0.00001 * s) / deltaLambda
    in
        JD jde

-- | Calculate mean equinox/solstice JDE0 using polynomial coefficients
-- Coefficients from Meeus Table 27.a (years 1000-3000) and Table 27.b (years -1000 to 1000)
calcMeanSeasonJDE :: SeasonEvent -> Integer -> Double
calcMeanSeasonJDE event year
    | year >= 1000 = calcMeanSeasonJDE_1000_3000 event y
    | otherwise    = calcMeanSeasonJDE_neg1000_1000 event y
  where
    y = fromIntegral year

-- | Polynomial coefficients for years 1000 to 3000 (Table 27.a)
calcMeanSeasonJDE_1000_3000 :: SeasonEvent -> Double -> Double
calcMeanSeasonJDE_1000_3000 event year =
    let y = (year - 2000) / 1000
    in case event of
        SpringEquinox   -> 2451623.80984 + 365242.37404 * y + 0.05169 * y^2 - 0.00411 * y^3 - 0.00057 * y^4
        SummerSolstice  -> 2451716.56767 + 365241.62603 * y + 0.00325 * y^2 + 0.00888 * y^3 - 0.00030 * y^4
        AutumnEquinox   -> 2451810.21715 + 365242.01767 * y - 0.11575 * y^2 + 0.00337 * y^3 + 0.00078 * y^4
        WinterSolstice  -> 2451900.05952 + 365242.74049 * y - 0.06223 * y^2 - 0.00823 * y^3 + 0.00032 * y^4

-- | Polynomial coefficients for years -1000 to 1000 (Table 27.b)
calcMeanSeasonJDE_neg1000_1000 :: SeasonEvent -> Double -> Double
calcMeanSeasonJDE_neg1000_1000 event year =
    let y = year / 1000
    in case event of
        SpringEquinox   -> 1721139.29189 + 365242.13740 * y + 0.06134 * y^2 + 0.00111 * y^3 - 0.00071 * y^4
        SummerSolstice  -> 1721233.25401 + 365241.72562 * y - 0.05323 * y^2 + 0.00907 * y^3 + 0.00025 * y^4
        AutumnEquinox   -> 1721325.70455 + 365242.49558 * y - 0.11677 * y^2 - 0.00297 * y^3 + 0.00074 * y^4
        WinterSolstice  -> 1721414.39987 + 365242.88257 * y - 0.00769 * y^2 - 0.00933 * y^3 - 0.00006 * y^4

-- | Sum of 24 periodic terms (Table 27.c)
-- Each term is: A * cos(B + C*T) where T is Julian centuries from J2000.0
sumPeriodicTerms :: Double -> Double
sumPeriodicTerms t = sum $ map calcTerm periodicTerms
  where
    calcTerm (a, b, c) = a * cos (degToRad (b + c * t))

-- | The 24 periodic terms from Meeus Table 27.c
-- Format: (A, B, C) where the term is A * cos(B + C*T)
periodicTerms :: [(Double, Double, Double)]
periodicTerms =
    [ (485, 324.96,   1934.136)
    , (203, 337.23,  32964.467)
    , (199, 342.08,     20.186)
    , (182,  27.85, 445267.112)
    , (156,  73.14,  45036.886)
    , (136, 171.52,  22518.443)
    , ( 77, 222.54,  65928.934)
    , ( 74, 296.72,   3034.906)
    , ( 70, 243.58,   9037.513)
    , ( 58, 119.81,  33718.147)
    , ( 52, 297.17,    150.678)
    , ( 50,  21.02,   2281.226)
    , ( 45, 247.54,  29929.562)
    , ( 44, 325.15,  31555.956)
    , ( 29,  60.93,   4443.417)
    , ( 18, 155.12,  67555.328)
    , ( 17, 288.79,   4562.452)
    , ( 16, 198.04,  62894.029)
    , ( 14, 199.76,  31436.921)
    , ( 12,  95.39,  14577.848)
    , ( 12, 287.11,  31931.756)
    , ( 12, 320.81,  34777.259)
    , (  9, 227.73,   1222.114)
    , (  8,  15.45,  16859.074)
    ]
