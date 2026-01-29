module Main where

import Web.Scotty
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, KeyValue(..))
import qualified Data.Aeson.Key as Key
import Data.Time.Clock
import Data.Time.Calendar hiding (diffDays)
import qualified Data.Time.Calendar as Cal
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import qualified Network.Wai as Wai

-- Import Ephem modules
import Ephem.Sun
import Ephem.Moon (calcMoon, calcMoonPhase, calcMoonElongation, getMoonPhaseName, MoonPhase(..), calcMoonDistance, calcMoonAngularDiameter, calcMoonHorizontalParallax, calcMoonRiseSetMeeus)
import Ephem.CelestialBody
import Ephem.Types hiding (addDays)
import Ephem.OrbitalElements
import Ephem.Coords (raToLHA, equToHor)
import Ephem.Time (gmtToGST, gstToLST, calcSpringEquinox, calcAutumnEquinox, calcSummerSolstice, calcWinterSolstice)
import Data.Char (toLower, isDigit)

main :: IO ()
main = do
    putStrLn "Starting Ephem API on port 3000..."
    scotty 3000 $ do
        -- Add CORS and logging middleware
        middleware simpleCors
        middleware logStdoutDev

        -- Health check endpoint
        get (literal "/") $ do
            text $ TL.pack "Ephem API is running. Endpoints: /api/sunrise-sunset, /api/planet-rise-set-transit, /api/celestial-position, /api/moon-phase, /api/equinoxes"

        ----------------------------------------------------------------------------------------------------------------
        -- Sun rise and set
        ----------------------------------------------------------------------------------------------------------------
        get (literal "/api/sunrise-sunset") $ do
            -- Get query parameters from request
            req <- request
            let queryParams = Wai.queryString req
                lookupDouble name def = case lookup name queryParams of
                    Just (Just val) -> case TR.double (TE.decodeUtf8 val) of
                        Right (d, _) -> d
                        Left _ -> def
                    _ -> def
                lookupString name def = case lookup name queryParams of
                    Just (Just val) -> T.unpack $ TE.decodeUtf8 val
                    _ -> def

            let latParam = lookupDouble "lat" 0
                lonParam = lookupDouble "lon" 0
                startDateParam = lookupString "startDate" ""
                endDateParam = lookupString "endDate" ""

            -- Get current date
            (currentYear, currentMonth, currentDay) <- liftIO $ getCurrentTime >>= return . toGregorian . utctDay
            (startYear, startMonth, startDay, endYear, endMonth, endDay) <- liftIO $ dateRange startDateParam endDateParam

            -- Calculate latitude and longitude
            let lat = toLatitude (Deg latParam)
                lon = toLongitude (Deg lonParam)

            -- Generate date range
            let startDate = fromGregorian startYear startMonth startDay
                endDate = fromGregorian endYear endMonth endDay
                dayCount = fromInteger $ Cal.diffDays endDate startDate + 1
                dates = map (\i ->
                    let day = addDays i startDate
                        (y, m, d) = toGregorian day
                    in YMD y m (fromIntegral d)
                    ) [0..dayCount-1]

            -- Calculate sunrise and sunset for each date
            let results = map (\date ->
                    let riseSet = calcSunRiseSet date earth2020 lat lon
                    in case riseSet of
                        Just ((riseTime, riseAzi), (setTime, setAzi)) ->
                            let YMD y m d = toYMD date
                                riseHMS = toHMS (getHours riseTime)
                                setHMS = toHMS (getHours setTime)
                                riseAziDeg = case toDeg riseAzi of Deg x -> x
                                setAziDeg = case toDeg setAzi of Deg x -> x
                            in object [
                                Key.fromString "date" .= formatDate (floor d) m y,
                                Key.fromString "sunrise" .= formatTime riseHMS,
                                Key.fromString "sunset" .= formatTime setHMS,
                                Key.fromString "sunriseAzimuth" .= riseAziDeg,
                                Key.fromString "sunsetAzimuth" .= setAziDeg
                                ]
                        Nothing ->
                            let YMD y m d = toYMD date
                            in object [
                                Key.fromString "date" .= formatDate (floor d) m y,
                                Key.fromString "error" .= ("No sunrise/sunset" :: String)
                                ]
                    ) dates

            -- Return JSON response
            json results

        ----------------------------------------------------------------------------------------------------------------
        -- Planet rise and set
        ----------------------------------------------------------------------------------------------------------------
        get (literal "/api/planet-rise-set-transit") $ do
            -- Get query parameters from request
            req <- request
            let queryParams = Wai.queryString req
                lookupDouble name def = case lookup name queryParams of
                    Just (Just val) -> case TR.double (TE.decodeUtf8 val) of
                        Right (d, _) -> d
                        Left _ -> def
                    _ -> def
                lookupString name def = case lookup name queryParams of
                    Just (Just val) -> T.unpack $ TE.decodeUtf8 val
                    _ -> def

            let latParam = lookupDouble "lat" 0
                lonParam = lookupDouble "lon" 0
                planetParam = lookupString "planet" ""
                startDateParam = lookupString "startDate" ""
                endDateParam = lookupString "endDate" ""

            case getPlanetElements planetParam of
                Nothing -> do
                    json $ object [
                        Key.fromString "error" .= ("Invalid planet name. Valid planets: " ++ show validPlanets :: String)
                        ]
                Just planetElements -> do
                    -- Get date range
                    (startYear, startMonth, startDay, endYear, endMonth, endDay) <- liftIO $ dateRange startDateParam endDateParam

                    -- Calculate latitude and longitude
                    let lat = toLatitude (Deg latParam)
                        lon = toLongitude (Deg lonParam)

                    -- Generate date range
                    let startDate = fromGregorian startYear startMonth startDay
                        endDate = fromGregorian endYear endMonth endDay
                        dayCount = fromInteger $ Cal.diffDays endDate startDate + 1
                        dates = map (\i ->
                            let day = addDays i startDate
                                (y, m, d) = toGregorian day
                            in YMD y m (fromIntegral d)
                            ) [0..dayCount-1]

                    -- Calculate rise, set, and transit for each date
                    let results = map (\date ->
                            let riseSet = calcPlanetRiseSet date planetElements earth2020 lat lon
                            in case riseSet of
                                Just ((riseTime, riseAzi), (setTime, setAzi)) ->
                                    let
                                        transit = calcPlanetTransit date planetElements earth2020 lat lon
                                        YMD y m d = toYMD date
                                        riseHMS = toHMS (getHours riseTime)
                                        setHMS = toHMS (getHours setTime)
                                        riseAziDeg = case toDeg riseAzi of Deg x -> x
                                        setAziDeg = case toDeg setAzi of Deg x -> x
                                        -- Add transit data
                                        transitData = case transit of
                                            Just (transitTime, transitAlt) ->
                                                let transitHMS = toHMS (getHours transitTime)
                                                    transitAltDeg = case toDeg transitAlt of Deg x -> x
                                                in [ Key.fromString "transit" .= formatTime transitHMS
                                                   , Key.fromString "transitAltitude" .= transitAltDeg
                                                   ]
                                            Nothing -> []
                                    in object $ [
                                        Key.fromString "date" .= formatDate (floor d) m y,
                                        Key.fromString "planet" .= planetParam,
                                        Key.fromString "rise" .= formatTime riseHMS,
                                        Key.fromString "set" .= formatTime setHMS,
                                        Key.fromString "riseAzimuth" .= riseAziDeg,
                                        Key.fromString "setAzimuth" .= setAziDeg
                                        ] ++ transitData
                                Nothing ->
                                    let YMD y m d = toYMD date
                                    in object [
                                        Key.fromString "date" .= formatDate (floor d) m y,
                                        Key.fromString "planet" .= planetParam,
                                        Key.fromString "error" .= ("No rise/set" :: String)
                                        ]
                            ) dates

                    -- Return JSON response
                    json results

        ----------------------------------------------------------------------------------------------------------------
        -- Celestial position (RA, Dec, Alt, Azi) for a given datetime
        ----------------------------------------------------------------------------------------------------------------
        get (literal "/api/celestial-position") $ do
            -- Get query parameters from request
            req <- request
            let queryParams = Wai.queryString req
                lookupDouble name def = case lookup name queryParams of
                    Just (Just val) -> case TR.double (TE.decodeUtf8 val) of
                        Right (d, _) -> d
                        Left _ -> def
                    _ -> def
                lookupString name def = case lookup name queryParams of
                    Just (Just val) -> T.unpack $ TE.decodeUtf8 val
                    _ -> def

            let latParam = lookupDouble "lat" 0
                lonParam = lookupDouble "lon" 0
                planetParam = lookupString "planet" ""
                datetimeParam = lookupString "datetime" ""

            case getPlanetElements planetParam of
                Nothing -> do
                    json $ object [
                        Key.fromString "error" .= ("Invalid planet name. Valid planets: " ++ show validPlanets :: String)
                        ]
                Just planetElements -> do
                    -- Parse datetime (format: "yyyy-mm-dd hh:mm")
                    case parseDatetime datetimeParam of
                        Nothing -> do
                            json $ object [
                                Key.fromString "error" .= ("Invalid datetime format. Expected: yyyy-mm-dd hh:mm" :: String)
                                ]
                        Just date -> do
                            -- Calculate latitude and longitude
                            let lat = toLatitude (Deg latParam)
                                lon = toLongitude (Deg lonParam)

                            -- Calculate RA and Dec
                            let (ra, dec) = calcRADec date planetElements earth2020 lat lon True

                            -- Calculate LST from GMT
                            let gst = gmtToGST date
                                lst = gstToLST gst lon

                            -- Calculate LHA from RA and LST
                            let lha = raToLHA ra lst

                            -- Calculate Alt and Azi from LHA, Dec, and Lat
                            let (alt, azi) = equToHor lha dec lat

                            -- Format output
                            let HMS raH raM raS = toHMS ra
                                Deg decDeg = toDeg dec
                                Deg altDeg = toDeg alt
                                Deg aziDeg = toDeg azi

                            json $ object [
                                Key.fromString "datetime" .= datetimeParam,
                                Key.fromString "planet" .= planetParam,
                                Key.fromString "rightAscension" .= formatRA (HMS raH raM raS),
                                Key.fromString "rightAscensionHours" .= (fromIntegral raH + fromIntegral raM / 60 + raS / 3600 :: Double),
                                Key.fromString "declination" .= formatDec dec,
                                Key.fromString "declinationDegrees" .= decDeg,
                                Key.fromString "altitude" .= altDeg,
                                Key.fromString "azimuth" .= aziDeg
                                ]

        ----------------------------------------------------------------------------------------------------------------
        -- Moon phase
        ----------------------------------------------------------------------------------------------------------------
        get (literal "/api/moon-phase") $ do
            -- Get query parameters from request
            req <- request
            let queryParams = Wai.queryString req
                lookupDouble name def = case lookup name queryParams of
                    Just (Just val) -> case TR.double (TE.decodeUtf8 val) of
                        Right (d, _) -> d
                        Left _ -> def
                    _ -> def
                lookupString name def = case lookup name queryParams of
                    Just (Just val) -> T.unpack $ TE.decodeUtf8 val
                    _ -> def

            let latParam = lookupDouble "lat" 0
                lonParam = lookupDouble "lon" 0
                startDateParam = lookupString "startDate" ""
                endDateParam = lookupString "endDate" ""

            -- Get date range
            (startYear, startMonth, startDay, endYear, endMonth, endDay) <- liftIO $ dateRange startDateParam endDateParam

            -- Calculate latitude and longitude
            let lat = toLatitude (Deg latParam)
                lon = toLongitude (Deg lonParam)

            -- Generate date range
            let startDate = fromGregorian startYear startMonth startDay
                endDate = fromGregorian endYear endMonth endDay
                dayCount = fromInteger $ Cal.diffDays endDate startDate + 1
                dates = map (\i ->
                    let day = addDays i startDate
                        (y, m, d) = toGregorian day
                    in YMD y m (fromIntegral d)
                    ) [0..dayCount-1]

            -- Calculate moon phase for each date
            let results = map (\date ->
                    let
                        -- Calculate sun position (needed for moon calculations)
                        (sunLong, sunMean) = calcSun earth2020 date
                        -- Calculate moon position
                        (moonLong, moonLat, mM', moonOrbitalLong) = calcMoon moon2010 (sunLong, sunMean) date
                        -- Calculate moon illumination (0 = new moon, 1 = full moon)
                        illumination = calcMoonPhase sunLong moonOrbitalLong
                        -- Calculate elongation angle (needed for phase name)
                        elongation = calcMoonElongation sunLong moonOrbitalLong
                        Deg elongDeg = toDeg elongation
                        -- Get phase name from elongation
                        phase = getMoonPhaseName elongation
                        phaseName = moonPhaseToString phase
                        -- Calculate moon distance
                        dist = calcMoonDistance moon2010 mM' date
                        -- Calculate angular diameter
                        angDiam = calcMoonAngularDiameter dist
                        Deg angDiamDeg = toDeg angDiam
                        -- Calculate horizontal parallax
                        horizParallax = calcMoonHorizontalParallax dist
                        Deg horizParallaxDeg = toDeg horizParallax
                        -- Calculate moon rise/set times
                        moonRiseSet = calcMoonRiseSetMeeus date lat lon 3
                        YMD y m d = toYMD date
                        Deg moonLongDeg = toDeg moonLong
                        Deg moonLatDeg = toDeg moonLat
                        -- Base moon data
                        baseData = [
                            Key.fromString "date" .= formatDate (floor d) m y,
                            Key.fromString "phaseName" .= phaseName,
                            Key.fromString "illumination" .= (illumination * 100),
                            Key.fromString "elongation" .= elongDeg,
                            Key.fromString "eclipticLongitude" .= moonLongDeg,
                            Key.fromString "eclipticLatitude" .= moonLatDeg,
                            Key.fromString "distance" .= dist,
                            Key.fromString "angularDiameter" .= angDiamDeg,
                            Key.fromString "horizontalParallax" .= horizParallaxDeg
                            ]
                        -- Add rise/set times if available
                        riseSetData = case moonRiseSet of
                            Just (riseTime, setTime) ->
                                let riseHMS = toHMS riseTime
                                    setHMS = toHMS setTime
                                in [ Key.fromString "moonrise" .= formatTime riseHMS
                                   , Key.fromString "moonset" .= formatTime setHMS
                                   ]
                            Nothing -> []
                    in object $ baseData ++ riseSetData
                    ) dates

            -- Return JSON response
            json results

        ----------------------------------------------------------------------------------------------------------------
        -- Equinoxes and Solstices
        ----------------------------------------------------------------------------------------------------------------
        get (literal "/api/equinoxes") $ do
            -- Get query parameters from request
            req <- request
            let queryParams = Wai.queryString req
                lookupInt name def = case lookup name queryParams of
                    Just (Just val) -> case reads (T.unpack $ TE.decodeUtf8 val) of
                        [(i, "")] -> i
                        _ -> def
                    _ -> def

            let yearParam = lookupInt "year" 2025

            -- Calculate equinoxes and solstices for the given year
            let springEquinox = calcSpringEquinox yearParam
                summerSolstice = calcSummerSolstice yearParam
                autumnEquinox = calcAutumnEquinox yearParam
                winterSolstice = calcWinterSolstice yearParam

                -- Convert JD to YMD for JSON output
                formatJD jd =
                    let YMD y m d = toYMD jd
                        dayInt = floor d
                        fracDay = d - fromIntegral dayInt
                        hours = fracDay * 24
                        HMS h mi s = toHMS (Hrs hours)
                    in object [
                        Key.fromString "date" .= formatDate dayInt m y,
                        Key.fromString "time" .= formatTime (HMS h mi s),
                        Key.fromString "jd" .= (case toJD jd of JD j -> j),
                        Key.fromString "dayOfYear" .= dayOfYear y m dayInt
                        ]

            json $ object [
                Key.fromString "year" .= yearParam,
                Key.fromString "springEquinox" .= formatJD springEquinox,
                Key.fromString "summerSolstice" .= formatJD summerSolstice,
                Key.fromString "autumnEquinox" .= formatJD autumnEquinox,
                Key.fromString "winterSolstice" .= formatJD winterSolstice
                ]

-- Helper functions

-- | Calculate day of year (1-366)
dayOfYear :: Integer -> Int -> Int -> Int
dayOfYear year month day =
    let daysInMonth = [31, if isLeapYear' year then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in sum (take (month - 1) daysInMonth) + day

-- | Check if year is a leap year
isLeapYear' :: Integer -> Bool
isLeapYear' y = (y `mod` 4 == 0) && (y `mod` 100 /= 0 || y `mod` 400 == 0)

formatTime :: Hours -> String
formatTime (HMS h m s) =
    pad2 h ++ ":" ++ pad2 m ++ ":" ++ pad2 (round s :: Int)
formatTime hrs = formatTime $ toHMS hrs

formatDate :: Int -> Int -> Integer -> String
formatDate d m y =
    show y ++ "-" ++ pad2 m ++ "-" ++ pad2 d

pad2 :: (Show a, Integral a) => a -> String
pad2 x = if x < 10 then "0" ++ show x else show x

dateRange :: String -> String -> IO (Integer, Int, Int, Integer, Int, Int)
dateRange startDateParam endDateParam =
    do
        (currentYear, currentMonth, currentDay) <- liftIO $ getCurrentTime >>= return . toGregorian . utctDay
        return $
            if null startDateParam || null endDateParam
            then -- Use current month if dates not provided
                let monthLength = gregorianMonthLength currentYear currentMonth
                in (currentYear, currentMonth, 1, currentYear, currentMonth, monthLength)
            else -- Parse provided dates (format: YYYY-MM-DD)
                let parseDate dateStr =
                        case reads dateStr of
                            [(y, rest1)] ->
                                case rest1 of
                                    ('-':rest2) ->
                                        case reads rest2 of
                                            [(m, rest3)] ->
                                                case rest3 of
                                                    ('-':rest4) ->
                                                        case reads rest4 of
                                                            [(d, _)] -> (y, m, d)
                                                            _ -> (currentYear, currentMonth, fromIntegral currentDay)
                                                    _ -> (currentYear, currentMonth, fromIntegral currentDay)
                                            _ -> (currentYear, currentMonth, fromIntegral currentDay)
                                    _ -> (currentYear, currentMonth, fromIntegral currentDay)
                            _ -> (currentYear, currentMonth, fromIntegral currentDay)
                    (sy, sm, sd) = parseDate startDateParam
                    (ey, em, ed) = parseDate endDateParam
                in (sy, sm, sd, ey, em, ed)

-- Map planet name to orbital elements (using J2020 elements from JPL Horizons DE441)
getPlanetElements :: String -> Maybe OrbitalElements
getPlanetElements name = case map toLower name of
    "mercury" -> Just mercury2020
    "venus"   -> Just venus2020
    "mars"    -> Just mars2020
    "jupiter" -> Just jupiter2020
    "saturn"  -> Just saturn2020
    "uranus"  -> Just uranus2020
    "neptune" -> Just neptune2020
    "pluto"   -> Just pluto2020
    _         -> Nothing

-- List of valid planet names for error messages
validPlanets :: [String]
validPlanets = ["mercury", "venus", "mars", "jupiter", "saturn", "uranus", "neptune", "pluto"]

-- Convert MoonPhase to String for JSON output
moonPhaseToString :: MoonPhase -> String
moonPhaseToString NewMoon        = "New Moon"
moonPhaseToString WaxingCrescent = "Waxing Crescent"
moonPhaseToString FirstQuarter   = "First Quarter"
moonPhaseToString WaxingGibbous  = "Waxing Gibbous"
moonPhaseToString FullMoon       = "Full Moon"
moonPhaseToString WaningGibbous  = "Waning Gibbous"
moonPhaseToString LastQuarter    = "Last Quarter"
moonPhaseToString WaningCrescent = "Waning Crescent"

-- Parse datetime string "yyyy-mm-dd hh:mm" into a Date with fractional day
parseDatetime :: String -> Maybe Date
parseDatetime str =
    case break (== ' ') str of
        (dateStr, ' ':timeStr) ->
            case parseDatePart dateStr of
                Just (y, m, d) ->
                    case parseTimePart timeStr of
                        Just (h, mins) ->
                            let fractionalDay = fromIntegral d + (fromIntegral h + fromIntegral mins / 60) / 24
                            in Just $ YMD y m fractionalDay
                        Nothing -> Nothing
                Nothing -> Nothing
        _ -> Nothing
  where
    parseDatePart :: String -> Maybe (Integer, Int, Int)
    parseDatePart s =
        case break (== '-') s of
            (yStr, '-':rest1) ->
                case break (== '-') rest1 of
                    (mStr, '-':dStr) ->
                        case (reads yStr, reads mStr, reads dStr) of
                            ([(y, "")], [(m, "")], [(d, "")]) -> Just (y, m, d)
                            _ -> Nothing
                    _ -> Nothing
            _ -> Nothing

    parseTimePart :: String -> Maybe (Int, Int)
    parseTimePart s =
        case break (== ':') s of
            (hStr, ':':mStr) ->
                case (reads hStr, reads (takeWhile isDigit mStr)) of
                    ([(h, "")], [(m, "")]) -> Just (h, m)
                    _ -> Nothing
            _ -> Nothing

-- Format RA as "XXh XXm XX.Xs"
formatRA :: Hours -> String
formatRA (HMS h m s) = show h ++ "h " ++ pad2 m ++ "m " ++ show (round s :: Int) ++ "s"
formatRA hrs = formatRA $ toHMS hrs

-- Format Dec as "+/-XX° XX' XX\""
formatDec :: Angle -> String
formatDec angle =
    let Deg d = toDeg angle
        sign = if d >= 0 then "+" else "-"
        DMS deg mins secs = toDMS (Deg (abs d))
    in sign ++ show deg ++ "° " ++ pad2 mins ++ "' " ++ show (round secs :: Int) ++ "\""
