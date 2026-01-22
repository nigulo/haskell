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
import Ephem.CelestialBody
import Ephem.Types hiding (addDays)
import Ephem.OrbitalElements
import Data.Char (toLower)

main :: IO ()
main = do
    putStrLn "Starting Ephem API on port 3000..."
    scotty 3000 $ do
        -- Add CORS and logging middleware
        middleware simpleCors
        middleware logStdoutDev

        -- Health check endpoint
        get (literal "/") $ do
            text $ TL.pack "Ephem API is running. Use /api/sunrise-sunset endpoint."

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
                    let riseSet = calcSunRiseSet date earth2000 lat lon
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
        get (literal "/api/planet-sunrise-sunset") $ do
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

                    -- Calculate rise and set for each date
                    let results = map (\date ->
                            let riseSet = calcPlanetRiseSet date planetElements earth2000 lat lon
                            in case riseSet of
                                Just ((riseTime, riseAzi), (setTime, setAzi)) ->
                                    let YMD y m d = toYMD date
                                        riseHMS = toHMS (getHours riseTime)
                                        setHMS = toHMS (getHours setTime)
                                        riseAziDeg = case toDeg riseAzi of Deg x -> x
                                        setAziDeg = case toDeg setAzi of Deg x -> x
                                    in object [
                                        Key.fromString "date" .= formatDate (floor d) m y,
                                        Key.fromString "planet" .= planetParam,
                                        Key.fromString "rise" .= formatTime riseHMS,
                                        Key.fromString "set" .= formatTime setHMS,
                                        Key.fromString "riseAzimuth" .= riseAziDeg,
                                        Key.fromString "setAzimuth" .= setAziDeg
                                        ]
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

-- Helper functions
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

-- Map planet name to orbital elements
getPlanetElements :: String -> Maybe OrbitalElements
getPlanetElements name = case map toLower name of
    "mercury" -> Just mercury2000
    "venus"   -> Just venus2000
    "mars"    -> Just mars2000
    "jupiter" -> Just jupiter2000
    "saturn"  -> Just saturn2000
    "uranus"  -> Just uranus2000
    "neptune" -> Just neptune2000
    "pluto"   -> Just pluto2000
    _         -> Nothing

-- List of valid planet names for error messages
validPlanets :: [String]
validPlanets = ["mercury", "venus", "mars", "jupiter", "saturn", "uranus", "neptune", "pluto"]
