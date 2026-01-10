module Main where

import Ephem.Sun
import Ephem.Types
import Ephem.OrbitalElements
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    if length args < 1 
        then 
            putStrLn "Usage: Sun latitude longitude timezone year [month] [day]"
        else
            return ()
    let
        lat = read $ head args
        long = read $ args !! 1
        timeZone = read $ args !! 2
        year = read $ args !! 3
        maybeMonthAndDay = if length args > 4 
            then 
                Just (read $ args !! 4, 
                    if length args > 5 
                        then 
                            Just (read $ args !! 5) 
                        else 
                            Nothing
                    ) 
            else Nothing
        sunRiseSets = sunRisesAndSets year maybeMonthAndDay earth2000 (toLatitude (Deg lat)) (toLongitude (Deg long)) timeZone
        
        formatRiseSet (date, maybeRiseSet) = 
            let
                YMD y m d = toYMD date
                riseSet = case maybeRiseSet of
                    Just ((rise, _), (set, _)) -> 
                        let
                            HMS hr mr sr = toHMS rise 
                            HMS hs ms ss = toHMS set
                        in
                            show hr ++ ":" ++ show mr ++ ":" ++ show (round sr) ++ " " ++ show hs ++ ":" ++ show ms ++ ":" ++ show (round ss) 
                    Nothing -> ""
            in
                show y ++ "-" ++ show m ++ "-" ++ show (floor d) ++ " " ++ riseSet ++ "\n"
    putStrLn $ "Date Rise Set"
    putStrLn $ concatMap (formatRiseSet) sunRiseSets
