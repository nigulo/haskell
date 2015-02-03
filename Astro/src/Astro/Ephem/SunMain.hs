module Main where

import Astro.Ephem.Sun
import Astro.Ephem.Types
import Astro.Ephem.OrbitalElements
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
    print sunRiseSets