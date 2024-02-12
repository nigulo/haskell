module Main where

import Data.Time.Clock
import Data.Time.Calendar
import Ephem.Sun
import Ephem.Types
import Ephem.OrbitalElements

date :: IO (Integer, Int, Int) -- :: (year, month, day)
date = getCurrentTime >>= return . toGregorian . utctDay

main :: IO ()
main = do
    (year, month, day) <- date
    let
        srs = sunRisesAndSets year (Just (month, Nothing)) earth1980 (Lat (Deg 58.3780) N) (Long (Deg 26.7290) E) 2
    mapM_ (\((YMD _ _ d), Just (((riseTime, riseAzi), (setTime, setAzi)))) -> 
        putStrLn (show (floor d) ++ ": " ++ hoursToStr riseTime ++ " - " ++ hoursToStr setTime ++ ", " ++ angleToStr riseAzi ++ " - " ++  angleToStr setAzi)) srs
    return ()

hoursToStr :: Hours -> String
hoursToStr (HMS h m s) = show h ++ ":" ++ show m ++ ":" ++ show (round s)
hoursToStr hours = hoursToStr $ toHMS hours

angleToStr :: Angle -> String
angleToStr (DMS d m s) = show d ++ "d" ++ show m ++ "m" ++ show (round s) ++ "s"
angleToStr angle = angleToStr $ toDMS angle
