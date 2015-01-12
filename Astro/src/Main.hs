module Main where

import Data.Time.Calendar
import Astro.Ephem.Types

main :: IO ()
main = do
    print $ toJD (YMD 2000 1 0.5)
    print $ atan2 1 (-1)
    print $ atan2 (-1) (-1)
    print $ atan2 (-1) 1
    