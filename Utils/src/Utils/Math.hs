
module Utils.Math (
    factorial, 
    combination, 
    epsilonRound
    ) where

import Numeric.SpecFunctions

combination :: Int -> Int -> Double
combination k n = fromIntegral (product [k + 1 .. n]) / factorial (n - k)

epsilonRound x = 
    fromIntegral (round (x * 10e10)) / 10e10

