
module Utils.Math (
    factorial, 
    combination, 
    epsilonRound
    ) where

factorial :: Integral a => a -> a
factorial n = product [1 .. n]


combination :: (Integral a) => a -> a -> a
combination k n = (product [k + 1 .. n]) `div` (factorial (n - k))

epsilonRound x = 
    fromIntegral (round (x * 10e10)) / 10e10

