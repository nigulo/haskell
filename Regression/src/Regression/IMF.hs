
-- Module for calculating intrinsic mode functions
module Regression.IMF (envelopeMean, imfIO) where

import Regression.Data as D
import Regression.Polynom as P
import Regression.Spline as S
import Regression.AnalyticData as AD
import Regression.Regression as R
import Regression.Utils as U

import Control.Concurrent
import qualified Data.Vector.Unboxed as V

envelopeMean :: Int -> Int -> Double -> Data -> IO Spline
envelopeMean rank knots sdev dat = 
    do
        upper <- R.envelope True rank knots sdev False dat (\_ -> return ()) (\_ -> return ()) (\_ -> return ())
        lower <- R.envelope False rank knots sdev False dat (\_ -> return ()) (\_ -> return ()) (\_ -> return ())
        return $ (upper `S.add` lower) `S.divide` 2

{-
imf :: Int -> Int -> Double -> Data -> Data
imf rank knots sdev dat = 
    let 
        dat2 = D.setY (zipWith (-) (D.ys dat) vals) dat where
            vals = S.getValues (D.xs dat) (envelopeMean rank knots sdev dat)
    in 
        if D.stdev dat dat2 < sdev then dat2
        else imf rank knots sdev dat2
-}
imfIO :: Int -> Int -> Int -> Int -> Double -> Data -> IO Data
imfIO upperRank upperKnots lowerRank lowerKnots sdev dat =
    do
        envMean <- envelopeMean upperRank upperKnots sdev dat
        let 
            dat2 = D.setY (V.zipWith (-) (D.ys dat) (V.fromList envMeanVals)) dat where
                envMeanVals = AD.getValues_ (D.xs dat) envMean
                    
                    -- ((R.envelope True upperRank upperKnots sdev dat) `S.add` (R.envelope False lowerRank lowerKnots sdev dat)) `S.divide` 2
            -- xRange = (D.xMax dat - D.xMin dat) / 3
            -- x1 = D.xMin dat + xRange
            -- x2 = D.xMax dat - xRange
            -- sdev2 = D.stdev (subSet (x1, x2) dat) (subSet (x1, x2) dat2)
            sdev2 = U.stdev dat (Left dat2)
                    
        putStrLn $ "sdev=" ++ show sdev2 ++ ", needed=" ++ show sdev
                    
        if sdev2 < sdev
            then 
                    return dat2
            else
                do
                    --threadDelay (100 * 1000)
                    imfIO upperRank upperKnots lowerRank lowerKnots sdev dat2

