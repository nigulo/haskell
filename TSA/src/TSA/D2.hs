
module TSA.D2 (
    calcDispersions) where

import Debug.Trace

import Regression.Functions as FS
import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils as U

import TSA.Params
import TSA.Data

import Data.List
import Data.Maybe
import Utils.Misc
import Utils.Concurrent

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import System.IO
import Math.Expression
import qualified Math.Function as F

import System.Random
import qualified Data.Vector.Unboxed as V
import Statistics.Sample
import qualified Math.IODoubleVector as IOV


df = 0.1
ln2 = log 2
lnp = ln2 / df / df

calcDispersions :: DataParams -> Double -> Double -> Double -> Double -> Int -> Int -> String
    -> Bool 
    -> Bool 
    -> TaskEnv
    -> IO Data
calcDispersions dataParams periodStart' periodEnd' minCorrLen' maxCorrLen' method precision name normalize bootstrap taskEnv = do
    let 
        periodStart = min periodStart' periodEnd'
        periodEnd = max periodStart' periodEnd'
        minCorrLen = min minCorrLen' maxCorrLen'
        maxCorrLen = max minCorrLen' maxCorrLen'
        freqStart = 1 / periodEnd
        freqEnd = 1 / periodStart
        step = (freqEnd - freqStart) / (fromIntegral precision)
        puFunc = progressUpdateFunc taskEnv
        Left dat = subData (head (dataSet dataParams))
    bins <- phaseBins dat (periodStart / fromIntegral precision / 2)
    let
        corrLens =
            if minCorrLen == maxCorrLen
                then 
                    V.singleton minCorrLen
                else
                    V.fromList [minCorrLen, minCorrLen + (maxCorrLen - minCorrLen) / fromIntegral precision .. maxCorrLen]

        mapFunc i _ = 
            do 
                let
                    freq = freqStart + fromIntegral i * step
                    disps = d2 bins freq corrLens method
                return (freq, disps)
    freqsDisps <- calcConcurrently mapFunc puFunc (taskInitializer taskEnv) (taskFinalizer taskEnv) [0 .. precision]
    let
        dispersions = V.concat $ zipWith (\corrLen i ->
                let 
                    dispersions = map (\(freq, disps) -> (corrLen, freq, disps V.! i)) freqsDisps
                    normalizedDispersions =  
                        if normalize 
                            then
                                let 
                                    (corrLens, freqs, disps) = unzip3 dispersions 
                                    norm = maximum disps
                            in
                                zip3 corrLens freqs $ map (/ norm) disps 
                            else dispersions
                in
                    V.fromList $ normalizedDispersions
            ) (V.toList corrLens) [0 .. ] 
    puFunc 1
    return $ D.data2' dispersions


phaseBins :: Data -> Double -> IO [(Double, Double)]
phaseBins dat binSize = do
    let
        vals = D.values1 dat
        numBins = round $ (D.xMax1 dat - D.xMin1 dat) / binSize 
    deltay2s <- IOV.vector (replicate numBins 0) 
    mapM_ (\i1 ->
        mapM_ (\i2 -> do
                let
                    (x1, y1, _) = vals V.! i1
                    (x2, y2, _) = vals V.! i2
                    binNo = floor $ (x2 - x1) / binSize
                IOV.elemOpi binNo (+((y2 - y1) ^ 2)) deltay2s
            ) [i1 + 1 .. V.length vals - 1]
        ) [0 .. V.length vals - 2]
    deltay2sList <- IOV.values deltay2s
    return $ zip [binSize * i | i <- [1 ..]] deltay2sList

d2 :: [(Double, Double)] -> Double -> V.Vector Double -> Int -> V.Vector Double
d2 bins freq corrLens method =
    let
        smoothWins = corrLens
        threeSigmas = V.map (* 2.54796540086) smoothWins
        invSmoothWins2 = V.map (\x -> 1 / (x * x)) smoothWins
    in
        V.zipWith3 (\corrLen threeSigma invSmoothWin2 -> 
                    let 
                        (disp, norm) = foldl' (\(disp, norm) (deltax, deltay2) ->
                                let
                                    ln2deltax2 = -ln2 * deltax ^ 2
                                    (n, deltaf') = properFraction $ deltax * freq
                                    deltaf = min (1 - deltaf') deltaf'
                                    lnpdeltaf2 = lnp * deltaf ^ 2
                                in
                                    case method of
                                        0 -> -- Box
                                            if deltax >= corrLen 
                                                then (disp, norm)
                                                else if deltaf < df then (disp + deltay2, norm + 1) else (disp, norm)
                                        1 -> -- Gauss
                                            let
                                                g = exp (ln2deltax2 * invSmoothWin2 - lnpdeltaf2) 
                                            in
                                                if deltax >= threeSigma
                                                    then (disp, norm)
                                                    else if deltaf < df then (disp + g * deltay2, norm + g) else (disp, norm)
                            
                            ) (0, 0) bins
                    in
                        disp / norm
                ) corrLens threeSigmas invSmoothWins2

{-
reshuffle :: Data -> Double -> IO Data
reshuffle dat period = 
    do 
        gen <- createSystemRandom
        let
            df = 0.1
            vals = D.values1 dat
            calcDisp i1 i2 = 
                if i2 >= V.length vals 
                    then []
                    else
                        let
                            (x1, y1, w1) = vals V.! i1
                            (x2, y2, w2) = vals V.! i2
                            deltax = x2 - x1

                            (n, deltaf) = properFraction $ deltax / period
                            vs = calcDisp i1 (i2 + 1)
                        in
                            if 0.5 - abs (0.5 - deltaf) > df then vs else (x1, y1 + (y2 - y1) * w1 / w2, w1):vs
        reshuffled <- V.mapM (\i -> do
                let ys = calcDisp i 0
                r :: Int <- asGenIO (uniform) gen
                return $ ys !! (r `mod` length ys)
            ) (V.fromList [0 .. V.length vals - 1]) 
        
        return $ D.data1 reshuffled
-}
