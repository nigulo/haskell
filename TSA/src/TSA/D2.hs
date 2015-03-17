
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

df = 0.1
ln2 = log 2
lnp = ln2 / df / df

calcDispersions :: DataParams -> Double -> Double -> Double -> Double -> Int -> Int -> String -> Bool 
    -> TaskEnv
    -> IO Data
calcDispersions dataParams periodStart' periodEnd' minCorrLen' maxCorrLen' method precision name bootstrap taskEnv = do
    let 
        periodStart = min periodStart' periodEnd'
        periodEnd = max periodStart' periodEnd'
        minCorrLen = min minCorrLen' maxCorrLen'
        maxCorrLen = max minCorrLen' maxCorrLen'
        freqStart = 1 / periodEnd
        freqEnd = 1 / periodStart
        step = (freqEnd - freqStart) / (fromIntegral precision)
        puFunc = progressUpdateFunc taskEnv

        findDispersions :: V.Vector Double -> Either D.Data (Either S.Spline FS.Functions) -> (Double -> IO ()) -> IO (V.Vector (Double, Double, Double)) 
        findDispersions corrLens (Left dat) puFunc = do
            let
                mapFunc i puFunc = 
                    do 
                        let
                            freq = freqStart + fromIntegral i * step
                        disps <- d2 dat freq corrLens method
                        putStrLn $ "freq corrLen disp: " ++ show freq ++ " " ++ show (V.zip corrLens disps) 
                        return (freq, disps)
            freqsDisps <- calcConcurrently mapFunc puFunc (taskInitializer taskEnv) (taskFinalizer taskEnv) [0 .. precision]

            return $ V.concat $ zipWith (\corrLen i ->
                    let 
                        dispersions = map (\(freq, disps) -> (corrLen, freq, disps V.! i)) freqsDisps
                        normalizedDispersions =  
                            if False -- norm > 0 
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
    let
        corrLens =
            if minCorrLen == maxCorrLen
                then 
                    V.singleton minCorrLen
                else
                    V.fromList [minCorrLen, minCorrLen + (maxCorrLen - minCorrLen) / fromIntegral precision .. maxCorrLen]
    dispersions <- findDispersions corrLens (subData (head (dataSet dataParams))) puFunc
    puFunc 1
    return $ D.data2' dispersions

d2 :: Data -> Double -> V.Vector Double -> Int -> IO (V.Vector Double)
d2 dat freq corrLens method =
    do 
        let
            smoothWins = corrLens
            threeSigmas = V.map (* 2.54796540086) smoothWins
            invSmoothWins2 = V.map (\x -> 1 / (x * x)) smoothWins
            vals = D.values1 dat
            calcDisp i1 i2 dispsNorms = 
                if i1 >= V.length vals - 1
                    then dispsNorms
                    else if i2 >= V.length vals 
                        then calcDisp (i1 + 1) (i1 + 2) dispsNorms
                        else
                            let
                                (x1, y1, _) = vals V.! i1
                                (x2, y2, _) = vals V.! i2
                                deltax = x2 - x1
                                ln2deltax2 = -ln2 * deltax ^ 2
                                deltay2 = (y2 - y1) ^ 2
                                (n, deltaf') = properFraction $ deltax * freq
                                deltaf = min (1 - deltaf') deltaf'
                                lnpdeltaf2 = lnp * deltaf ^ 2
                            in
                                calcDisp i1 (i2 + 1) $ V.zipWith4 (\corrLen threeSigma invSmoothWin2 (disp, norm) -> 
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
                                    ) corrLens threeSigmas invSmoothWins2 dispsNorms
            dispsNorms = calcDisp 0 1 (V.replicate (V.length corrLens) (0, 0))
        return $ V.map (\(disp, norm) -> disp / norm) dispsNorms

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
