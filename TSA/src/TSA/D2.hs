
module TSA.D2 (
    calcDispersions,
    Method (..),
    bootstrapData) where

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

import qualified Data.Vector.Unboxed as V
import Statistics.Sample
import qualified Math.IODoubleVector as IOV
import System.Random.MWC

df = 0.1
ln2 = log 2
lnp = ln2 / df / df
numPhaseBins = 50

data Method = Box | Gauss deriving (Show, Read)

calcDispersions :: Data -> Double -> Double -> Double -> Double -> Method -> Int -> String
    -> Bool 
    -> TaskEnv
    -> IO Data
calcDispersions dat freqStart' freqEnd' minCorrLen' maxCorrLen' method precision name normalize taskEnv = do
    let 
        freqStart = min freqStart' freqEnd'
        freqEnd = max freqStart' freqEnd'
        minCorrLen = min minCorrLen' maxCorrLen'
        maxCorrLen = max minCorrLen' maxCorrLen'
        freqStep = (freqEnd - freqStart) / (fromIntegral precision)
        puFunc = progressUpdateFunc taskEnv
    bins <- phaseBins dat (freqEnd / numPhaseBins)
    let
        corrLens =
            if minCorrLen == maxCorrLen
                then 
                    [minCorrLen]
                else
                    [minCorrLen, minCorrLen + (maxCorrLen - minCorrLen) / fromIntegral precision .. maxCorrLen]
        freqs =
            if freqStart == freqEnd
                then 
                    [freqStart]
                else
                    [freqStart + fromIntegral i * freqStep | i <- [0 .. precision]]
        dispFunc corrLen _ = 
            do 
                let
                    disps = map (d2 method bins corrLen) freqs 
                    normalizedDisps =  
                        if normalize 
                            then
                                let 
                                    maxDisp = maximum disps
                                    minDisp = minimum disps
                                    norm = maxDisp - minDisp
                            in
                                map (\disp -> (disp - minDisp) / norm) disps 
                            else disps
                return (corrLen, zip freqs normalizedDisps)
    disps <- calcConcurrently dispFunc puFunc (taskInitializer taskEnv) (taskFinalizer taskEnv) corrLens
    let
        dispersions = concatMap (\(corrLen, freqsDisps) -> map (\(freq, disp) -> (corrLen, freq, disp)) freqsDisps) disps
    puFunc 1
    case corrLens of
        [_] -> 
            return $ D.data1' $ V.fromList $ map (\(_, freq, disp) -> (freq, disp)) dispersions
        otherwise ->
            return $ D.data2' $ V.fromList dispersions


phaseBins :: Data -> Double -> IO [(Double, Double)]
phaseBins dat binSize = do
    let
        vals = D.values1 dat
        numBins = ceiling $ (D.xMax1 dat - D.xMin1 dat) / binSize 
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
    return $ zip [binSize * fromIntegral i | i <- [1 ..]] deltay2sList


data MethodParams = BoxParams Double | GaussParams Double Double

getMethodParams :: Method -> Double -> MethodParams
getMethodParams Box corrLen = BoxParams corrLen
getMethodParams Gauss corrLen = GaussParams (corrLen * 2.54796540086) (1 / (corrLen * corrLen))

d2 :: Method -> [(Double, Double)] -> Double -> Double -> Double
d2 method bins corrLen freq =
    let
        methodParams = getMethodParams method corrLen
        (disp, norm) = foldl' (\(disp, norm) (deltax, deltay2) ->
                let
                    ln2deltax2 = -ln2 * deltax ^ 2
                    (n, deltaf') = properFraction $ deltax * freq
                    deltaf = min (1 - deltaf') deltaf'
                    lnpdeltaf2 = lnp * deltaf ^ 2
                in
                    case methodParams of
                        BoxParams corrLen ->
                            if deltax >= corrLen 
                                then (disp, norm)
                                else if deltaf < df then (disp + deltay2, norm + 1) else (disp, norm)
                        GaussParams threeSigma invSmoothWin2 ->
                            let
                                g = exp (ln2deltax2 * invSmoothWin2 - lnpdeltaf2) 
                            in
                                if deltax >= threeSigma
                                    then (disp, norm)
                                    else if deltaf < df then (disp + g * deltay2, norm + g) else (disp, norm)
            
            ) (0, 0) bins
    in
        disp / norm

bootstrapData :: Method -> Data -> Double -> Double -> IO Data
bootstrapData method dat corrLen freq = 
    do 
        gen <- createSystemRandom
        let
            methodParams = getMethodParams method corrLen
            vals = D.values1 dat
            calcDisp i1 i2 = 
                if i2 >= V.length vals 
                    then return V.empty
                    else do
                        let
                            (x1, y1, w1) = vals V.! i1
                            (x2, y2, w2) = vals V.! i2
                            deltax = x2 - x1
                            ln2deltax2 = -ln2 * deltax ^ 2
                            (n, deltaf') = properFraction $ deltax * freq
                            deltaf = min (1 - deltaf') deltaf'
                            lnpdeltaf2 = lnp * deltaf ^ 2
                        vs <- calcDisp i1 (i2 + 1)
                        case methodParams of
                            BoxParams corrLen -> 
                                if deltax >= corrLen 
                                    then return vs
                                    else if deltaf < df 
                                        then return $ V.cons (x1, y2, w1) vs 
                                        else return vs
                            GaussParams threeSigma invSmoothWin2 -> do
                                let
                                    g = exp (ln2deltax2 * invSmoothWin2 - lnpdeltaf2) 
                                if deltax >= threeSigma
                                    then return vs
                                    else do 
                                        r :: Double <- asGenIO (uniform) gen
                                        if deltaf < df && r < g 
                                            then return $ V.cons (x1, y2, w1) vs 
                                            else return vs
        reshuffled <- V.mapM (\i -> do
                ys <- calcDisp i 0
                r :: Int <- asGenIO (uniform) gen
                return $ ys V.! (r `mod` V.length ys)
            ) (V.fromList [0 .. V.length vals - 1]) 
        
        return $ D.data1 reshuffled

