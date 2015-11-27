
module TSA.D2 (
    calcDispersions,
    calcDispersions',
    Method (..),
    getBinSize,
    phaseBins,
    bootstrapBins) where

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
import Math.Expression hiding (Gauss)
import qualified Math.Function as F

import qualified Data.Vector.Unboxed as V
import Statistics.Sample
import qualified Math.IODoubleVector as IOV
import System.Random.MWC

--df = 0.1
ln2 = log 2
numPhaseBins = 50

getBinSize :: Double -> Double
getBinSize freqEnd = freqEnd / numPhaseBins

data Method = Box | Gauss deriving (Show, Read)

calcDispersions :: Data -> Double -> Double -> Double -> Double -> Method -> Int -> String
    -> Bool -> Double
    -> TaskEnv
    -> IO Data
calcDispersions dat freqStart' freqEnd' minCorrLen' maxCorrLen' method precision name normalize df taskEnv = do
    let
        freqStart = min freqStart' freqEnd'
        freqEnd = max freqStart' freqEnd'
        minCorrLen = min minCorrLen' maxCorrLen'
        maxCorrLen = max minCorrLen' maxCorrLen'
    bins <- phaseBins dat (getBinSize freqEnd)
    calcDispersions' bins freqStart freqEnd minCorrLen maxCorrLen method precision name normalize df taskEnv

calcDispersions' :: V.Vector (Double, Double) -> Double -> Double -> Double -> Double -> Method -> Int -> String
    -> Bool -> Double
    -> TaskEnv
    -> IO Data
calcDispersions' bins freqStart freqEnd minCorrLen maxCorrLen method precision name normalize df taskEnv = do
    let 
        freqStep = (freqEnd - freqStart) / (fromIntegral precision)
        puFunc = progressUpdateFunc taskEnv
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
                    disps = map (\freq -> d2 method bins corrLen freq df) freqs 
                    normalizedDisps =  
                        if normalize
                            then
                                let 
                                    maxDisp = maximum disps
                                    minDisp = minimum disps
                                    norm = maxDisp - minDisp
                            in
                                if maxDisp == minDisp
                                    then
                                        disps
                                    else
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

phaseBins :: Data -> Double -> IO (V.Vector (Double, Double))
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
    return $ V.fromList $ zip [binSize * fromIntegral i | i <- [1 ..]] deltay2sList


data MethodParams = BoxParams Double | GaussParams Double Double 

getMethodParams :: Method -> Double -> MethodParams
getMethodParams Box corrLen = BoxParams corrLen
getMethodParams Gauss corrLen = GaussParams (corrLen * 2.54796540086) (1 / (corrLen * corrLen))

d2 :: Method -> V.Vector (Double, Double) -> Double -> Double -> Double -> Double
d2 method bins corrLen freq df =
    let
        methodParams = getMethodParams method corrLen
        lnp = ln2 / df / df
        (disp, norm) = V.foldl' (\(disp, norm) (deltax, deltay2) ->
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
                                --g = exp (ln2deltax2 * invSmoothWin2 - lnpdeltaf2) 
                                g = (0.5 * cos (pi * deltaf / df) + 0.5) * exp (ln2deltax2 * invSmoothWin2) 
                            in
                                if deltax >= threeSigma
                                    then (disp, norm)
                                    else if deltaf < df then (disp + g * deltay2, norm + g) else (disp, norm)
            
            ) (0, 0) bins
    in
        disp / norm

bootstrapBins :: Method -> V.Vector (Double, Double) -> Double -> Double -> Double -> IO (V.Vector (Double, Double))
bootstrapBins method bins corrLen freq df = 
    do 
        gen <- createSystemRandom
        let
            methodParams = getMethodParams method corrLen
            lnp = ln2 / df / df
            closeInPhase :: V.Vector (Double, Double) -> IO (V.Vector (Double, Double))
            closeInPhase bins 
              | V.null bins = return V.empty
              | otherwise = do
                    let
                        (deltax, deltay2) = V.head bins
                        ln2deltax2 = -ln2 * deltax ^ 2
                        (n, deltaf') = properFraction $ deltax * freq
                        deltaf = min (1 - deltaf') deltaf'
                        lnpdeltaf2 = lnp * deltaf ^ 2
                    vs <- closeInPhase (V.tail bins)
                    case methodParams of
                        BoxParams corrLen -> 
                            if deltax >= corrLen 
                                then return vs
                                else if deltaf < df 
                                    then return $ V.cons (deltax, deltay2) vs 
                                    else return vs
                        GaussParams threeSigma invSmoothWin2 -> do
                            let
                                g = exp (ln2deltax2 * invSmoothWin2 - lnpdeltaf2) 
                            if deltax >= threeSigma
                                then return vs
                                else do 
                                    r :: Double <- asGenIO (uniform) gen
                                    if deltaf < df && r < g 
                                        then return $ V.cons (deltax, deltay2) vs 
                                        else return vs
        bins1 <- closeInPhase bins
        V.mapM (\i -> do
                r :: Int <- asGenIO (uniform) gen
                return $ bins1 V.! (r `mod` V.length bins1)
            ) (V.fromList [0 .. V.length bins - 1]) 

