
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

calcDispersions :: DataParams -> Double -> Double -> Double -> Double -> Int -> Int -> String -> Bool 
    -> TaskEnv
    -> IO [(Double, DataParams)]
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

        findDispersions :: Double -> (Double, [(Double, Double)]) -> Either D.Data (Either S.Spline FS.Functions) -> (Double -> IO ()) -> IO (Either D.Data (Either S.Spline FS.Functions), [(Double, Double)]) 
        findDispersions corrLen (prevCorrLen, info) (Left dat) puFunc = do
            let
                mapFunc i puFunc = 
                    do
                        dispersionAndInfo <-
                            do 
                                let
                                    (prevDisp, prevNorm) = info !! i
                                    freq = freqStart + fromIntegral i * step
                                --dat1 <- if bootstrap then reshuffle dat freq else return dat
                                (disp, norm) <- d2' dat freq corrLen prevCorrLen method
                                putStrLn $ "corrLen freq disp norm: " ++ show corrLen ++ " " ++ show freq ++ " " ++ show (disp + prevDisp) ++ " " ++ show (norm + prevNorm) 
                                return ((disp + prevDisp) / (norm + prevNorm) / 2 / (variance (D.ys dat)), (disp + prevDisp, norm + prevNorm))
                                --disp <- d2 dat period corrLen 0
                                --return (disp, (0, 0))
                        --puFunc $ (fromIntegral i) / (fromIntegral precision)
                        return dispersionAndInfo
            dispersionAndInfo <- calcConcurrently mapFunc puFunc (taskInitializer taskEnv) (taskFinalizer taskEnv) [0 .. precision]
            let
                (dispersions, info) = unzip dispersionAndInfo
                norm = maximum dispersions
                normalizedDispersions =  
                    if False -- norm > 0 
                        then map (\d -> d / norm) dispersions 
                        else dispersions
                freqSpec = D.Spectrum2 ((freqStart, step), V.zip (V.fromList normalizedDispersions) (V.replicate (length normalizedDispersions) 1))
            return (Left freqSpec, info)
    let
        namesCorrLenghts =
            if minCorrLen == maxCorrLen
                then 
                    [(name, minCorrLen)]
                else
                    zipWith (\i corrLen -> (name ++ (show i), corrLen)) [1 ..] [minCorrLen, minCorrLen + (maxCorrLen - minCorrLen) / fromIntegral precision .. maxCorrLen]
        corrLenFunc :: [(String, Double)] -> Double -> [[(Double, Double)]] -> IO [(Double, DataParams)]
        corrLenFunc [] _ _ = return []
        corrLenFunc ((name, corrLen):namesCorrLenghts) prevCorrLen prevInfos = do
            subDispersionsAndInfo <- mapM (\(sdp, prevInfo) -> do
                    let
                        Left dat = subData sdp
                    dat1 <- if bootstrap then reshuffleData dat else return dat
                    findDispersions corrLen (prevCorrLen, prevInfo) (Left dat1) puFunc
                ) (zip (dataSet dataParams) prevInfos)
            let
                (subDispersions, infos) = unzip subDispersionsAndInfo
                dispersions = createDataParams_ name (map (\sd -> createSubDataParams__ sd) subDispersions)
                    
            bestPeriods <- mapM (\(i, SubDataParams _ (Left periodSpec) _) -> do
                    let
                        freqDisps = sortBy (\(_, disp1) (_, disp2) -> compare disp1 disp2) $ V.toList $ D.getMinima periodSpec
                        freqs = map fst freqDisps
                    --logFunc ("Possible freqs for " ++ name ++ "[" ++ show i ++ "] = " ++ (show freqs))
                    case freqDisps of 
                         [] -> return Nothing
                         ((freq, disp):_) -> return $ Just (1 / freq, (1 - disp) ^ 2)  
                ) (zip [1, 2 ..] (dataSet dispersions))
            let
                periodMean = meanWeighted $ V.fromList $ catMaybes bestPeriods
                periodVar = varianceWeighted $ V.fromList $ catMaybes bestPeriods
                periodDisp = mean $ V.map (\(p, d) -> 1 - sqrt d) $ V.fromList $ catMaybes bestPeriods
                
            (logFunc taskEnv) ("Period mean stdev disp: " ++ show periodMean ++ " " ++ show periodVar ++ " " ++ show periodDisp)
            next <- corrLenFunc namesCorrLenghts corrLen infos
            return $ (corrLen, dispersions):next
    dispersions <- corrLenFunc namesCorrLenghts 0 (repeat (repeat (0, 0)))
    puFunc 0
    return dispersions

d2 :: Data -> Double -> Double -> Int -> IO Double
d2 dat freq corrLen method =  
    do 
        (disp, norm) <- d2' dat freq corrLen 0 method
        putStrLn $ "corrLen freq disp norm: " ++ show corrLen ++ " " ++ show freq ++ " " ++ show disp ++ " " ++ show norm 
        return $ disp / norm / 2 / (variance (D.ys dat))

df = 0.1
ln2 = log 2
lnp = ln2 / df / df

d2' :: Data -> Double -> Double -> Double -> Int -> IO (Double, Double)
d2' dat freq corrLen prevCorrLen method =
    do 
        let
            smoothWin = corrLen
            threeSigma = 2.54796540086 * smoothWin
            invSmoothWin = 1 / smoothWin
            vals = D.values1 dat
            calcDisp i1 i2 = 
                if i1 >= V.length vals - 1
                    then (0, 0)
                    else if i2 >= V.length vals then calcDisp (i1 + 1) (i1 + 2)
                    else
                        let
                            (x1, y1, _) = vals V.! i1
                            (x2, y2, _) = vals V.! i2
                            deltax = x2 - x1
                        in  
                         if deltax < prevCorrLen
                             then calcDisp i1 (i2 + 1)
                             else
                                let
                                    (n, deltaf') = properFraction $ deltax * freq
                                    (disp, norm) = calcDisp i1 (i2 + 1)
                                    deltaf = min (1 - deltaf') deltaf'
                                in
                                    case method of
                                        0 -> -- Box
                                            if deltax >= corrLen 
                                                then calcDisp (i1 + 1) (i1 + 2)
                                                else if deltaf < df then (disp + ((y2 - y1) ^ 2), norm + 1) else (disp, norm)
                                        1 -> -- Gauss
                                            let
                                                g = exp (-ln2 * (deltax * invSmoothWin) ^ 2 - lnp * deltaf ^ 2) 
                                            in
                                                if deltax >= threeSigma
                                                    then calcDisp (i1 + 1) (i1 + 2)
                                                    else if deltaf < df then (disp + g * ((y2 - y1) ^ 2), norm + g) else (disp, norm)
            (disp, norm) = calcDisp 0 1
        return $ (disp, norm)

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
