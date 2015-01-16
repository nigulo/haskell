
module TSA.AnalyticSignal (analyticSignal) where

import qualified Regression.Polynom as P
import Regression.AnalyticData as AD
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils
import Regression.FFT
import qualified Math.Function as F

import TSA.Params

import Utils.Misc

import Data.IORef
import Data.Maybe
import Data.Complex
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative

import Debug.Trace

import Math.Expression
import System.Random
import qualified Data.Vector.Unboxed as V

analyticSignal :: (Eq id) => AnalyticSignalParams -> Int -> (id, id, id, id) -> ProgressUpdateFunc -> LogFunc -> DataUpdateFunc id -> IO ()
analyticSignal asParms precision (amplitudeId, phaseId, frequencyId, conjId) puFunc logFunc duf@(DataUpdateFunc dataUpdateFunc) = 
    do
        g <- getStdGen 
        let
            dataParams = fromJust (asRealData asParms)
            rd = subData $ head $ dataSet $ dataParams
            realData = case rd of
                Left s -> s
                Right (Left s) -> sampleAnalyticData_ s [precision] g
                Right (Right f) -> sampleAnalyticData_ f [precision] g
            
        imagData <- case asImagData asParms of
            Nothing -> 
                case rd of 
                    Left _ -> do
                        fft puFunc duf dataParams conjId
                    Right s ->
                        conjugatedCarrierFit duf dataParams precision conjId
                        
            Just imag -> return $ 
                case subData $ head $ dataSet imag of
                    Left s -> s
                    Right (Left s) -> sampleAnalyticData_ s [precision] g
                    Right (Right f) -> sampleAnalyticData_ f [precision] g
            --Just (realSpec@(Spectrum (offset, step, _))) = fftRealData fftParms
            --reals = D.ys realSpec

        let            
            amplitudeOp = F.function "sqrt(u*u+v*v)"
            phaseOp = F.function "atan(v/u)"

            Left amplitude = binaryOp amplitudeOp (Left realData) (Left imagData) True g
            Left phase = binaryOp phaseOp (Left realData) (Left imagData) True g
            
            
            Left frequency = binaryOp (F.divide) (binaryOp (F.subtr) v'u u'v True g) u2v2 True g where
                u' = D.getTangent realData
                v' = D.getTangent imagData
                v'u = binaryOp (F.mult) (Left v') (Left realData) True g
                u'v = binaryOp (F.mult) (Left u') (Left imagData) True g
                u2v2 = binaryOp (F.function "u*u + v*v") (Left realData) (Left imagData) True g
            
            --frequency = D.getTangent phase
            
            phaseVals@(val0:_) = V.toList $ D.values1 phase
            
            -- phase difference curve
            newVals = val0:(func 0 phaseVals) where 
                func n [(x0, y0, w0), (x1, y1, w1)] = [(x0, y0 + n * pi, w0), (x1, y1 + n * pi, w1)]
                func n ((x0, y0, w0):(x1, y1, w1):(x2, y2, w2):vals) =
                    if y1 - y0 < 0 -- && y1 - y2 <= 0 
                        then (x1, y1 + (n + 1) * pi, w1):func (n + 1) ((x1, y1, w1):(x2, y2, w2):vals)
                    --else if y1 - y0 < 0
                    --    then (x1, (n + 1) * pi / 2, w1):func n ((x1, y1, w1):(x2, y2, w2):vals)
                    else (x1, y1 + n * pi, w1):func n ((x1, y1, w1):(x2, y2, w2):vals)
            (x0, y0, _) = head newVals
            (xn, yn, _) = last newVals
            slope = (yn - y0) / (xn - x0)
            primeVals = map (\(x, y, w) -> y0 + slope * (x - x0)) newVals

            diffVals = zipWith (\(x0, y0, w0) y1 -> (x0, y0 - y1, w0)) newVals primeVals
            newPhase = spectrum1 $ V.fromList diffVals
        dataUpdateFunc (Left amplitude) amplitudeId False
        dataUpdateFunc (Left phase) phaseId False
        dataUpdateFunc (Left frequency) frequencyId False
        putStrLn "Tere siin3"

fft :: (Eq id) => ProgressUpdateFunc -> DataUpdateFunc id -> DataParams -> id -> IO Data
fft puFunc (DataUpdateFunc dataUpdateFunc) dataParams id = 
    do
        let
            Left s = subData $ head $ dataSet dataParams
            --Just s@(Spectrum(_, vals)) = D.toSpectrum d
            --s@(Spectrum(offset, step, vals)) = left $ dataSet dataParams
            minX = D.xMin1 s
            maxX = D.xMax1 s
            rangeX = maxX - minX
            numSamples = 2 ^^ (ceiling (logBase 2 (fromIntegral (D.dataLength s))))
            step = rangeX / (numSamples - 1)
            xs = [minX + step * i | i <- [0 .. numSamples - 1]]
            ys = interpolatedValues1 (V.fromList xs) s
            inerpolatedYs = V.map (\r -> (:+) r 0) ys
            n = trace ("inerpolatedYs length: " ++ show (inerpolatedYs)) V.length inerpolatedYs --values
--            interpSpec = spectrum1 $ zip3 xs ys (replicate n 1.0)
--            n = length vals
--            numToAdd = 2 ^ (ceiling (logBase 2 (fromIntegral n))) - n
--            ys1 = trace ("numValues: " ++ show (numToAdd + n)) $ map (\r -> (:+) r 0) ((ys s) ++ replicate numToAdd 0)
        
--        s <- F.fromTimeToFrequency ys1 (n + numToAdd) 0
--        conjugated <- F.fromFrequencyToTime s (n + numToAdd) (-pi / 2)
        spec <- fromTimeToFrequency (V.toList inerpolatedYs) n 0 puFunc
        --let
        --    s1 = map (\((:+) a b) -> (:+) b (-a)) spec
        conjugated <- fromFrequencyToTime spec n (pi / 2) puFunc -- actually should be -pi / 2
        
        let
            len = length conjugated
            --specStep = if len == 0 then 0 else 1 / (fromIntegral len * step)

            realSpec1 = D.Spectrum ([(minX, step)], zip (map realPart conjugated) (replicate len 0))
            xs1 = D.xs1 s
            
            realSpec = spectrum1 $ V.zip3 xs1 (interpolatedValues1 xs1 realSpec1) (V.replicate (V.length xs1) 1.0)

        dataUpdateFunc (Left realSpec) id False
        return realSpec

conjugatedCarrierFit :: (Eq id) => DataUpdateFunc id -> DataParams -> Int -> id -> IO Data
conjugatedCarrierFit (DataUpdateFunc dataUpdateFunc) dataParams precision id = do
    g <- getStdGen 
    let 
        sdp = head $ dataSet dataParams
        Right (Left (AnalyticData pols)) = subData sdp
        s = AnalyticData (map (\(xMin, xMax, pol)-> (xMin, xMax, swapFuncAndDeriv pol)) pols) where
        swapFuncAndDeriv pol = 
            let 
                funcs =  P.getModulators pol
                derivs = P.getModulatorDerivs pol
                newFuncs = zipWith (\f d -> 
                    case f of 
                        Nothing -> Nothing
                        Just f ->
                            let
                                func = F.initialExpression f
                            in
                            case d of 
                                    Nothing -> Nothing
                                    Just deriv -> 
                                            let fn = F.setVarValue ("i", 1) (F.function ("sgn(-(" ++ F.initialExpression deriv ++ "))*sqrt(1-(" ++ func ++ ")^2)"))
                                            in Just ({-trace ("fn=" ++ show fn)-} fn)
                    ) funcs derivs 
            in
                P.setModulators newFuncs pol
    dataUpdateFunc (Right (Left s)) id False
    return $ sampleAnalyticData_ s [precision] g
    
