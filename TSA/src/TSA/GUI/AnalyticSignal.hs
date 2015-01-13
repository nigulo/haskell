
module TSA.GUI.AnalyticSignal (analyticSignalDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox

import qualified Regression.Polynom as P
import Regression.AnalyticData as AD
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils
import Regression.FFT
import qualified Math.Function as F

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common

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

analyticSignalDialog :: StateRef -> IO ()
analyticSignalDialog stateRef = do
    state <- readMVar stateRef
    let 
        parms = asParams (params state)
        amplitudeParams = asAmplitudeParams parms
        phaseParams = asPhaseParams parms
        frequencyParams = asFrequencyParams parms
    dialog <- dialogWithTitle state "Analytic signal"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    
    amplitudeEntry <- entryNew
    amplitudeEntry `entrySetText` (getNameWithNo amplitudeParams)
    addWidget (Just "Amplitude name: ") amplitudeEntry dialog

    phaseEntry <- entryNew
    phaseEntry `entrySetText` (getNameWithNo phaseParams)
    addWidget (Just "Phase name: ") phaseEntry dialog

    frequencyEntry <- entryNew
    frequencyEntry `entrySetText` (getNameWithNo frequencyParams)
    addWidget (Just "Frequency name: ") frequencyEntry dialog
    
    realCombo <- dataSetComboNew (\_ -> True) state
    addWidget (Just "Real signal: ") (getComboBox realCombo) dialog

    conjugatedCombo <- dataSetComboNew2 (\_ -> True) state False
    addWidget (Just "Conjugated signal: ") (getComboBox conjugatedCombo) dialog

    precisionAdjustment <- adjustmentNew 65536 1 1048576 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                amplitudeName <- entryGetText amplitudeEntry
                phaseName <- entryGetText phaseEntry
                frequencyName <- entryGetText frequencyEntry
                                
                Just realData <- getSelectedData realCombo
                conjugatedData <- getSelectedData conjugatedCombo
                precision <- spinButtonGetValue precisionSpin
                widgetDestroy dialog

                modifyStateParams stateRef $ \params -> params {asParams = AnalyticSignalParams {
                    asAmplitudeParams = updateCommonParams amplitudeName amplitudeParams,
                    asPhaseParams = updateCommonParams phaseName phaseParams,
                    asFrequencyParams = updateCommonParams frequencyName frequencyParams,
                    asRealData = Just realData,
                    asImagData = conjugatedData                    
                }}
                
                forkIO $ analyticSignal stateRef (round precision) (amplitudeName, phaseName, frequencyName)
                return ()
                
        else
            do
                widgetDestroy dialog

analyticSignal :: StateRef -> Int -> (String, String, String) -> IO ()
analyticSignal stateRef precision (amplitudeName, phaseName, frequencyName) = 
    do
        state <- readMVar stateRef
        g <- getStdGen 
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms
            asParms = asParams (params state)
            rd = subData $ head $ dataSet $ fromJust $ asRealData asParms
            realData = case rd of
                Left s -> s
                Right (Left s) -> sampleAnalyticData_ s [precision] g
                Right (Right f) -> sampleAnalyticData_ f [precision] g
            
            
        imagData <- case asImagData asParms of
            Nothing -> 
                case rd of 
                    Left _ ->
                        fft stateRef (fromJust (asRealData asParms))
                    Right s ->
                        conjugatedCarrierFit stateRef (fromJust (asRealData asParms)) precision
                        
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
        modifyState stateRef $ addDiscreteData amplitude amplitudeName (Just (currentGraphTab, selectedGraph))
        modifyState stateRef $ addDiscreteData phase phaseName (Just (currentGraphTab, selectedGraph))
        --modifyState stateRef $ addData (Left newPhase) (phaseName ++ "_") (Just (currentGraphTab, selectedGraph))
        modifyState stateRef $ addDiscreteData frequency frequencyName (Just (currentGraphTab, selectedGraph))
        putStrLn "Tere siin3"

fft :: StateRef -> DataParams -> IO Data
fft stateRef dataParams = 
    do
        state <- readMVar stateRef
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms

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
        spec <- fromTimeToFrequency (V.toList inerpolatedYs) n 0 (progressUpdate stateRef)
        --let
        --    s1 = map (\((:+) a b) -> (:+) b (-a)) spec
        conjugated <- fromFrequencyToTime spec n (pi / 2) (progressUpdate stateRef) -- actually should be -pi / 2
        
        let
            len = length conjugated
            --specStep = if len == 0 then 0 else 1 / (fromIntegral len * step)

            realSpec1 = D.Spectrum ([(minX, step)], zip (map realPart conjugated) (replicate len 0))
            xs1 = D.xs1 s
            
            realSpec = spectrum1 $ V.zip3 xs1 (interpolatedValues1 xs1 realSpec1) (V.replicate (V.length xs1) 1.0)

        --realSpec <- return $ D.Spectrum (offset, step, zip (map realPart conjugated) (replicate len 0))
        modifyState stateRef $ addDiscreteData realSpec ((dataName dataParams) ++ "_conj") (Just (currentGraphTab, selectedGraph))
        --modifyMVar_ stateRef $ \state -> return $ updateData dataParams {dataSet = Left interpSpec} state

        return realSpec

conjugatedCarrierFit :: StateRef -> DataParams -> Int -> IO Data
conjugatedCarrierFit stateRef dataParams precision = do
    state <- readMVar stateRef
    g <- getStdGen 
    (currentGraphTab, _) <- getCurrentGraphTab state
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms

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
    modifyState stateRef $ addSpline s ((dataName dataParams) ++ "_conj") (Just (currentGraphTab, selectedGraph))
    return $ sampleAnalyticData_ s [precision] g
    
