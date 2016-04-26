
module TSA.GUI.FFT where


import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox

import Regression.Data as D
import Regression.FFT
import Regression.Utils as U
import Math.Function as F

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget

import Utils.Misc

import Data.IORef
import Data.Complex
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Applicative
import System.Random

paramsDialog :: StateRef -> IO ()
paramsDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = fftParams (params state)
        commonParams = fftCommonParams parms

    dialog <- dialogWithTitle state "Fast fourier transform"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fftButton <- dialogAddButton dialog "Ok" ResponseOk

    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    
    fftNameEntry <- entryNew
    fftNameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "FFT name: ") fftNameEntry dialog
    
    directionCombo <- createComboBox ["Time -> Frequency", "Frequency -> Time"]
    comboBoxSetActive directionCombo 0
    if fftDirection parms then comboBoxSetActive directionCombo 0 else comboBoxSetActive directionCombo 1
    addWidget (Just "Direction: ") directionCombo dialog
    
    realSpectrumCombo <- dataSetComboNew2 onlySpectrum state False
    addWidget (Just "Real spectrum: ") (getComboBox realSpectrumCombo) dialog

    imagSpectrumCombo <- dataSetComboNew2 onlySpectrum state False
    addWidget (Just "Imag spectrum: ") (getComboBox imagSpectrumCombo) dialog

    phaseShiftAdjustment <- adjustmentNew (fftPhaseShift parms) (-1) 1 0.1 0.1 1
    phaseShiftSpin <- spinButtonNew phaseShiftAdjustment 1 2
    addWidget (Just "Phase shift: ") phaseShiftSpin dialog
    
    infoLabel <- labelNew $ Just "Number of samples will be truncated to closest power of 2"
    addWidget Nothing infoLabel dialog

    let 
        toggleFftButton :: IO ()
        toggleFftButton = 
            do
                --selectedRealSpec <- getSelectedData realSpectrumCombo
                --numSamples <- spinButtonGetValue numSamplesSpin
                fftName <- entryGetString fftNameEntry
                sensitivity <- 
                    if length fftName <= 0 -- || fromIntegral (round (logBase numSamples 2)) /= (logBase numSamples 2)
                        then return False 
                        else return True
                fftButton `widgetSetSensitivity` sensitivity
                        
--    (getComboBox realSpectrumCombo) `onChanged` toggleFftButton
--    (castToEditable numSamplesSpin) `onEditableChanged` toggleFftButton
    on (castToEditable fftNameEntry) editableChanged toggleFftButton
    
    
    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString fftNameEntry
                direction <- comboBoxGetActiveString directionCombo
                
                selectedRealSpec <- getSelectedData realSpectrumCombo
                selectedImagSpec <- getSelectedData imagSpectrumCombo
                phaseShift <- spinButtonGetValue phaseShiftSpin
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {fftParams = FftParams {
                    fftCommonParams = updateCommonParams name commonParams,
                    fftDirection = (direction == Just "Time -> Frequency"),
                    fftPhaseShift = phaseShift,
                    fftRealData = case selectedRealSpec of 
                        Nothing -> Nothing
                        Just s -> Just $ left $ unboxSubData $ subData $ head $ dataSet s,
                    fftImagData = case selectedImagSpec of 
                        Nothing -> Nothing
                        Just s -> Just $ left $ unboxSubData $ subData $ head $ dataSet s
                }}
                
                forkOS $ fft stateRef name
                return ()
        else
            do
                widgetDestroy dialog
                --return state

fft :: StateRef -> String -> IO ()
fft stateRef name = 
    do
        state <- readMVar stateRef
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms

            parms = fftParams (params state)
            --Just (realSpec@(Spectrum (offset, step, _))) = fftRealData parms
            --reals = D.ys realSpec
            
            
            numSamples dat defVal = maybe defVal (V.length . D.xs1) $ dat
            
            n1 = numSamples (fftRealData parms) (numSamples (fftImagData parms) 0)
            n2 = numSamples (fftImagData parms) (numSamples (fftRealData parms) 0)
            n = min n1 n2

            (reals, realStep) = case fftRealData parms of 
                Nothing -> (V.replicate n 0, 0)
                Just s@(Spectrum2 ((_, step), _)) -> (D.ys s, step)
            (imags, imagStep) = case fftImagData parms of 
                Nothing -> (V.replicate n 0, 0)
                Just s@(Spectrum2 ((_, step), _)) -> (D.ys s, step)
            step = max realStep imagStep
            fftFunc = if fftDirection parms then fromTimeToFrequency else fromFrequencyToTime
            phaseShift = fftPhaseShift parms
            numToUse = 2 ^ (floor (logBase 2 (fromIntegral n)))
            ys1 = V.take numToUse $ V.zipWith (:+) reals imags
        
        spec1 <- fftFunc ys1 phaseShift
        let
            len = V.length spec1
            specStep = if len == 0 then 0 else 1 / (fromIntegral len * step)
        realSpec <- return $ D.Spectrum2 ((0, specStep), V.zip (V.map realPart spec1) (V.replicate len 0))
        imagSpec <- return $ D.Spectrum2 ((0, specStep), V.zip (V.map imagPart spec1) (V.replicate len 0))
        
        --modifyState stateRef $ addDiscreteData realSpec (name ++ "_Real") (Just (currentGraphTab, selectedGraph))
        --modifyState stateRef $ addDiscreteData imagSpec (name ++ "_Imag") (Just (currentGraphTab, selectedGraph))
        g <- getStdGen 
        let
            powerSpec = U.dataToDataOp (F.function "sqrt(x*x + y*y)") realSpec imagSpec True g
            -- shift it to zero
            yMax = D.yMax powerSpec
            xMax = D.xMax1 powerSpec
            xMiddle = xMax / 2
            normVals = V.map (\(x, y, w) -> (x, y / yMax, w)) (D.values1 powerSpec)
            xStep = xMax / fromIntegral (V.length normVals - 1)
            (left, right) = V.partition (\(x, _, _) -> x >= xMiddle) normVals
            left1 = V.map (\(x, y, w) -> (x - xMax - xStep, y, w)) left
            powerSpec1 = D.spectrum1 $ left1 V.++ right
        modifyState stateRef $ addDiscreteData powerSpec1 name (Just (currentGraphTab, selectedGraph))

