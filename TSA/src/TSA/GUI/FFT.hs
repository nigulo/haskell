
module TSA.GUI.FFT where


import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox

import Regression.Data as D
import qualified Regression.FFT as F

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget

import Utils.Misc

import Data.IORef
import Data.Complex
import qualified Data.Vector.Unboxed as V
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Applicative

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
    
    --label <- labelNew $ Just "Fast fourier transform"
    --addWidget Nothing label dialog

    bandStartAdjustment <- adjustmentNew (fftBandStart parms) 0 10000 1 1 1
    bandStartSpin <- spinButtonNew bandStartAdjustment 1 0
    addWidget (Just "Band start: ") bandStartSpin dialog

    bandEndAdjustment <- adjustmentNew (fftBandEnd parms) 0 10000 1 1 1
    bandEndSpin <- spinButtonNew bandEndAdjustment 1 0
    addWidget (Just "Band end: ") bandEndSpin dialog

    numSamplesAdjustment <- adjustmentNew (fromIntegral (fftNumSamples parms)) 2 16384 1 1 1
    numSamplesSpin <- spinButtonNew numSamplesAdjustment 1 0
    addWidget (Just "Num. samples: ") numSamplesSpin dialog

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

    let 
        toggleFftButton :: IO ()
        toggleFftButton = 
            do
                --selectedRealSpec <- getSelectedData realSpectrumCombo
                --numSamples <- spinButtonGetValue numSamplesSpin
                fftName <- entryGetText fftNameEntry
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
                start <- spinButtonGetValue bandStartSpin
                end <- spinButtonGetValue bandEndSpin
                samples <- spinButtonGetValue numSamplesSpin
                name <- entryGetText fftNameEntry
                direction <- comboBoxGetActiveText directionCombo
                
                selectedRealSpec <- getSelectedData realSpectrumCombo
                selectedImagSpec <- getSelectedData imagSpectrumCombo
                phaseShift <- spinButtonGetValue phaseShiftSpin
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {fftParams = FftParams {
                    fftCommonParams = updateCommonParams name commonParams,
                    fftBandStart = start,
                    fftBandEnd = end,
                    fftNumSamples = round samples,
                    fftDirection = (direction == Just "Time -> Frequency"),
                    fftPhaseShift = phaseShift,
                    fftRealData = case selectedRealSpec of 
                        Nothing -> Nothing
                        Just s -> Just $ left $ subData $ head $ dataSet s,
                    fftImagData = case selectedImagSpec of 
                        Nothing -> Nothing
                        Just s -> Just $ left $ subData $ head $ dataSet s
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
            n = case fftRealData parms of 
                Just s -> V.length $ D.xs1 s
                Nothing -> case fftImagData parms of
                    Just s -> V.length $ D.xs1 s
                    Nothing -> 0
            (reals, realStep) = case fftRealData parms of 
                Nothing -> (replicate n 0, 0)
                Just s@(Spectrum (((_, step):_), _)) -> (V.toList (D.ys s), step)
            (imags, imagStep) = case fftImagData parms of 
                Nothing -> (replicate n 0, 0)
                Just s@(Spectrum (((_, step):_), _)) -> (V.toList (D.ys s), step)
            step = max realStep imagStep
            fftFunc = if fftDirection parms then F.fromTimeToFrequency else F.fromFrequencyToTime
            bandStart = fftBandStart parms
            bandEnd = fftBandEnd parms
            phaseShift = fftPhaseShift parms
            numToAdd = 2 ^ (ceiling (logBase 2 (fromIntegral n))) - n
            ys1 = trace ("numToAdd: " ++ show numToAdd) $ zipWith (\r i -> (:+) r i) (reals ++ replicate numToAdd 0) (imags ++ replicate numToAdd 0)
        
        spec1 <- trace ("ys1: " ++ show (length ys1)) $ fftFunc ys1 (n + numToAdd) (phaseShift * pi) (progressUpdate stateRef)
        let
            len = length spec1
            specStep = if len == 0 then 0 else 1 / (fromIntegral len * step)
        putStrLn "Tere siin" 
        realSpec <- return $ D.Spectrum ([(0, specStep)], zip (map realPart spec1) (replicate len 0))
        putStrLn "Tere siin1" 
        imagSpec <- return $ D.Spectrum ([(0, specStep)], zip (map imagPart spec1) (replicate len 0))
        putStrLn $ "Tere siin2:"
        
        --putStrLn $ "realSpec xs:" ++ show (xs realSpec)
        --putStrLn $ "realSpec ys:" ++ show (spec1)

        --putStrLn $ "imagSpec xs:" ++ show (xs imagSpec)
        --putStrLn $ "imafSpec ys:" ++ show (ys imagSpec)

        modifyState stateRef $ addDiscreteData realSpec (name ++ "_Real") (Just (currentGraphTab, selectedGraph))
        modifyState stateRef $ addDiscreteData imagSpec (name ++ "_Imag") (Just (currentGraphTab, selectedGraph))
        putStrLn "Tere siin3"

