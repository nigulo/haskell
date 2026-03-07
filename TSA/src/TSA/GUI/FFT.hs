{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.FFT where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import Regression.Data as D
import Regression.FFT
import Regression.Utils as U
import Math.Function as F

import TSA.CommonParams
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

    win <- dialogWithTitle state "Fast fourier transform"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    fftNameEntry <- Gtk.entryNew
    entrySetText fftNameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "FFT name: ") fftNameEntry contentBox

    directionCombo <- createComboBox ["Time -> Frequency", "Frequency -> Time"]
    if fftDirection parms then comboBoxSetActive directionCombo 0 else comboBoxSetActive directionCombo 1
    addWidgetToBox (Just "Direction: ") directionCombo contentBox

    realSpectrumCombo <- dataSetComboNew2 onlyEvenlySampled state False
    addWidgetToBox (Just "Real signal: ") (getComboBox realSpectrumCombo) contentBox

    imagSpectrumCombo <- dataSetComboNew2 onlyEvenlySampled state False
    addWidgetToBox (Just "Imaginary signal: ") (getComboBox imagSpectrumCombo) contentBox

    phaseShiftAdjustment <- Gtk.adjustmentNew (fftPhaseShift parms) (-1) 1 0.1 0.1 1
    phaseShiftSpin <- Gtk.spinButtonNew (Just phaseShiftAdjustment) 1 2
    addWidgetToBox (Just "Phase shift: ") phaseShiftSpin contentBox

    calcPowerCheck <- do
        button <- Gtk.checkButtonNew
        Gtk.checkButtonSetActive button (fftCalcPower parms)
        return button
    addWidgetToBox (Just "Power:") calcPowerCheck contentBox

    calcReAndImCheck <- do
        button <- Gtk.checkButtonNew
        Gtk.checkButtonSetActive button (fftCalcReAndIm parms)
        return button
    addWidgetToBox (Just "Real and imaginary:") calcReAndImCheck contentBox

    infoLabel <- Gtk.labelNew $ Just $ T.pack "Number of samples will be truncated to closest power of 2"
    addWidgetToBox Nothing infoLabel contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd

    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"

    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton

    _ <- Gtk.onButtonClicked cancelButton $ do
        Gtk.windowDestroy win

    _ <- Gtk.onButtonClicked okButton $ do
        name <- entryGetString fftNameEntry
        directionIdx <- comboBoxGetActive directionCombo

        selectedRealSpec <- getSelectedData realSpectrumCombo
        selectedImagSpec <- getSelectedData imagSpectrumCombo
        phaseShift <- spinButtonGetValue phaseShiftSpin
        calcPower <- Gtk.checkButtonGetActive calcPowerCheck
        calcReAndIm <- Gtk.checkButtonGetActive calcReAndImCheck
        Gtk.windowDestroy win

        modifyStateParams stateRef $ \params -> params {fftParams = FftParams {
            fftCommonParams = updateCommonParams name commonParams,
            fftDirection = (directionIdx == 0),
            fftPhaseShift = phaseShift,
            fftCalcPower = calcPower,
            fftCalcReAndIm = calcReAndIm,
            fftRealData = case selectedRealSpec of
                Nothing -> Nothing
                Just s -> Just $ left $ unboxSubData $ subData $ head $ dataSet s,
            fftImagData = case selectedImagSpec of
                Nothing -> Nothing
                Just s -> Just $ left $ unboxSubData $ subData $ head $ dataSet s
        }}

        forkOS $ fft stateRef name
        return ()

    Gtk.boxAppend contentBox buttonBox
    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win

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
                Just dat ->
                    let
                        (xs, ys, _) = V.unzip3 (D.values1 dat)
                        step = xs V.! 1 - V.head xs
                    in
                        (ys, step)
            (imags, imagStep) = case fftImagData parms of
                Nothing -> (V.replicate n 0, 0)
                Just dat ->
                    let
                        (xs, ys, _) = V.unzip3 (D.values1 dat)
                        step = xs V.! 1 - V.head xs
                    in
                        (ys, step)
            step = max realStep imagStep
            fftFunc = if fftDirection parms then fromTimeToFrequency else fromFrequencyToTime
            phaseShift = fftPhaseShift parms
            calcPower = fftCalcPower parms
            calcReAndIm = fftCalcReAndIm parms
            numToUse = 2 ^ (floor (logBase 2 (fromIntegral n)))
            ys1 = V.take numToUse $ V.zipWith (:+) reals imags

        spec1 <- fftFunc ys1 phaseShift
        let
            len = V.length spec1
            specStep = if len == 0 then 0 else 1 / (fromIntegral len * step)
        realSpec <- return $ D.Spectrum2 ((0, specStep), V.zip (V.map realPart spec1) (V.replicate len 0))
        imagSpec <- return $ D.Spectrum2 ((0, specStep), V.zip (V.map imagPart spec1) (V.replicate len 0))

        if calcReAndIm
            then do
                modifyState stateRef $ addDiscreteData realSpec (name ++ "_Re") (Just (currentGraphTab, selectedGraph))
                modifyState stateRef $ addDiscreteData imagSpec (name ++ "_Im") (Just (currentGraphTab, selectedGraph))
            else
                return ()

        if calcPower
            then do
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
            else
                return ()
