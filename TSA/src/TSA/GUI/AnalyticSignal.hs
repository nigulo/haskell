{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.AnalyticSignal (analyticSignalDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import qualified Regression.Polynom as P
import Regression.AnalyticData as AD
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils
import Regression.FFT
import qualified Math.Function as F

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import qualified TSA.AnalyticSignal as AS

import GUI.Widget hiding (entryGetString)
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

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    amplitudeEntry <- Gtk.entryNew
    entrySetText amplitudeEntry (getNameWithNo amplitudeParams)
    addWidgetToBox (Just "Amplitude name: ") amplitudeEntry contentBox

    phaseEntry <- Gtk.entryNew
    entrySetText phaseEntry (getNameWithNo phaseParams)
    addWidgetToBox (Just "Phase name: ") phaseEntry contentBox

    frequencyEntry <- Gtk.entryNew
    entrySetText frequencyEntry (getNameWithNo frequencyParams)
    addWidgetToBox (Just "Frequency name: ") frequencyEntry contentBox

    realCombo <- dataSetComboNew (\_ -> True) state
    addWidgetToBox (Just "Real signal: ") (getComboBox realCombo) contentBox

    conjugatedCombo <- dataSetComboNew2 (\_ -> True) state False
    addWidgetToBox (Just "Conjugated signal: ") (getComboBox conjugatedCombo) contentBox

    precisionAdjustment <- Gtk.adjustmentNew 65536 1 1048576 1 1 1
    precisionSpin <- Gtk.spinButtonNew (Just precisionAdjustment) 1 0
    addWidgetToBox (Just "Precision: ") precisionSpin contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy dialog

    _ <- Gtk.onButtonClicked okButton $ do
        amplitudeName <- entryGetString amplitudeEntry
        phaseName <- entryGetString phaseEntry
        frequencyName <- entryGetString frequencyEntry

        Just realData <- getSelectedData realCombo
        conjugatedData <- getSelectedData conjugatedCombo
        precision <- spinButtonGetValue precisionSpin
        Gtk.windowDestroy dialog

        modifyStateParams stateRef $ \params -> params {asParams = AnalyticSignalParams {
            asAmplitudeParams = updateCommonParams amplitudeName amplitudeParams,
            asPhaseParams = updateCommonParams phaseName phaseParams,
            asFrequencyParams = updateCommonParams frequencyName frequencyParams,
            asRealData = Just realData,
            asImagData = conjugatedData
        }}

        runTask stateRef "Analytic signal" $ analyticSignal stateRef (round precision) (amplitudeName, phaseName, frequencyName)
        return ()

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowPresent dialog

analyticSignal :: StateRef -> Int -> (String, String, String) -> IO ()
analyticSignal stateRef precision (amplitudeName, phaseName, frequencyName) =
    do
        state <- readMVar stateRef
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms
            asParms = asParams (params state)
            dataParams = fromJust (asRealData asParms)
        tEnv <- taskEnv stateRef
        AS.analyticSignal asParms precision (amplitudeName, phaseName, frequencyName, ((dataName dataParams) ++ "_conj")) tEnv (DataUpdateFunc (\dat name update -> modifyState stateRef $ addOrUpdateData dat name (Just (currentGraphTab, selectedGraph)) update))
        return ()