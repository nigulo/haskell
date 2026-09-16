{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Envelopes where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils as U
import qualified Math.Function as F

import TSA.CommonParams
import TSA.RegressionParams
import TSA.Params
import TSA.Data
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import GUI.Widget hiding (entryGetString)
import qualified TSA.Envelopes as E

import Utils.Misc

import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent
import System.Random

import Control.Applicative

paramsDialog :: StateRef -> IO ()
paramsDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = envParams (params state)
        upperParams = envUpperParams parms
        lowerParams = envLowerParams parms
        meanParams = envMeanParams parms

    win <- dialogWithTitle state "Envelopes"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8

    upperNameEntry <- Gtk.entryNew
    upperNameEntry `entrySetText` (getNameWithNo upperParams)
    addWidgetToBox (Just "Upper envelope name: ") upperNameEntry contentBox

    lowerNameEntry <- Gtk.entryNew
    lowerNameEntry `entrySetText` (getNameWithNo lowerParams)
    addWidgetToBox (Just "Lower envelope name: ") lowerNameEntry contentBox

    meanNameEntry <- Gtk.entryNew
    meanNameEntry `entrySetText` (getNameWithNo meanParams)
    addWidgetToBox (Just "Mean envelope name: ") meanNameEntry contentBox

    sep <- Gtk.separatorNew Gtk.OrientationHorizontal
    addWidgetToBox Nothing sep contentBox

    methodCombo <- createComboBox ["Least squares fit", "Interpolate"]
    case envMethod parms of
        False -> comboBoxSetActive methodCombo 0
        True -> comboBoxSetActive methodCombo 1
    addWidgetToBox (Just "Method: ") methodCombo contentBox

    startExtremaAdjustment <- Gtk.adjustmentNew (fromIntegral (envStartExtrema parms)) 1 100 1 1 1
    startExtremaSpin <- Gtk.spinButtonNew (Just startExtremaAdjustment) 1 0
    addWidgetToBox (Just "Start from extrema: ") startExtremaSpin contentBox

    dataSetCombo <- dataSetComboNew dataAndSpectrum state
    addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    Gtk.onButtonClicked cancelButton $ do
        Gtk.windowDestroy win

    Gtk.onButtonClicked okButton $ do
        upperName <- entryGetString upperNameEntry
        lowerName <- entryGetString lowerNameEntry
        meanName <- entryGetString meanNameEntry

        method <- comboBoxGetActive methodCombo
        startExtrema <- spinButtonGetValue startExtremaSpin

        Just selectedData <- getSelectedData dataSetCombo
        Gtk.windowDestroy win

        let
            newEnvParams = EnvParams {
                    envUpperParams = updateCommonParams upperName upperParams,
                    envLowerParams = updateCommonParams lowerName lowerParams,
                    envMeanParams = updateCommonParams meanName meanParams,
                    envStartExtrema = round startExtrema,
                    envMethod = (if method == 0 then False else True),
                    envData = Just selectedData
                }

        modifyStateParams stateRef $ \params -> params {envParams = newEnvParams}
        runTask stateRef "Find envelopes" $ (envelopes stateRef (upperName, lowerName, meanName))
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win

envelopes :: StateRef -> (String, String, String) -> IO ()
envelopes stateRef (upperName, lowerName, meanName) =
    do
        state <- readMVar stateRef
        g <- getStdGen
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms

            parms = envParams (params state)
        tEnv <- taskEnv stateRef

        E.envelopes parms (upperName, lowerName, meanName) tEnv (DataUpdateFunc (\dat name update -> modifyState stateRef $ addOrUpdateData dat name (Just (currentGraphTab, selectedGraph)) update))
        return ()
