{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Period (findPeriodDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import Debug.Trace

import Regression.Functions as FS
import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils

import TSA.CommonParams
import TSA.Params
import TSA.Period
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import GUI.Widget

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

import System.Random.MWC

findPeriodDialog :: StateRef -> IO ()
findPeriodDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = findPeriodParams (params state)
        commonParams = findPeriodCommonParams parms

    win <- dialogWithTitle state "Find period"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

    dataSetCombo <- dataSetComboNew onlyData state
    addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) contentBox

    methodCombo <- createComboBox ["Least squares", "String length"]
    comboBoxSetActive methodCombo (findPeriodMethod parms)
    addWidgetToBox (Just "Method: ") methodCombo contentBox

    periodStartAdjustment <- Gtk.adjustmentNew (findPeriodStart parms) 0 (2**52) 1 1 1
    periodStartSpin <- Gtk.spinButtonNew (Just periodStartAdjustment) 1 10
    addWidgetToBox (Just "Period start: ") periodStartSpin contentBox

    periodEndAdjustment <- Gtk.adjustmentNew (findPeriodEnd parms) 0 (2**52) 1 1 1
    periodEndSpin <- Gtk.spinButtonNew (Just periodEndAdjustment) 1 10
    addWidgetToBox (Just "Period end: ") periodEndSpin contentBox

    precisionAdjustment <- Gtk.adjustmentNew (fromIntegral (findPeriodPrecision parms)) 1 (2**52) 1 1 1
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

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy win

    _ <- Gtk.onButtonClicked okButton $ do
        name <- entryGetString nameEntry
        Just selectedData <- getSelectedData dataSetCombo
        methodNo <- comboBoxGetActive methodCombo
        periodStart <- spinButtonGetValue periodStartSpin
        periodEnd <- spinButtonGetValue periodEndSpin
        precision <- spinButtonGetValue precisionSpin
        Gtk.windowDestroy win

        modifyStateParams stateRef $ \params -> params {findPeriodParams = FindPeriodParams {
                findPeriodData = Just selectedData,
                findPeriodMethod = methodNo,
                findPeriodStart = periodStart,
                findPeriodEnd = periodEnd,
                findPeriodPrecision = round precision,
                findPeriodCommonParams = updateCommonParams name commonParams
            }}

        runTask stateRef "Find period" $ findPeriod stateRef selectedData periodStart periodEnd (round precision) methodNo name
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win

findPeriod :: StateRef -> DataParams -> Double -> Double -> Int -> Int -> String-> IO ()
findPeriod stateRef dataParams periodStart periodEnd precision method name = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    tEnv <- taskEnv stateRef
    dispersions <- calcDispersions dataParams periodStart periodEnd precision method name False tEnv
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
    modifyState stateRef $ addDataParams dispersions (Just (currentGraphTab, selectedGraph))
