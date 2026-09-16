{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.SpecificPoints (findSpecificPointsDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.AnalyticData as AD
import Regression.Utils

import TSA.CommonParams
import TSA.Params
import qualified TSA.SpecificPoints
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import GUI.Widget hiding (entryGetString)

import Data.List
import Data.Maybe
import Utils.Misc

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import Math.Expression
import qualified Math.Function as F

import System.Random
import qualified Data.Vector.Unboxed as V
import Statistics.Sample


findSpecificPointsDialog :: StateRef -> IO ()
findSpecificPointsDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = specificPointsParams (params state)
        commonParams = specificPointsCommonParams parms

    win <- dialogWithTitle state "Find specific points"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

    typeCombo <- createComboBox ["Local extrema", "Global extrema", "Zero-crossings"]
    comboBoxSetActive typeCombo (specificPointsType parms)
    addWidgetToBox (Just "Type: ") typeCombo contentBox

    dataSetCombo <- dataSetComboNew (\_ -> True) state
    addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) contentBox

    precisionAdjustment <- Gtk.adjustmentNew (fromIntegral (specificPointsPrecision parms)) 1 (2**52) 1 1 1
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
        spType <- comboBoxGetActive typeCombo
        precision <- spinButtonGetValue precisionSpin
        Just selectedData <- getSelectedData dataSetCombo
        Gtk.windowDestroy win

        modifyStateParams stateRef $ \params -> params {specificPointsParams = SpecificPointsParams {
                specificPointsData = Just selectedData,
                specificPointsType = spType,
                specificPointsPrecision = round precision,
                specificPointsCommonParams = updateCommonParams name commonParams
            }}


        runTask stateRef "Find specific points" $ findSpecificPoints stateRef selectedData (round precision) name spType
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win

findSpecificPoints :: StateRef -> DataParams -> Int -> String -> Int -> IO ()
findSpecificPoints stateRef dataParams precision name spType = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
    tEnv <- taskEnv stateRef
    results <-
        case spType of
            0 -> do
                (minima, maxima) <- TSA.SpecificPoints.findExtrema dataParams precision False name tEnv
                return [minima, maxima]
            1 -> do
                (minima, maxima) <- TSA.SpecificPoints.findExtrema dataParams precision True name tEnv
                return [minima, maxima]
            2 -> do
                zc <- TSA.SpecificPoints.findZeroCrossings dataParams precision name tEnv
                return [zc]
    mapM_ (\dp -> modifyState stateRef $ addDataParams dp (Just (currentGraphTab, selectedGraph))) results
    (progressUpdateFunc tEnv) 1
