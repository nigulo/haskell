{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Interpolate (paramsDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D hiding (interpolate)

import System.Random

import TSA.CommonParams
import TSA.Params
import qualified TSA.Interpolate as I
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget hiding (entryGetString)

import Utils.Misc

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import Math.Expression

paramsDialog :: StateRef -> IO ()
paramsDialog stateRef = do
    state <- readMVar stateRef
    let
        intParams = interpolateParams (params state)
        commonParams = interpolateCommonParams intParams

    g <- getStdGen
    (currentGraphTab, _) <- getCurrentGraphTab state

    win <- dialogWithTitle state "Interpolate"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

    methodCombo <- createComboBox ["Linear", "Polynomial"]
    addWidgetToBox (Just "Method: ") methodCombo contentBox

    dataSetCombo <- dataSetComboNew onlyData state
    addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) contentBox

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
        methodNo <- comboBoxGetActive methodCombo
        Just selectedData <- getSelectedData dataSetCombo
        Gtk.windowDestroy win

        modifyStateParams stateRef $ \params -> params {interpolateParams = InterpolateParams {
                interpolateCommonParams = updateCommonParams name commonParams
            }}

        runTask stateRef "Interpolate" $ interpolate stateRef methodNo name selectedData
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win

interpolate :: StateRef -> Int -> String -> DataParams -> IO ()
interpolate stateRef method fitName dat = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    tEnv <- taskEnv stateRef
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
    result <- I.interpolate method fitName dat tEnv
    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
