{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Build (buildDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import Regression.Data as D

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget hiding (entryGetString)

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative

import System.Random
import qualified Data.Vector.Unboxed as V

buildDialog :: StateRef -> IO ()
buildDialog stateRef = do
    state <- readMVar stateRef

    let
        parms = buildParams (params state)
        commonParams = buildCommonParams parms

    g <- getStdGen
    (currentGraphTab, _) <- getCurrentGraphTab state

    win <- dialogWithTitle state "Build new data set"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

    addLabel "Choose x" contentBox

    dataSetCombo1 <- dataSetComboNew dataAndSpectrum state
    addWidgetToBox (Just "Data set 1: ") (getComboBox dataSetCombo1) contentBox

    typeCombo1 <- createComboBox ["x", "y"]
    comboBoxSetActive typeCombo1 0
    addWidgetToBox (Just "From: ") typeCombo1 contentBox

    addSeparator contentBox

    addLabel "Choose y" contentBox
    dataSetCombo2 <- dataSetComboNew dataAndSpectrum state
    addWidgetToBox (Just "Data set 2: ") (getComboBox dataSetCombo2) contentBox

    typeCombo2 <- createComboBox ["y", "x"]
    comboBoxSetActive typeCombo2 0
    addWidgetToBox (Just "From: ") typeCombo2 contentBox

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

        Just selectedData1 <- getSelectedData dataSetCombo1
        Just selectedData2 <- getSelectedData dataSetCombo2
        typeNo1 <- comboBoxGetActive typeCombo1
        typeNo2 <- comboBoxGetActive typeCombo2
        Gtk.windowDestroy win

        let
            -- typeCombo1: ["x", "y"] -> 0="x", 1="y"
            t1 = if typeNo1 == 0 then "x" else "y"
            -- typeCombo2: ["y", "x"] -> 0="y", 1="x"
            t2 = if typeNo2 == 0 then "y" else "x"
            mapOp sdp1 sdp2 =
                let
                    SD1 d1 = subData sdp1
                    SD1 d2 = subData sdp2
                    xs = case t1 of
                        "x" -> D.xs1 d1
                        "y" -> D.ys d1
                    ys = case t2 of
                        "x" -> D.xs1 d2
                        "y" -> D.ys d2
                in
                    createSubDataParams_ (SD1 (D.data1' (V.zip xs ys )))
            result = zipWith mapOp (dataSet selectedData1) (dataSet selectedData2)

            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms

        modifyState stateRef $ addDataParams (createDataParams_ name result) (Just (currentGraphTab, selectedGraph))

        modifyStateParams stateRef $ \params -> params {buildParams = BuildParams {
                buildCommonParams = updateCommonParams name commonParams
            }}
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win
