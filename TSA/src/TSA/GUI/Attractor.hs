{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Attractor (attractorDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget hiding (entryGetString)

import Data.List
import Utils.Misc

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import Math.Expression
import qualified Math.Function as F

import System.Random
import qualified Data.Vector.Unboxed as V

attractorDialog :: StateRef -> IO ()
attractorDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = attractorParams (params state)
        commonParams = attractorCommonParams parms

    win <- dialogWithTitle state "Find Attractor"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

    dataSetCombo <- dataSetComboNew (\dp -> only2d dp && dataAndSpectrum dp) state
    addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) contentBox

    dimensionCombo <- createComboBox ["2", "3"]
    comboBoxSetActive dimensionCombo (attractorDimension parms - 2)
    addWidgetToBox (Just "Dimension: ") dimensionCombo contentBox

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
        dimension <- comboBoxGetActive dimensionCombo
        Gtk.windowDestroy win

        modifyStateParams stateRef $ \params -> params {attractorParams = AttractorParams {
                attractorData = Just selectedData,
                attractorDimension = dimension + 2,
                attractorCommonParams = updateCommonParams name commonParams
            }}

        runTask stateRef "Find attractor" $ findAttractor stateRef selectedData (dimension + 2)
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win

findAttractor :: StateRef -> DataParams -> Int -> IO ()
findAttractor stateRef dataParams dimension =
    do
        state <- readMVar stateRef
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms

            SD1 dat = subData $ head $ dataSet dataParams
            vals = D.ys dat

            attractorData =
                case dimension of
                    2 -> D.data1' $ V.zip (V.init vals) (V.tail vals)
                    3 -> D.data2' $ V.zip3 (V.init (V.init vals)) (V.init (V.tail vals)) (V.tail (V.tail vals))

        modifyState stateRef $ addData (SD1 attractorData) ((dataName dataParams) ++ "_attractor" ++ show dimension) (Just (currentGraphTab, selectedGraph))
