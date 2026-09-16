{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.D2 (d2Dialog) where

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
import TSA.D2
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import GUI.Widget hiding (entryGetString)

import Data.List
import Data.Maybe
import Utils.Misc
import Utils.Concurrent

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import System.IO
import qualified Math.Function as F

import System.Random
import qualified Data.Vector.Unboxed as V
import Statistics.Sample

import System.Random.MWC

d2Dialog :: StateRef -> IO ()
d2Dialog stateRef = do
    state <- readMVar stateRef
    let
        parms = d2Params (params state)
        commonParams = d2CommonParams parms

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

    periodStartAdjustment <- Gtk.adjustmentNew (d2PeriodStart parms) 0 (2**52) 1 1 1
    periodStartSpin <- Gtk.spinButtonNew (Just periodStartAdjustment) 1 10
    addWidgetToBox (Just "Period start: ") periodStartSpin contentBox

    periodEndAdjustment <- Gtk.adjustmentNew (d2PeriodEnd parms) 0 (2**52) 1 1 1
    periodEndSpin <- Gtk.spinButtonNew (Just periodEndAdjustment) 1 10
    addWidgetToBox (Just "Period end: ") periodEndSpin contentBox

    corrLenStartAdjustment <- Gtk.adjustmentNew (d2CorrLenStart parms) 0 (2**52) 1 1 1
    corrLenStartSpin <- Gtk.spinButtonNew (Just corrLenStartAdjustment) 1 10
    addWidgetToBox (Just "Correlation length start: ") corrLenStartSpin contentBox

    corrLenEndAdjustment <- Gtk.adjustmentNew (d2CorrLenEnd parms) 0 (2**52) 1 1 1
    corrLenEndSpin <- Gtk.spinButtonNew (Just corrLenEndAdjustment) 1 10
    addWidgetToBox (Just "Correlation length end: ") corrLenEndSpin contentBox

    methodCombo <- createComboBox ["Box", "Gauss"]
    comboBoxSetActive methodCombo (d2Method parms)
    addWidgetToBox (Just "Method: ") methodCombo contentBox

    normalizeCheck <- Gtk.checkButtonNew
    Gtk.checkButtonSetActive normalizeCheck (d2Normalize parms)
    addWidgetToBox (Just "Normalize") normalizeCheck contentBox

    precisionAdjustment <- Gtk.adjustmentNew (fromIntegral (d2Precision parms)) 1 (2**52) 1 1 1
    precisionSpin <- Gtk.spinButtonNew (Just precisionAdjustment) 1 0
    addWidgetToBox (Just "Precision: ") precisionSpin contentBox

    deltaPhiAdjustment <- Gtk.adjustmentNew (0.1) 0 (2**52) 1 1 1
    deltaPhiSpin <- Gtk.spinButtonNew (Just deltaPhiAdjustment) 1 10
    addWidgetToBox (Just "Phase proximity: ") deltaPhiSpin contentBox

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
        periodStart <- spinButtonGetValue periodStartSpin
        periodEnd <- spinButtonGetValue periodEndSpin
        corrLenStart <- spinButtonGetValue corrLenStartSpin
        corrLenEnd <- spinButtonGetValue corrLenEndSpin
        methodNo <- comboBoxGetActive methodCombo
        normalize <- Gtk.checkButtonGetActive normalizeCheck
        precision <- spinButtonGetValue precisionSpin
        deltaPhi <- spinButtonGetValue deltaPhiSpin
        Gtk.windowDestroy win

        modifyStateParams stateRef $ \params -> params {d2Params = D2Params {
                d2Data = Just selectedData,
                d2PeriodStart = periodStart,
                d2PeriodEnd = periodEnd,
                d2CorrLenStart = corrLenStart,
                d2CorrLenEnd = corrLenEnd,
                d2Method = methodNo,
                d2Normalize = normalize,
                d2Precision = round precision,
                d2CommonParams = updateCommonParams name commonParams
            }}

        runTask stateRef "D2 statistic" $ d2 stateRef selectedData periodStart periodEnd corrLenStart corrLenEnd methodNo (round precision) name normalize deltaPhi
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win

d2 :: StateRef -> DataParams -> Double -> Double -> Double -> Double -> Int -> Int -> String -> Bool -> Double -> IO ()
d2 stateRef dataParams periodStart periodEnd minCorrLen maxCorrLen methodNo precision name normalize deltaPhi = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    tEnv <- taskEnv stateRef
    let
        SD1 dat = subData (head (dataSet dataParams))
    dispersions <- calcDispersions dat (1 / periodEnd) (1 / periodStart) minCorrLen maxCorrLen (if methodNo == 0 then Box else Gauss) precision name normalize deltaPhi tEnv
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
    handle <- openFile (name ++ ".csv") WriteMode
    V.mapM_ (\(corrLen, freq, disp, _) -> do
        hPutStr handle (show corrLen ++ " " ++ show freq ++ " " ++ show disp ++ "\n")
        ) $ D.values2 dispersions
    hClose handle
    modifyState stateRef $ addDataParams (createDataParams_ name [createSubDataParams_ (SD1 dispersions)]) (Just (currentGraphTab, selectedGraph))
    (progressUpdateFunc tEnv) 1
