{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Selection (selectionDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T

import qualified Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.AnalyticData as AD
import Regression.Data as D
import Regression.Utils
import qualified Math.Function as F
import qualified Math.Expression as E

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Plot
import GUI.Widget hiding (entryGetString)

import Utils.Misc

import Data.IORef
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative

import System.Random

selectionDialog :: StateRef -> IO ()
selectionDialog stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms
        (x1, x2, y1, y2) =
            case graphSelection graphParms of
                Just sel ->
                    (min (graphSelectionLeft sel) (graphSelectionRight sel),
                    max (graphSelectionLeft sel) (graphSelectionRight sel) ,
                    min (graphSelectionBottom sel) (graphSelectionTop sel),
                    max (graphSelectionBottom sel) (graphSelectionTop sel))
                Nothing -> (plotAreaLeft ga, plotAreaRight ga, plotAreaBottom ga, plotAreaTop ga)

        parms = selectionParams (params state)
        commonParams = selectionCommonParams parms

    win <- dialogWithTitle state "Filter data"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    modifyOriginalCheck <- Gtk.checkButtonNew
    Gtk.checkButtonSetActive modifyOriginalCheck (selectionModifyOriginal parms)
    addWidgetToBox (Just "Modify original data: ") modifyOriginalCheck contentBox

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name suffix: ") nameEntry contentBox

    dataSetCombo <- dataSetComboNew2 (\dp -> (dataName dp) `elem` (map graphDataParamsName (graphData graphParms))) state False
    addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) contentBox

    leftAdjustment <- Gtk.adjustmentNew x1 (-2**52) (2**52) 1 1 10
    leftSpin <- Gtk.spinButtonNew (Just leftAdjustment) 1 10
    addWidgetToBox (Just "Left: ") leftSpin contentBox

    rightAdjustment <- Gtk.adjustmentNew x2 (-2**52) (2**52) 1 1 10
    rightSpin <- Gtk.spinButtonNew (Just rightAdjustment) 1 10
    addWidgetToBox (Just "Right: ") rightSpin contentBox

    bottomAdjustment <- Gtk.adjustmentNew y1 (-2**52) (2**52) 1 1 10
    bottomSpin <- Gtk.spinButtonNew (Just bottomAdjustment) 1 10
    addWidgetToBox (Just "Bottom: ") bottomSpin contentBox

    topAdjustment <- Gtk.adjustmentNew y2 (-2**52) (2**52) 1 1 10
    topSpin <- Gtk.spinButtonNew (Just topAdjustment) 1 10
    addWidgetToBox (Just "Top: ") topSpin contentBox

    opCombo <- createComboBox ["Crop", "Delete"]
    comboBoxSetActive opCombo (selectionOp parms)
    addWidgetToBox (Just "Operation: ") opCombo contentBox

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
        suffix <- entryGetString nameEntry
        selectedData <- getSelectedData dataSetCombo
        opNo <- comboBoxGetActive opCombo
        modifyOriginal <- Gtk.checkButtonGetActive modifyOriginalCheck
        left <- spinButtonGetValue leftSpin
        right <- spinButtonGetValue rightSpin
        bottom <- spinButtonGetValue bottomSpin
        top <- spinButtonGetValue topSpin

        Gtk.windowDestroy win
        let
            op1 = opNo == 0  -- Crop = True, Delete = False
            dataToModify = case selectedData of
                Just dat -> [dat]
                Nothing -> map (\gp -> getDataByName (graphDataParamsName gp) state) $ graphData graphParms
            dataParms = map (doSelectionOp op1 (left, right, bottom, top)) dataToModify
            mapFunc dp =
                modifyMVar_ stateRef $ \state ->
                    if modifyOriginal
                        then return $ updateData dp state
                        else return $ addDataParams (dp {dataName = dataName dp ++ suffix}) (Just (currentGraphTab, selectedGraph)) state
        mapM_ mapFunc dataParms
        modifyStateParams stateRef $ \params -> params {selectionParams = SelectionParams {
                selectionCommonParams = updateCommonParams suffix commonParams,
                selectionOp = opNo,
                selectionModifyOriginal = modifyOriginal
            }}
        return ()

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowPresent win

doSelectionOp :: Bool -> (Double, Double, Double, Double) -> DataParams -> DataParams
doSelectionOp op (x1, x2, y1, y2) dp =
    let
        dat = dataSet dp
    in
        dp {
            dataSet = filter (\sdp ->
                case subData sdp of
                    SD1 dat -> D.dataLength dat > 0
                    _ -> True) $ map (\sdp ->
                    sdp {
                        subData =
                            case subData sdp of
                                SD1 d -> SD1 (selectData1 (x1, x2) (y1, y2) op d)
                                SD2 ad@(AnalyticData [(_, _, s)]) ->
                                    let
                                        xMin = AD.xMin1 ad
                                        xMax = AD.xMin1 ad
                                    in
                                        SD2 (if op then AnalyticData [([x1], [x2], s)] else AnalyticData [([xMin], [x1], s), ([x2], [xMax], s)])
                                SD3 ad@(AnalyticData [(_, _, f)]) ->
                                    let
                                        xMin = AD.xMin1 ad
                                        xMax = AD.xMin1 ad
                                    in
                                        SD3 (if op then AnalyticData [([x1], [x2], f)] else AnalyticData [([xMin], [x1], f), ([x2], [xMax], f)])
                                SD4 ad@(AnalyticData [(_, _, rbf)]) ->
                                    let
                                        xMin = AD.xMin1 ad
                                        xMax = AD.xMin1 ad
                                    in
                                        SD4 (if op then AnalyticData [([x1], [x2], rbf)] else AnalyticData [([xMin], [x1], rbf), ([x2], [xMax], rbf)])
                    }
                ) (dataSet dp)
        }
