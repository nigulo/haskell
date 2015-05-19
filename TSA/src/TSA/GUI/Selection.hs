module TSA.GUI.Selection (selectionDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox
import qualified Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.AnalyticData as AD
import Regression.Data as D
import Regression.Utils
import qualified Math.Function as F
import qualified Math.Expression as E

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Plot
import GUI.Widget

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
    
    dialog <- dialogWithTitle state "Filter data"
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk
    
    modifyOriginalCheck <- checkButtonNew >>= \button -> toggleButtonSetActive button (selectionModifyOriginal parms) >> return button
    addWidget (Just "Modify original data: ") modifyOriginalCheck dialog 

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name suffix: ") nameEntry dialog
    
    dataSetCombo <- dataSetComboNew2 (\dp -> (dataName dp) `elem` (map graphDataParamsName (graphData graphParms))) state False
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog
    
    leftAdjustment <- adjustmentNew x1 (-2**52) (2**52) 1 1 10
    leftSpin <- spinButtonNew leftAdjustment 1 10
    addWidget (Just "Left: ") leftSpin dialog

    rightAdjustment <- adjustmentNew x2 (-2**52) (2**52) 1 1 10
    rightSpin <- spinButtonNew rightAdjustment 1 10
    addWidget (Just "Right: ") rightSpin dialog
    
    bottomAdjustment <- adjustmentNew y1 (-2**52) (2**52) 1 1 10
    bottomSpin <- spinButtonNew bottomAdjustment 1 10
    addWidget (Just "Bottom: ") bottomSpin dialog

    topAdjustment <- adjustmentNew y2 (-2**52) (2**52) 1 1 10
    topSpin <- spinButtonNew topAdjustment 1 10
    addWidget (Just "Top: ") topSpin dialog
    
    opCombo <- createComboBox ["Crop", "Delete"]
    comboBoxSetActive opCombo (selectionOp parms)
    addWidget (Just "Operation: ") opCombo dialog
        
    widgetShowAll dialog
    response <- dialogRun dialog
        
    if response == ResponseOk 
        then
            do
                suffix <- entryGetString nameEntry
                selectedData <- getSelectedData dataSetCombo
                opNo <- comboBoxGetActive opCombo
                Just op <- comboBoxGetActiveString opCombo
                modifyOriginal <- toggleButtonGetActive modifyOriginalCheck
                left <- spinButtonGetValue leftSpin
                right <- spinButtonGetValue rightSpin
                bottom <- spinButtonGetValue bottomSpin
                top <- spinButtonGetValue topSpin
                
                widgetDestroy dialog
                let
                    op1 = if (op == "Delete") then False else True
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
        else
            widgetDestroy dialog

doSelectionOp :: Bool -> (Double, Double, Double, Double) -> DataParams -> DataParams
doSelectionOp op (x1, x2, y1, y2) dp =
    let 
        dat = dataSet dp
    in 
        dp {
            dataSet = filter (\sdp ->
                case subData sdp of
                    Left dat -> D.dataLength dat > 0
                    otherwise -> True) $ map (\sdp -> 
                    sdp {
                        subData = 
                            case subData sdp of
                                Left d -> Left (selectData1 (x1, x2) (y1, y2) op d)
                                Right (Left ad@(AnalyticData [(_, _, s)])) ->
                                    let 
                                        xMin = AD.xMin1 ad 
                                        xMax = AD.xMin1 ad
                                    in 
                                        Right $ Left (if op then AnalyticData [([x1], [x2], s)] else AnalyticData [([xMin], [x1], s), ([x2], [xMax], s)])
                                Right (Right ad@(AnalyticData [(_, _, f)])) -> 
                                    let 
                                        xMin = AD.xMin1 ad 
                                        xMax = AD.xMin1 ad
                                    in 
                                        Right $ Right (if op then AnalyticData [([x1], [x2], f)] else AnalyticData [([xMin], [x1], f), ([x2], [xMax], f)])
                    }
                ) (dataSet dp)
        }
