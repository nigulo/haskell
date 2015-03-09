
module TSA.GUI.Interpolate (paramsDialog) where

import Graphics.UI.Gtk hiding (addWidget, Plus)
import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D

import System.Random

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget

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

    dialog <- dialogWithTitle state "Interpolate"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog

    methodCombo <- createComboBox ["Linear", "Polynomial"]
    addWidget (Just "Method: ") methodCombo dialog

    dataSetCombo <- dataSetComboNew onlyData state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog
    
    let 
        toggleFitButton :: IO ()
        toggleFitButton = 
            do
                selectedData <- getSelectedData dataSetCombo
                fitName <- entryGetString nameEntry
                sensitivity <-
                    case selectedData of 
                        Just _ -> if length fitName <= 0 then return False 
                                                    else return True
                        Nothing -> return False
                fitButton `widgetSetSensitivity` sensitivity
                        
    on (getComboBox dataSetCombo) changed toggleFitButton
    --nameEntry `onButtonRelease` (\e -> toggleFitButton >> return False)
    --nameEntry `onKeyRelease` (\e -> toggleFitButton >> return False)
    on (castToEditable nameEntry) editableChanged toggleFitButton
--    nameEntry `afterInsertAtCursor` (\s -> toggleFitButton)
--    nameEntry `afterPasteClipboard` (toggleFitButton)
--    nameEntry `afterCutClipboard` (toggleFitButton)
    
    
    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString nameEntry
                Just method <- comboBoxGetActiveString methodCombo
                methodNo <- comboBoxGetActive methodCombo
                Just selectedData <- getSelectedData dataSetCombo
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {interpolateParams = InterpolateParams {
                        interpolateCommonParams = updateCommonParams name commonParams
                    }}

                runTask stateRef "Interpolate" $ fit stateRef methodNo name selectedData
                return ()
        else
            do
                widgetDestroy dialog

fit :: StateRef -> Int -> String -> DataParams -> IO ()
fit stateRef method fitName dat = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    tEnv <- taskEnv stateRef
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        func i j (Left dat) _ = fitWithSpline_ 1 1 dat True 0 (progressUpdateFunc tEnv) >>= \spline -> return $ Right $ Left spline
    result <- applyToData1 func dat fitName tEnv
    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))

