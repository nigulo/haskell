module TSA.GUI.Bayes (linRegWithMLIIDialog) where

import Graphics.UI.Gtk hiding (addWidget, Plus)

import qualified Regression.Bayes as B
import Regression.Data as D

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log

import GUI.Widget

import Control.Concurrent.MVar

linRegWithMLIIDialog :: StateRef -> IO ()
linRegWithMLIIDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = bayesLinRegParams (params state)
        commonParams = bayesLinRegCommonParams parms
    dialog <- dialogWithTitle state "Bayesian linear regression using MLII algorithm"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog

    methodCombo <- createComboBox ["RBF"]
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
    on (castToEditable nameEntry) editableChanged toggleFitButton
    
    
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
                
                modifyStateParams stateRef $ \params -> params {bayesLinRegParams = BayesLinRegParams {
                        bayesLinRegCommonParams = updateCommonParams name commonParams
                    }}

                runTask stateRef "MLII" $ linRegWithMLII stateRef methodNo name selectedData
                return ()
        else
            do
                widgetDestroy dialog

linRegWithMLII :: StateRef -> Int -> String -> DataParams -> IO ()
linRegWithMLII stateRef method fitName dataParams = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    tEnv <- taskEnv stateRef
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        dat = dataSet dataParams
        minx = D.xMin1 dat
        maxx = D.xMax1 dat
        range = maxx - minx
        numCentres = 10
        lambdaMin = range / 10
        lambdaMax = range
        numLambdas = 100
        opts = (50, 0.001)

        func i j (Left dat) _ = do
            (ad, varFunc) <- B.linRegWithMLII dat (B.MethodRBF 10 [(minx, maxx)] [lambdaMin, lambdaMin + (lambdaMax - lambdaMin) / (numLambdas - 1) .. lambdaMax] opts)
            return $ Right $ Left ad
    result <- applyToData1 func dataParams fitName tEnv
    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))

