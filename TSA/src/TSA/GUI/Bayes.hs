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

import Utils.List

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

    algoCombo <- createComboBox ["ML-II"]
    comboBoxSetActive algoCombo (bayesLinRegAlgo parms)
    addWidget (Just "Algorithm: ") algoCombo dialog

    methodCombo <- createComboBox ["RBF"]
    comboBoxSetActive methodCombo (bayesLinRegMethod parms)
    addWidget (Just "Method: ") methodCombo dialog

    -----------------------------------------
    let
        rbfParams = head (bayesLinRegMethodParams parms)
    rbfNumCentresAdjustment <- adjustmentNew (fromIntegral (rbfNumCentres rbfParams)) 1 (2**52) 1 1 1
    rbfNumCentresSpin <- spinButtonNew rbfNumCentresAdjustment 1 0
    (Just rbfNumCentresLabel, _) <- addWidget (Just "Num. centres: ") rbfNumCentresSpin dialog

    rbfNumLambdasAdjustment <- adjustmentNew (fromIntegral (rbfNumLambdas rbfParams)) 1 (2**52) 1 1 1
    rbfNumLambdasSpin <- spinButtonNew rbfNumLambdasAdjustment 1 0
    (Just rbfNumLambdasLabel, _) <- addWidget (Just "Num. lambdas: ") rbfNumLambdasSpin dialog
    -----------------------------------------

    dataSetCombo <- dataSetComboNew onlyData state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog


    let
        updateWidgets =
            do
                methodNo <- comboBoxGetActive methodCombo
                case methodNo of
                    0 -> do
                        rbfNumCentresSpin `set`  [widgetVisible := True]
                        rbfNumCentresLabel `set` [widgetVisible := True]
                        rbfNumLambdasSpin `set`  [widgetVisible := True]
                        rbfNumLambdasLabel `set` [widgetVisible := True]

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
    after dialog realize updateWidgets
    on methodCombo changed updateWidgets
    
    
    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                let
                    getMethodParams 0 = do
                        numCentres <- spinButtonGetValue rbfNumCentresSpin
                        numLambdas <- spinButtonGetValue rbfNumLambdasSpin
                        return RBFParams {
                            rbfNumCentres = round numCentres,
                            rbfNumLambdas = round numLambdas
                        }

                name <- entryGetString nameEntry
                --Just method <- comboBoxGetActiveString methodCombo
                methodNo <- comboBoxGetActive methodCombo
                Just selectedData <- getSelectedData dataSetCombo
                widgetDestroy dialog
                methodParams <- getMethodParams methodNo
                
                modifyStateParams stateRef $ \params -> 
                    let
                        bayesLinRegParms = bayesLinRegParams params 
                    in
                        params {bayesLinRegParams = bayesLinRegParms {
                                bayesLinRegData = Just selectedData,
                                bayesLinRegAlgo = 0,
                                bayesLinRegMethod = methodNo,
                                bayesLinRegMethodParams = Utils.List.updateAt methodNo methodParams (bayesLinRegMethodParams bayesLinRegParms),
                                bayesLinRegCommonParams = updateCommonParams name commonParams
                            }}

                runTask stateRef "ML-II" $ linRegWithMLII stateRef methodNo name selectedData methodParams
                return ()
        else
            do
                widgetDestroy dialog


linRegWithMLII :: StateRef -> Int -> String -> DataParams -> BayesLinRegMethodParams -> IO ()
linRegWithMLII stateRef method fitName dataParams (RBFParams numCentres numLambdas) = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    tEnv <- taskEnv stateRef
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        dat = dataSet dataParams
        opts = (50, 0.001)

        func i j (SD1 dat) _ = do
            let
                minx = D.xMin1 dat
                maxx = D.xMax1 dat
                range = maxx - minx
                lambdaMin = range / 10
                lambdaMax = range
            (ad, varFunc) <- B.linRegWithMLII dat (B.MethodRBF numCentres [(minx, maxx)] [lambdaMin, lambdaMin + (lambdaMax - lambdaMin) / (fromIntegral numLambdas - 1) .. lambdaMax] opts)
            return $ SD4 ad
    result <- applyToData1 func dataParams fitName tEnv
    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))

