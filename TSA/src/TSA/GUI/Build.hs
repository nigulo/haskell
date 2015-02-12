
module TSA.GUI.Build (buildDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Abstract.Box
import Graphics.UI.Gtk.Layout.VBox

import Regression.Data as D

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data 
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget

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

    dialog <- dialogWithTitle state "Build new data set"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    
    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog
    
    addLabel "Choose x" dialog

    dataSetCombo1 <- dataSetComboNew dataAndSpectrum state
    addWidget (Just "Data set 1: ") (getComboBox dataSetCombo1) dialog

    typeCombo1 <- createComboBox ["x", "y"]
    comboBoxSetActive typeCombo1 0
    addWidget (Just "From: ") typeCombo1 dialog

    addSeparator dialog

    addLabel "Choose y" dialog
    dataSetCombo2 <- dataSetComboNew dataAndSpectrum state
    addWidget (Just "Data set 2: ") (getComboBox dataSetCombo2) dialog

    typeCombo2 <- createComboBox ["y", "x"]
    comboBoxSetActive typeCombo2 0
    addWidget (Just "From: ") typeCombo2 dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString nameEntry
                
                Just selectedData1 <- getSelectedData dataSetCombo1
                Just selectedData2 <- getSelectedData dataSetCombo2
                Just t1 <- comboBoxGetActiveString typeCombo1
                typeNo1 <- comboBoxGetActive typeCombo1
                Just t2 <- comboBoxGetActiveString typeCombo2
                typeNo2 <- comboBoxGetActive typeCombo2
                widgetDestroy dialog

                let
                    mapOp sdp1 sdp2 =
                        let
                            Left d1 = subData sdp1
                            Left d2 = subData sdp2
                            xs = case t1 of
                                "x" -> D.xs1 d1
                                "y" -> D.ys d1
                            ys = case t2 of
                                "x" -> D.xs1 d2
                                "y" -> D.ys d2
                        in
                                SubDataParams {subData = Left (D.data1' (V.zip xs ys )), subDataBootstrapSet = []}
                    result = zipWith mapOp (dataSet selectedData1) (dataSet selectedData2) 
                    
                    graphTabParms = (graphTabs state) !! currentGraphTab
                    selectedGraph = graphTabSelection graphTabParms

                modifyState stateRef $ addDataParams (DataParams {dataName = name, dataSet = result}) (Just (currentGraphTab, selectedGraph))

                modifyStateParams stateRef $ \params -> params {buildParams = BuildParams {
                        buildCommonParams = updateCommonParams name commonParams
                    }}
                return ()
        else
            do
                widgetDestroy dialog

