
module TSA.GUI.Attractor (attractorDialog) where



import Graphics.UI.Gtk hiding (addWidget)
import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget

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

    dialog <- dialogWithTitle state "Find Attractor"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    okButton <- dialogAddButton dialog "Ok" ResponseOk

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog

    dataSetCombo <- dataSetComboNew (\dp -> only2d dp && dataAndSpectrum dp) state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog

    dimensionCombo <- createComboBox ["2", "3"]
    comboBoxSetActive dimensionCombo (attractorDimension parms - 2)
    addWidget (Just "Dimension: ") dimensionCombo dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString nameEntry
                Just selectedData <- getSelectedData dataSetCombo
                dimension <- comboBoxGetActive dimensionCombo 
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {attractorParams = AttractorParams {
                        attractorData = Just selectedData,
                        attractorDimension = dimension + 2,
                        attractorCommonParams = updateCommonParams name commonParams
                    }}
                
                
                forkIO $ findAttractor stateRef selectedData (dimension + 2)
                return ()
        else
            do
                widgetDestroy dialog

findAttractor :: StateRef -> DataParams -> Int -> IO ()
findAttractor stateRef dataParams dimension =
    do
        state <- readMVar stateRef
        (currentGraphTab, _) <- getCurrentGraphTab state
        let 
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms

            Left dat = subData $ head $ dataSet dataParams
            vals = D.ys dat

            attractorData =
                case dimension of 
                    2 -> D.data1' $ V.zip (V.init vals) (V.tail vals)
                    3 -> D.data2 $ map (\(x1, x2, y, w ) -> ([x1, x2], y, w)) $ (zip4 (init  (init (V.toList vals))) (init (tail (V.toList vals)))) (tail (tail (V.toList vals))) (repeat 1)
            
        modifyState stateRef $ addData (Left attractorData) ((dataName dataParams) ++ "_attractor" ++ show dimension) (Just (currentGraphTab, selectedGraph))




