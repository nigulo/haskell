-- | depracated
module TSA.GUI.IMF where

import Graphics.UI.Gtk hiding (addWidget)

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.IMF
import Regression.Utils as U

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog

import Utils.Misc

import Data.IORef
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Applicative

imfName = commonName . imfCommonParams

paramsDialog :: StateRef -> IO ()
paramsDialog stateRef = do
    state <- readMVar stateRef
    let
        params = imfParams state
        upperParams = imfUpperParams params
        lowerParams = imfLowerParams params
        commonParams = imfCommonParams params
    dialog <- dialogWithTitle state "Intrinsic mode functions"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    okButton <- dialogAddButton dialog "Ok" ResponseOk

    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    
    label <- labelNew $ Just "Upper envelope"
    addWidget Nothing label dialog
    upperFitWidgets@(FitWidgets upperNameEntry upperRankSpin upperPeriod upperHarmonics _ _ _ _) <- addFitWidgets upperParams state dialog
    upperNameEntry `widgetSetSensitivity` False
    upperPeriod `widgetSetSensitivity` False

    label <- labelNew $ Just "Lower envelope"
    addWidget Nothing label dialog
    lowerFitWidgets@(FitWidgets lowerNameEntry lowerRankSpin lowerPeriod lowerHarmonics _ _ _ _) <- addFitWidgets lowerParams state dialog
    lowerNameEntry `widgetSetSensitivity` False
    lowerPeriod `widgetSetSensitivity` False


    sep <- hSeparatorNew
    addWidget Nothing sep dialog
    precisionAdjustment <- adjustmentNew (imfPrecision params) 1 100000 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    imfNameEntry <- entryNew
    imfNameEntry `entrySetText` (imfName params)
    addWidget (Just "IMF name: ") imfNameEntry dialog
    
    dataSetCombo <- dataSetComboNew onlyData state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                newUpperParams <- getFitParams upperFitWidgets upperParams
                newLowerParams <- getFitParams lowerFitWidgets lowerParams

                precision <- spinButtonGetValue precisionSpin
                name <- entryGetText imfNameEntry
                
                Just selectedData <- getSelectedData dataSetCombo
                widgetDestroy dialog
                
                modifyMVar_ stateRef $ \state -> return $ state {imfParams = ImfParams {
                    imfUpperParams = newUpperParams,
                    imfLowerParams = newLowerParams,
                    imfPrecision = precision,
                    imfCommonParams = updateCommonParams name commonParams,
                    imfData = Just $ left (dataSet selectedData)
                }}
                
                forkIO $ imf stateRef
                return ()
        else
            do
                widgetDestroy dialog
                --return state

imf :: StateRef -> IO ()
imf stateRef = 
    do
        state <- readMVar stateRef
        (currentGraphTab, _) <- getCurrentGraphTab state
        let
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms

            params = imfParams state
            upperParms = imfUpperParams params
            lowerParms = imfLowerParams params
            Just dat = imfData params
        spline <- fitWithSpline_ 
            (fitPolynomRank upperParms) 
            (splineNumNodes (fitSplineParams upperParms)) 
            dat
            False
            2
            (\_ -> return ())
        let sdev = U.stdev dat (Right (Left spline))

        imfDat <- imfIO 
            (fitPolynomRank upperParms) 
            (splineNumNodes (fitSplineParams upperParms)) 
            (fitPolynomRank lowerParms) 
            (splineNumNodes (fitSplineParams lowerParms)) 
            (sdev / (imfPrecision params)) 
            dat
        
        let dat2 = D.subtr dat imfDat
        
        imfDat2 <- imfIO 
            (fitPolynomRank upperParms) 
            (splineNumNodes (fitSplineParams upperParms)) 
            (fitPolynomRank lowerParms) 
            (splineNumNodes (fitSplineParams lowerParms)) 
            (sdev / (imfPrecision params)) 
            dat2
        
        let dat3 = D.subtr dat2 imfDat2

        imfDat3 <- imfIO 
            (fitPolynomRank upperParms) 
            (splineNumNodes (fitSplineParams upperParms)) 
            (fitPolynomRank lowerParms) 
            (splineNumNodes (fitSplineParams lowerParms)) 
            (sdev / (imfPrecision params)) 
            dat3

        let 
            dat4 = D.subtr dat3 imfDat3

        modifyMVar_ stateRef $ \state -> return $ addData (Left imfDat) (imfName params) (Just (currentGraphTab, selectedGraph)) state
        modifyMVar_ stateRef $ \state -> return $ addData (Left imfDat2) ((imfName params) ++ "2") (Just (currentGraphTab, selectedGraph)) state
        modifyMVar_ stateRef $ \state -> return $ addData (Left imfDat3) ((imfName params) ++ "3") (Just (currentGraphTab, selectedGraph)) state
        modifyMVar_ stateRef $ \state -> return $ addData (Left dat2) ((imfName params) ++ "_diff") (Just (currentGraphTab, selectedGraph)) state
        modifyMVar_ stateRef $ \state -> return $ addData (Left dat3) ((imfName params) ++ "diff2") (Just (currentGraphTab, selectedGraph)) state
        modifyMVar_ stateRef $ \state -> return $ addData (Left dat4) ((imfName params) ++ "diff3") (Just (currentGraphTab, selectedGraph)) state
                                                

