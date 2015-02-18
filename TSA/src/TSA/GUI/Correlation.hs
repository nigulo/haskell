module TSA.GUI.Correlation (correlationDialog) where

import Graphics.UI.Gtk hiding (addWidget)

import Regression.Data as D
import Regression.Utils as U
import Regression.AnalyticData as AD

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import GUI.Widget

import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative
import Debug.Trace

import Statistics.LinearRegression
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Test.KolmogorovSmirnov
import Statistics.Resampling.Bootstrap
import System.Random
import System.Random.MWC

correlationDialog :: StateRef -> IO ()
correlationDialog stateRef = do
    state <- readMVar stateRef
    
    let
        parms = correlationParams (params state)
        commonParams = correlationCommonParams parms
    
    g <- getStdGen 
    (currentGraphTab, _) <- getCurrentGraphTab state

    dialog <- dialogWithTitle state "Find correlation"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    
    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog
    
    dataSetCombo1 <- dataSetComboNew only2d state
    addWidget (Just "Data set 1: ") (getComboBox dataSetCombo1) dialog

    dataSetCombo2 <- dataSetComboNew only2d state
    addWidget (Just "Data set 2: ") (getComboBox dataSetCombo2) dialog

    precisionAdjustment <- adjustmentNew (fromIntegral (correlationPrecision parms)) 1 100000000 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString nameEntry
                precision <- spinButtonGetValue precisionSpin
                
                Just selectedData1 <- getSelectedData dataSetCombo1
                Just selectedData2 <- getSelectedData dataSetCombo2
                widgetDestroy dialog

                let
                    Left d1 = subData $ head $ dataSet selectedData1
                    Left d2 = subData $ head $ dataSet selectedData2
                    graphTabParms = (graphTabs state) !! currentGraphTab
                    selectedGraph = graphTabSelection graphTabParms

                modifyStateParams stateRef $ \params -> params {correlationParams = CorrelationParams {
                        correlationPrecision = round precision,
                        correlationCommonParams = updateCommonParams name commonParams
                    }}

                forkIO $ findCorrelation stateRef selectedData1 selectedData2 precision name
                return ()
        else
            do
                widgetDestroy dialog

findCorrelation :: StateRef -> DataParams -> DataParams -> Double -> String -> IO ()
findCorrelation stateRef dataParams1 dataParams2 precision name = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms

        getRange dataParams =
            case subData $ head $ dataSet dataParams of
                Left dat -> (D.xMin1 dat, D.xMax1 dat) 
                Right (Left spline) -> (AD.xMin1 spline, AD.xMax1 spline)
                Right (Right fns) -> (AD.xMin1 fns, AD.xMax1 fns)

        

        (xMin1, xMax1) = getRange dataParams1
        (xMin2, xMax2) = getRange dataParams2

        xMin = max xMin1 xMin2
        xMax = min xMax1 xMax2
    if xMin < xMax then 
        do
            let
                maybeYs sdp1 sdp2 = 
                    case (subData sdp1, subData sdp2) of
                        (Left d1, Left d2) ->
                            let 
                                subD1 = D.subSet1 (xMin, xMax) d1  
                                subD2 = D.subSet1 (xMin, xMax) d2  
                            in
                            if D.xs subD1 == D.xs subD2 then Just (D.ys subD1, D.ys subD2) else Nothing
                        otherwise -> Nothing
                (ys1, ys2) = 
                    case maybeYs (head $ dataSet dataParams1) (head $ dataSet dataParams2) of
                        Just (ys1, ys2) -> (ys1, ys2)
                        otherwise ->
                            let
                                step =  (xMax - xMin) / precision
                                xs = map (\x -> [x]) (init [xMin, xMin + step .. xMax] ++ [xMax])
                                (_, ys1) = unzip $ U.getValues xs (subData $ head $ dataSet dataParams1)
                                (_, ys2) = unzip $ U.getValues xs (subData $ head $ dataSet dataParams2)
                            in
                                (V.fromList ys1, V.fromList ys2)
                (alpha, beta, r2) = linearRegressionRSqr ys1 ys2
                (alphaDisp, betaDist) = linearRegressionDistributions (alpha, beta) ys1 ys2
                Just stdBeta = maybeStdDev betaDist
                stdYs1 = stdDev $ normalFromSample ys1
                stdYs2 = stdDev $ normalFromSample ys2
                stdR = stdBeta * stdYs1 / stdYs2
            rndVects <- mapM (\_ -> do rndVect :: V.Vector Int <- withSystemRandom . asGenST $ \gen -> uniformVector gen (V.length ys1); return rndVect) [0 .. 999]
            let
                resamples = map (\rndVect -> V.unzip (V.map (\r -> let i = r `mod` (V.length ys1) in (ys1 V.! i, ys2 V.! i)) rndVect)) rndVects
                correls = Data.List.sort $ map (\(ys1, ys2) -> correl ys1 ys2) resamples
                --stdCorrels = stdDev $ normalFromSample (V.fromList correls)
                ksStat = kolmogorovSmirnovD (normalFromSample (V.fromList correls)) (V.fromList correls) 
            appendLog stateRef ("alpha, beta" ++ show alpha ++ ", " ++ show beta)
            appendLog stateRef ("Correlation coefficient for " ++ name ++ ": " ++ (show (signum beta * sqrt r2)) ++ ", 90% confInt=[" ++ (show (correls !! 49)) ++ ", " ++ (show (correls !! 949)) ++ "]")
            modifyState stateRef $ addData (Left (D.data1' (V.zip ys1 ys2))) name (Just (currentGraphTab, selectedGraph))
        else return ()

