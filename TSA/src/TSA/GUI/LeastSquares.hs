module TSA.GUI.LeastSquares  (paramsDialog) where

import Graphics.UI.Gtk hiding (addWidget, Plus)
--import qualified Data.Map as Map
--import Data.IORef
--import Control.Concurrent
import Debug.Trace

import Math.Function as F
import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.Utils as U
import Regression.AnalyticData as AD
import Regression.Bootstrap as B

import TSA.Params
import TSA.LeastSquares
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log

import Utils.Misc
import Utils.Xml

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import Math.Expression
import Math.Statistics
import System.Random

import qualified Data.Vector.Unboxed as V
import Statistics.Test.KolmogorovSmirnov
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Types
import qualified Statistics.Sample as Sample

import qualified Utils.Xml as Xml

paramsDialog :: StateRef -> IO ()
paramsDialog stateRef = do
    state <- readMVar stateRef
    let
        parms = lsqParams (params state)
        fitParams = lsqFitParams parms
        commonParams = fitCommonParams fitParams
    dialog <- dialogWithTitle state "Least squares fit"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    fitWidgets@(FitWidgets nameEntry rankSpin periodSpin harmonicsSpin typeCombo _ _ _) <- addFitWidgets fitParams state dialog

    dataSetCombo <- dataSetComboNew dataAndSpectrum state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog
    
    bootstrapCountAdjustment <- adjustmentNew (fromIntegral (lsqBootstrapCount parms)) 0 1000 1 1 1
    bootstrapCountSpin <- spinButtonNew bootstrapCountAdjustment 1 0
    addWidget (Just "Bootstrap count: ") bootstrapCountSpin dialog
    
    let 
        toggleFitButton :: IO ()
        toggleFitButton = 
            do
                selectedData <- getSelectedData dataSetCombo
                fitName <- entryGetText nameEntry
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
                Just selectedData <- getSelectedData dataSetCombo
                bootstrapCount <- spinButtonGetValue bootstrapCountSpin
                
                newFitParams <- getFitParams fitWidgets fitParams
                
                name <- entryGetText nameEntry
                
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {lsqParams = LsqParams {
                    lsqFitParams = newFitParams,
                    lsqBootstrapCount = round bootstrapCount
                }}
                forkIO $ fit stateRef selectedData name
                return ()
        else
            do
                widgetDestroy dialog


fit :: StateRef -> DataParams -> String -> IO ()
fit stateRef dataParams fitName = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms

        lsqParms = lsqParams (params state)
        bootstrapCount = lsqBootstrapCount lsqParms
        
        func i maybeJ (Left dat) puFunc = do 
            spline <- fitData (lsqFitParams lsqParms) dat puFunc 
            g <- getStdGen 
            let
                Left diff = U.binaryOp (F.subtr) (Left dat) (Right (Left spline)) True g
                Left squareDiff = U.binaryOp (F.subtr) (Left diff) (Left diff) True g
                diffVals = D.values1 diff
                datVals = D.values1 dat
                datMean = Sample.meanWeighted (V.map (\(x, y, w) -> (y, w)) datVals)
                datVar = V.sum $ V.map (\(x, y, w) -> (y - datMean) ^ 2) datVals
                diffVar = V.sum $ V.map (\(x, y, w) -> y * y) diffVals
                n = fromIntegral $ D.dataLength dat
                bic = n * Prelude.log (diffVar / (n - 1)) + degFreedom * Prelude.log n
                r2 = 1 - diffVar / datVar
                degFreedom = fromIntegral $ getDegreesOfFreedom (lsqFitParams lsqParms)
                redChiSquared = (V.sum $ V.map (\(x, y, w) -> y * y * w) diffVals) / (n - degFreedom - 1)
                diffSample = D.ys diff
                normal = normalFromSample diffSample
                dist = D.data1 $ V.map (\(x, y) -> (x, y, 1)) (cumulProbDist_ diffSample)
            appendLog stateRef ("Results for " ++ fitName ++ " " ++ show i ++ (case maybeJ of Just j -> ", " ++ show j; Nothing -> "") ++  ":")
            appendLog stateRef ("stdev residuals = " ++ (show (stdDev normal)))
            appendLog stateRef ("num parameters = " ++ (show degFreedom))
            appendLog stateRef ("chi-squared = " ++ (show redChiSquared))
            appendLog stateRef ("R^2 = " ++ show r2)
            appendLog stateRef ("BIC = " ++ show bic)
            --appendLog stateRef ("KS statistic D = " ++ (show (kolmogorovSmirnovD normal diffSample)))
            --modifyState stateRef $ addData (Left dist) (fitName ++ "_residueDist") (Just (currentGraphTab, selectedGraph))
            return $ Right $ Left spline
    fitDataParams <- applyToData1 func dataParams fitName (progressUpdate stateRef)
    modifyState stateRef $ (addDataParams fitDataParams (Just (currentGraphTab, selectedGraph)))

    if bootstrapCount > 0
        then do
            Xml.renderToFile (Xml.toDocument (lsqFitParams lsqParms)) ("fitdata")
            mapM_ (\(i, fitParams, dataParams)  ->
                    do
                        let
                            Right (Left spline) = subData fitParams
                            Left dat = subData dataParams
                        Xml.renderToFile (Xml.toDocument spline) ("spline" ++ show i)
                        Xml.renderToFile (Xml.toDocument dat) ("data" ++ show i)
                ) (zip3 [1, 2 ..] (dataSet fitDataParams) (dataSet dataParams))
                
{-
            let
                numSubData = length $ dataSet dataParams
            subDataParams <- mapM (\(i, fitParams, dataParams)  ->
                    do
                        let
                            Right (Left spline) = subData fitParams
                            Left dat = subData dataParams
                        Xml.renderToFile (Xml.toDocument spline) ("spline" ++ show i)
                        Xml.renderToFile (Xml.toDocument dat) ("data" ++ show i)
                        bsSplines <- B.bootstrapSplines bootstrapCount (fitData (lsqFitParams lsqParms)) spline dat (\pct -> progressUpdate stateRef (pct * fromIntegral i / fromIntegral numSubData))
                        --mapM (\(j, bsSpline) -> 
                        --        modifyState stateRef $ addDataParams (DataParams {
                        --            dataName = fitName ++ "_b" ++ show j,
                        --            dataSet = [
                        --                SubDataParams {
                        --                    subData = Right (Left bsSpline),
                        --                    subDataBootstrapSet = []
                        --                }
                        --            ]
                        --        }) (Just (currentGraphTab, selectedGraph)) 
                        --    ) (zip [1 ..] bsSplines)
                        let
                            (upperSpline, lowerSpline) = upperLowerSplines spline bsSplines
                        return fitParams {subDataBootstrapSet = map (\spline -> Right (Left spline)) bsSplines}
                ) (zip3 [1, 2 ..] (dataSet fitDataParams) (dataSet dataParams))
            modifyMVar_ stateRef $ \state -> 
                do
                let
                    dataParams = getDataByName fitName state
                return $ updateData (dataParams {dataSet = subDataParams}) state
-}
            progressUpdate stateRef 0
        else do
            progressUpdate stateRef 0
            return ()

getDegreesOfFreedom :: FitParams -> Int
getDegreesOfFreedom fitParams =
        let
                rank = fitPolynomRank fitParams
                numHarmonics = fitNumHarmonics fitParams
        
        in
        case fitType fitParams of
                FitTypeSpline ->
                        let
                                numNodes = splineNumNodes (fitSplineParams fitParams)
                        in
                                (3 * (rank + 1)) * numNodes  * numHarmonics
                FitTypeHarmonic -> 
                        let
                                numModulators = harmonicCount (fitHarmonicParams fitParams)
                     in
                                (2 + 4 * numModulators) * numHarmonics + 2 * numModulators + 1

