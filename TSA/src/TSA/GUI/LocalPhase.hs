
module TSA.GUI.LocalPhase (localPhaseDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.AnalyticData as AD
import Regression.AnalyticDataWrapper as ADW
import Regression.Functions as FS
import Regression.Utils as U

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import TSA.GUI.Log
import TSA.GUI.Graph

import GUI.Widget

import Utils.Misc
import Utils.Concurrent

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import System.IO
import Graphics.Gnuplot.Simple
import System.Random

import Data.List
import qualified Data.Vector.Unboxed as V

localPhaseDialog :: StateRef -> IO ()
localPhaseDialog stateRef = do
    state <- readMVar stateRef
    let 
        parms = localPhaseParams (params state)
        commonParams = localPhaseCommonParams parms

    dialog <- dialogWithTitle state "Local phase"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    okButton <- dialogAddButton dialog "Ok" ResponseOk

    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog

    dataSetCombo <- dataSetComboNew splineAndSpectrum state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog

    periodAdjustment <- adjustmentNew (localPhasePeriod parms) 0 (2**52) 1 1 1
    periodSpin <- spinButtonNew periodAdjustment 1 10
    addWidget (Just "Period: ") periodSpin dialog

    maxPeriodAdjustment <- adjustmentNew (localPhaseMaxPeriod parms) 0 (2**52) 1 1 1
    maxPeriodSpin <- spinButtonNew maxPeriodAdjustment 1 10
    addWidget (Just "Max period: ") maxPeriodSpin dialog

    epochAdjustment <- adjustmentNew (localPhaseEpoch parms) (-(2**52)) (2**52) 1 1 1
    epochSpin <- spinButtonNew epochAdjustment 1 10
    addWidget (Just "Epoch: ") epochSpin dialog
    
    precisionAdjustment <- adjustmentNew (fromIntegral (localPhasePrecision parms)) 5 1000 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    colorMapCheck <- checkButtonNew >>= \button -> toggleButtonSetActive button (localPhaseCalcColorMap parms) >> return button
    addWidget (Just "Calculate color map: ") colorMapCheck dialog 

    normalizeCheck <- checkButtonNew >>= \button -> toggleButtonSetActive button True >> return button
    addWidget (Just "Normalize: ") normalizeCheck dialog 

    barCodeCombo <- dataSetComboNew2 dataAndSpectrum state False
    addWidget (Just "Bar code from: ") (getComboBox barCodeCombo) dialog

    phaseDispersionCheck <- checkButtonNew >>= \button -> toggleButtonSetActive button (localPhaseCalcPhaseDisp parms) >> return button
    addWidget (Just "Calculate phase dispersion: ") phaseDispersionCheck dialog 
    
    avgOverCyclesAdjustment <- adjustmentNew (fromIntegral (localPhaseAvgOverCycles parms)) 1 (2**52) 1 1 1
    avgOverCyclesSpin <- spinButtonNew avgOverCyclesAdjustment 1 0
    addWidget (Just "Avg. over no. cycles: ") avgOverCyclesSpin dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString nameEntry
                Just selectedData <- getSelectedData dataSetCombo
                period <- spinButtonGetValue periodSpin
                maxPeriod <- spinButtonGetValue maxPeriodSpin
                epoch <- spinButtonGetValue epochSpin
                precision <- spinButtonGetValue precisionSpin
                calculateColorMap <- toggleButtonGetActive colorMapCheck
                normalize <- toggleButtonGetActive normalizeCheck
                barCode <- getSelectedData barCodeCombo                
                calculatePhaseDispersion <- toggleButtonGetActive phaseDispersionCheck
                avgOverCycles <- spinButtonGetValue avgOverCyclesSpin
                widgetDestroy dialog
                
                modifyStateParams stateRef $ \params -> params {localPhaseParams = LocalPhaseParams {
                        localPhaseData = Just selectedData,
                        localPhasePeriod = period,
                        localPhaseMaxPeriod = maxPeriod,
                        localPhasePrecision = round precision,
                        localPhaseEpoch = epoch,
                        localPhaseCalcColorMap = calculateColorMap,
                        localPhaseCalcPhaseDisp = calculatePhaseDispersion,
                        localPhaseAvgOverCycles = round avgOverCycles,
                        localPhaseCommonParams = updateCommonParams name commonParams
                    }}
                
                let
                    barCodeData = case barCode of 
                        Just dataParams -> 
                            let
                                SD1 d = subData $ head $ dataSet dataParams
                            in
                                Just d
                        Nothing -> Nothing
                
                runTask stateRef "Local phase" $ localPhase stateRef selectedData period maxPeriod epoch (round precision) name calculateColorMap barCodeData calculatePhaseDispersion (round avgOverCycles) normalize
                return ()
        else
            do
                widgetDestroy dialog

calcStatistics :: Double -> Double -> Int -> StdGen -> Int -> Maybe Int -> SubData -> (Double -> IO ()) -> IO [SubData]
calcStatistics period epoch precision g no bsNo dataSet puFunc = do 
    let
        (mins, maxs) =
            case unboxSubData dataSet of
                Left s -> D.getExtrema s False
                Right ad -> ADW.getExtrema (round ((ADW.xMax1 ad - ADW.xMin1 ad) / period) * precision) (Just period) g ad
        
        minx = {-min (D.xMin1 detailedSpec) -} epoch
        mapOp = \(x, y) -> (x, snd (properFraction ((x - minx) / period)), 1)
        minima = data1 $ V.map mapOp mins
        maxima = data1 $ V.map mapOp maxs
        numCycles = 10
        (periods, amplitudes, means) = V.unzip3 $ V.generate ((min (V.length mins) (V.length maxs)) - numCycles) (\i ->
            let
                (xMin1, yMin1) = mins V.! i
                (xMin2, yMin2) = mins V.! (i + numCycles)
                (xMax, yMax) = maxs V.! i
                x = (xMin1 + xMin2) / 2
            in
                ((x, (xMin2 - xMin1) / (fromIntegral numCycles), 1), (x, (abs (yMax - yMin1)) / 2, 1), (x, (yMax + yMin1) / 2 , 1))
            )
    return [SD1 (data1 periods), SD1 (data1 amplitudes), SD1 (data1 means), SD1 minima, SD1 maxima]
        
localPhase :: StateRef -> DataParams -> Double -> Double -> Double -> Int -> String -> Bool -> Maybe D.Data -> Bool -> Int -> Bool -> IO ()
localPhase stateRef dataParams period maxPeriod epoch precision name calculateColorMap barCodeData calculatePhaseDispersion avgOverCycles normalize = do
    state <- readMVar stateRef
    g <- getStdGen 
    (currentGraphTab, _) <- getCurrentGraphTab state
    let 
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
    
    --stats <- applyToData (calcStatistics period epoch precision g) dataParams [name ++ "_periods", name ++ "_amplitudes", name ++ "_means", name ++ "_minima", name ++ "_maxima"] (\_ -> return ())
    --mapM_ (\stat -> modifyState stateRef $ (addDataParams (calculateWeights stat True) (Just (currentGraphTab, selectedGraph)))) (take 3 stats)
    let
        colorData = map (\sdp -> 
                let 
                    normalizedGroups = getNormalizedGroups (subData sdp) period epoch precision (2 * precision) normalize g
                    expandedGroups = map (\grp ->
                        let
                            (upperHalf, lowerHalf) = V.partition (\(x, f, y, w) -> f > 0.5) grp
                        in
                            (V.map (\(x, f, y, w) -> (x, f - 1, y, w)) upperHalf) V.++ grp V.++ (V.map (\(x, f, y, w) -> (x, f + 1, y, w)) lowerHalf)
                        ) normalizedGroups
                    (minZ, maxZ) = getMinMaxZ normalizedGroups normalize
                    getBarCodeValue d x =
                        let
                            minx = D.xMin1 d
                            maxx = D.xMax1 d
                            halfRange = (maxx - minx) / (4 * fromIntegral precision)
                        in
                         if D.dataLength (D.subSet1 (x - halfRange, x + halfRange) d) > 0 then maxZ else minZ 
                    
                    expandedGroupsWithBarCode = case barCodeData of
                        Just d -> map (\grp -> V.map (\(x, f, y, w) -> (x, f, if f < -0.3 then getBarCodeValue d x else y, w)) grp) expandedGroups
                        Nothing -> expandedGroups
                in
                    createSubDataParams__ (SD1 (Data3 $ foldl' (\res v -> res V.++ v) V.empty expandedGroupsWithBarCode))
            ) (dataSet dataParams)

    if calculatePhaseDispersion
        then
            do
                let
                    maxFreq = 1 / period
                    minFreq = 1 / maxPeriod
                    freqStep = (maxFreq - minFreq) / fromIntegral precision
                    freqs = [minFreq, minFreq + freqStep  .. maxFreq]
                    periods = map (\x -> 1 / x) freqs 
                appendLog stateRef ("delta_f = " ++ show freqStep)
                result <- mapM (\(no, sdp) -> do
                        let
                                dat = subData sdp
                                sampledDat = 
                                    case unboxSubData dat of 
                                        Left d -> d
                                        Right ad -> sampleAnalyticData_ ad [200000] g
                                bootstrapSet = subDataBootstrapSet sdp
                        phaseDispersions <- calcConcurrently_ (\period -> return (period, calcPhaseDispersion (SD1 sampledDat) period epoch 10 avgOverCycles True g)) periods
                        bsPhaseDispersions <- mapM (\dat -> calcConcurrently_ (\period -> return (period, calcPhaseDispersion dat period epoch 10 avgOverCycles True g)) periods) bootstrapSet 
                        let
                            SD2 s = dat
                            xmin = AD.xMin1 s
                            xmax = AD.xMin1 s
                            minByFunc (_, (d1, ng1)) (_, (d2, ng2)) = compare (d1 / (fromIntegral ng1 - 1)) (d2 / (fromIntegral ng2 - 1))
                            (minPhaseDispersionPeriod, (minD, minG)) = minimumBy minByFunc phaseDispersions
                            minPhaseDispersion = minD / (fromIntegral minG - 1)
                            minBSPeriodsAndDispersions = map (\phaseDispersions -> minimumBy minByFunc phaseDispersions) bsPhaseDispersions
                            globalDispersions = sort phaseDispersions
                            globalBSDispersions = map (\phaseDispersions -> sortVector $ V.fromList phaseDispersions) bsPhaseDispersions
                            sortedDispersions = V.map (\(period, (d, g)) -> (period, d / (fromIntegral g - 1))) $ V.fromList globalDispersions
                            sortedBSDispersions = map (\phaseDispersions -> V.map (\(period, (d, g)) -> (period, d / (fromIntegral g - 1))) phaseDispersions) globalBSDispersions
                        appendLog stateRef ("Minimum Phase dispersion for " ++ name ++ " " ++ show no ++ ", period = " ++ (show minPhaseDispersionPeriod) ++ ": " ++ (show minPhaseDispersion))
                        return $ (createSubDataParams_ 
                                    (SD1 (data1' sortedDispersions)) 
                                    (map (\sortedDispersions -> SD1 (data1' sortedDispersions)) sortedBSDispersions), 
                                ((xmax + xmin) / 2, minPhaseDispersionPeriod), 
                                (map (\(minPhaseDispersionPeriod, _) -> ((xmax + xmin) / 2, minPhaseDispersionPeriod)) minBSPeriodsAndDispersions),
                                globalDispersions, 
                                globalBSDispersions
                                )
                    ) (zip [1, 2 ..] (dataSet dataParams))
                let
                    
                        (phaseDispersions, periods, bsPeriods, globalDispersions, globalBSDispersions) = unzip5 result
                        globalPersAndDisps = map (\periodDisps@((period, _):_) ->
                            let
                                (disp, numGroups) = foldl' (\(dispSum, groupLenSum) (_, (disp, groupLen)) ->
                                        let 
                                            dispSum' = dispSum + disp
                                            groupLenSum' = groupLenSum + groupLen
                                        in
                                            dispSum' `seq` groupLenSum' `seq` (dispSum', groupLenSum')
                                    ) (0, 0) periodDisps
                            in
                                (period, disp / (fromIntegral numGroups - 1))
                            ) (transpose globalDispersions)
                        (minGlobalPhaseDispersionPeriod, minGlobalPhaseDispersion) = minimumBy (\(_, d1) (_, d2) -> compare d1 d2) globalPersAndDisps
                appendLog stateRef ("Minimum Global Phase dispersion for " ++ name ++ ", period = " ++ (show minGlobalPhaseDispersionPeriod) ++ ": " ++ (show minGlobalPhaseDispersion))
                modifyState stateRef $ addDataParams (createDataParams_ (name ++ "_gdisp") [createSubDataParams__ (SD1 (data1' (V.fromList globalPersAndDisps)))]) (Just (currentGraphTab, selectedGraph))
                --modifyState stateRef $ addDataParams (DataParams {dataName = name ++ "_disp", dataSet = phaseDispersions}) (Just (currentGraphTab, selectedGraph))
                --modifyState stateRef $ addDataParams (calculateWeights (DataParams {dataName = name ++ "_periods", 
                --        dataSet = [SubDataParams {
                --                subData = Left (D.data1' (V.fromList periods)), 
                --                subDataBootstrapSet = map (\periods -> Left (D.data1' (V.fromList periods))) (transpose bsPeriods)}]
                --                }) True) (Just (currentGraphTab, selectedGraph))
                 
        else
            return ()

    currentGraphTab <- addGraphTab stateRef Nothing
    state <- readMVar stateRef
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms

    if calculateColorMap 
        then
            do
                mapM_ (\(i, sdp) -> do
                    let 
                        (SD1 d) = subData sdp
                        vals = V.toList $ D.xys2 d
                        (minX, _, _) = head vals
                        toStr (x1, x2, y) = show (x1 - minX) ++ " " ++ show x2 ++ " " ++ show y ++ "\n"
                        str = (concatMap (\(val@(x1, x2, y), (x1Next, _, _)) -> toStr val ++ (if x1Next /= x1 then "\n" else "")) (zip (init vals) (tail vals))) ++ toStr (last vals)
                    handle <- openFile ("segment" ++ show i ++ ".csv") WriteMode
                    hPutStr handle str
                    hClose handle
                    ) (zip [1, 2 ..] colorData)
                
                modifyState stateRef $ (addDataParams (createDataParams_ name colorData) (Just (currentGraphTab, selectedGraph)))
        else
            return ()
    --mapM_ (\stat -> modifyState stateRef $ (addDataParams stat (Just (currentGraphTab, selectedGraph)))) (drop 3 stats)

    return ()

    
calcPhaseDispersion :: (RandomGen g) => SubData -> Double -> Double -> Int -> Int -> Bool -> g -> (Double, Int)
calcPhaseDispersion dat period epoch precision avgOverCycles normalize g =
    let 
        (range, weightFact) = 
            case unboxSubData dat of
                Left d -> (D.xMax1 d - D.xMin1 d, 1 / period)
                Right ad -> (ADW.xMax1 ad - ADW.xMin1 ad, 1)
        normalizedGroups = getNormalizedGroups dat period epoch precision (round (range / period)) normalize g
        avgGroups groups = 
            if length groups >= avgOverCycles 
                then foldl' (\sums grp -> V.zipWith (\sum (_, _, y, _) -> sum + y) sums grp) (V.replicate precision 0) (take avgOverCycles groups):avgGroups (drop (max (avgOverCycles `div` 2) 1) groups) 
                else []
        func groups = (sum (zipWith (\group1 group2 ->
            let 
                (diffs, y2s) = V.unzip $ V.zipWith (\y1 y2 -> ((y1 - y2) ^ 2, (max y1 y2) ^ 2)) group1 group2
            in
                (V.sum diffs) / (V.sum y2s) / (fromIntegral (min (V.length group1) (V.length group2)))) (init groups) (tail groups)), length groups)
        --sampleGroups groups = map (\i -> groups !! i) [0, avgOverCycles `div` 2 - 1 .. length groups - 1]
        --func groups = (sum (zipWith (\group1 group2 -> (sum (zipWith (\(_, _, y1, _) (_, _, y2, _) -> (y1 - y2) ^ 2) group1 group2)) / (fromIntegral (min (length group1) (length group2)))) (sampleGroups (take (length groups - avgOverCycles) groups)) (sampleGroups (drop avgOverCycles groups))) / period, length groups) 
        --func groups = (sum (zipWith (\group1 group2 -> (sum (zipWith (\(_, _, y1, _) (_, _, y2, _) -> (y1 - y2) ^ 2) group1 group2)) / (fromIntegral (min (length group1) (length group2)))) (init groups) (tail groups)) / period, length groups) 
        lens = map V.length normalizedGroups
     in
        func (avgGroups normalizedGroups)

getNormalizedGroups :: (RandomGen g) => SubData -> Double -> Double -> Int -> Int -> Bool -> g -> [V.Vector (Double, Double, Double, Double)]
getNormalizedGroups dat period epoch numSamplesPerCycle numCycles normalize g =
    let 
        minx = {-min (D.xMin1 detailedSpec) -} epoch
        -- Calculate color map (using less precision)
        spec = 
            case unboxSubData dat of
                Left d -> d
                Right ad -> sample2dAnalyticData ad (numSamplesPerCycle, period, numCycles) g

        --minx = min (D.xMin1 spec) epoch
        vals = {-trace ("vals: " ++ show (D.xys1 spec))-} (D.xys1 spec) 
        precFact = fromIntegral numSamplesPerCycle * 100

        colorSpecFn (x, y) =
            let
                (n, f) = properFraction ((x - minx) / period)
                roundedF = (fromIntegral (floor (f * precFact))) / precFact
            in
                if f > 1 - 1 / precFact
                    then
                        (minx + fromIntegral (n + 1) * period, 0, y, 1)
                    else
                        (minx + fromIntegral n * period, roundedF, y, 1)
        colorSpec = V.map colorSpecFn vals
        normalizedGroups =
            let
                groups = map (\v -> if V.length v > numSamplesPerCycle then V.generate numSamplesPerCycle (\i -> v V.! (floor (fromIntegral i * (fromIntegral (V.length v)) / (fromIntegral numSamplesPerCycle)))) else v) $ groupVectorBy (\(x1, _, _, _) (x2, _, _, _) -> x1 == x2) colorSpec
            in
                if normalize
                    then            
                            map (\vals ->
                                let
                                    (_, _, yMin, _) = V.minimumBy (\(_, _, y1, _) (_, _, y2, _) -> compare y1 y2) vals
                                    (_, _, yMax, _) = V.maximumBy (\(_, _, y1, _) (_, _, y2, _) -> compare y1 y2) vals
                                    yDiff = 1 / (yMax - yMin)
                                    clip y = if y > 1 then 1 else if y < -1 then -1 else y
                                in
                                    V.map (\(x1, x2, y, w) -> (x1, x2, clip (2 * ((y - yMin) * yDiff - 0.5)), w)) vals
                                ) groups 
                    else
                        groups        
    in
        normalizedGroups 

getMinMaxZ :: [V.Vector (Double, Double, Double, Double)] -> Bool -> (Double, Double)
getMinMaxZ groups normalize = 
        let
                getMaxZIngroup = V.maximumBy (\(_, _, z1, _) (_, _, z2, _) -> compare z1 z2)
                getMinZIngroup = V.minimumBy (\(_, _, z1, _) (_, _, z2, _) -> compare z1 z2)
                maxZs = map (getMaxZIngroup) groups
                minZs = map (getMinZIngroup) groups
                (_, _, maxZ, _) = maximumBy (\(_, _, z1, _) (_, _, z2, _) -> compare z1 z2) maxZs
                (_, _, minZ, _) = minimumBy (\(_, _, z1, _) (_, _, z2, _) -> compare z1 z2) minZs
        in
                if normalize then (-1, 1) else (minZ, maxZ)
                