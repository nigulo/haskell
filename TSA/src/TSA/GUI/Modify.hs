
module TSA.GUI.Modify (modifyDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Graphics.UI.Gtk.Layout.VBox
import qualified Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.AnalyticData as AD
import Regression.Data as D
import Regression.Utils as U
import qualified Math.Function as F
import qualified Math.Expression as E

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget
import Astro.Ephem.Types

import Utils.Misc

import Data.IORef
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import Data.List
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative

import System.Random

modifyDialog :: StateRef -> IO ()
modifyDialog stateRef = do
    state <- readMVar stateRef
    
    g <- getStdGen 
    (currentGraphTab, _) <- getCurrentGraphTab state

    let
        parms = modifyParams (params state)
        commonParams = modifyCommonParams parms
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
    
    dialog <- dialogWithTitle state "Modify data set"
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    vBox <- castToBox <$> dialogGetContentArea dialog
    
    nameEntry <- entryNew
    nameEntry `entrySetText` (getNameWithNo commonParams)
    addWidget (Just "Name: ") nameEntry dialog
    
    dataSetCombo1 <- dataSetComboNew2 (\_ -> True) state False
    addWidget (Just "Data set 1: ") (getComboBox dataSetCombo1) dialog

    opCombo <- createComboBox [
        "Add", 
        "Subtract", 
        "Mean", 
        "Multiply", 
        "Invert", -- only 2d data
        "Union",
        "Intersection",
        "Complement",
        "Join",
        "Split",
        "Segment by markers",
        "Segment by count",
        "Segment by gaps",
        "Unsegment",
        "Set weights",
        "Moving average",
        "JD to year",
        "Year to JD",
        "Round",
        "Remove neighbour",
        "Find neighbour",
        "Scale",
        "Add noise",
        "Interpolate"
        ]
    comboBoxSetActive opCombo (modifyOp parms)
    addWidget (Just "Operation: ") opCombo dialog

    typeCombo <- createComboBox ["y", "x"]
    comboBoxSetActive typeCombo (modifyType parms)
    addWidget (Just "Type: ") typeCombo dialog

    dataSetCombo2 <- dataSetComboNew2 (\_ -> True) state False
    addWidget (Just "Data set 2: ") (getComboBox dataSetCombo2) dialog

    constantAdjustment <- adjustmentNew (modifyConstant parms) 0 (2**52) 1 1 1
    constantSpin <- spinButtonNew constantAdjustment 1 10
    addWidget (Just "Constant: ") constantSpin dialog

    constant2Adjustment <- adjustmentNew (modifyConstant parms) 0 (2**52) 1 1 1
    constant2Spin <- spinButtonNew constant2Adjustment 1 10
    addWidget (Just "Constant 2: ") constant2Spin dialog

    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetText nameEntry
                
                selectedData1 <- getSelectedData dataSetCombo1
                selectedData2 <- getSelectedData dataSetCombo2
                Just op <- comboBoxGetActiveText opCombo
                opNo <- comboBoxGetActive opCombo
                Just t <- comboBoxGetActiveText typeCombo
                typeNo <- comboBoxGetActive typeCombo
                constant <- spinButtonGetValue constantSpin
                constant2 <- spinButtonGetValue constant2Spin
                widgetDestroy dialog

                let
                    graphTabParms = (graphTabs state) !! currentGraphTab
                    selectedGraph = graphTabSelection graphTabParms
                    segments = graphSegments ((graphTabGraphs graphTabParms) !! selectedGraph)
                    modify (name, selectedData1) = 
                        case selectedData2 of
                            Just sd2 ->
                                if (op == "Union") || (op == "Intersection") || op == "Complement"
                                    then
                                        if isDiscrete selectedData1 && isDiscrete sd2 then do
                                                let
                                                    func i _ (Left ds1) _ = do
                                                        let
                                                            Left ds2 = getSubDataAt sd2 i
                                                            func = case op of
                                                                "Union" -> union
                                                                "Intersection" -> intersect
                                                                "Complement" -> (\\)
                                                            vals = V.fromList $ func (V.toList (D.values1 ds1)) (V.toList (D.values1 ds2)) 
                                                        return $ Left (D.data1 vals)
                                                                                
                                                result <- applyToData1 func selectedData1 name (progressUpdate stateRef)
                                                modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                            else return ()
                                else if (op == "Join")
                                    then
                                        modifyState stateRef $ addDataParams (merge name selectedData1 sd2) (Just (currentGraphTab, selectedGraph))
                                else if op == "Segment by data points" then
                                        if isDiscrete selectedData1 && isDiscrete sd2 then do
                                            let
                                                Left d2 = subData $ head $ dataSet sd2
                                                filteredSegments = V.toList $ D.xs1 d2
                                                mapOp sdp =
                                                    let
                                                        Left d = subData sdp 
                                                        xMin = D.xMin1 d
                                                        xMax = D.xMax1 d
                                                        ds = zipWith (\xLeft xRight -> SubDataParams {subDataRange = ([xLeft], [xRight]), subData = Left (D.subSet1 (xLeft, xRight) d), subDataBootstrapSet = []}) (xMin:filteredSegments) (filteredSegments ++ [xMax])
                                                    in
                                                        ds
                                            modifyState stateRef $ addDataParams (DataParams {dataName = name, dataSet = concat (map mapOp (dataSet selectedData1))}) (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                else if (take 7 op == "Segment") 
                                    then do
                                            let
                                                d = subData $ head $ dataSet selectedData1
                                                minMax sdp = case subData sdp of
                                                    Left d -> (D.xMins d, D.xMaxs d)
                                                    Right (Left s) -> (AD.xMins s, AD.xMaxs s)  
                                                    Right (Right f) -> (AD.xMins f, AD.xMaxs f)
                                                mapOp (minX, maxX) = 
                                                    SubDataParams {
                                                        subDataRange = (minX, maxX),
                                                        subData = case d of
                                                            Left d -> Left (D.subSet1 (head minX, head maxX) d)
                                                            Right (Left (AnalyticData [(_, _, s)])) -> Right $ Left (AnalyticData [(minX, maxX, s)])
                                                            Right (Right (AnalyticData [(_, _, f)])) -> Right $ Right (AnalyticData [(minX, maxX, f)]), 
                                                        subDataBootstrapSet = []
                                                    }
                                                minsMaxs = map minMax (dataSet sd2)
                                                gaps = zipWith (\(_, minX) (maxX, _) -> (minX, maxX)) (init minsMaxs) (tail minsMaxs)
                                                result = if op == "Segment by gaps" then map mapOp gaps else map mapOp minsMaxs 
                                            modifyState stateRef $ addDataParams (DataParams {dataName = name, dataSet = result}) (Just (currentGraphTab, selectedGraph))
                                else if op == "Find neighbour" then
                                        if isDiscrete selectedData1 && isDiscrete sd2 then do
                                            let
                                                Left d1 = subData $ head $ dataSet selectedData1
                                                Left d2 = subData $ head $ dataSet sd2
                                                mapFunc val@(x1, y1, _) = 
                                                    let
                                                        vals2 = V.toList $ D.values1 d2
                                                        nearestVals = filter (\(x2, y2, _) -> abs (x1 - x2) <= constant && abs (y1 - y2) <= constant2) vals2
                                                    in
                                                        if null nearestVals
                                                        then Nothing
                                                        else Just val
                                                nearest = Left (D.data1 (V.fromList (catMaybes (map mapFunc (V.toList (D.values1 d1))))))
                                            modifyState stateRef $ addData nearest name (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                else do
                                    let
                                        mapOp sdp1 sdp2 = SubDataParams {
                                                subDataRange = subDataRange sdp1,
                                                subData = U.binaryOp (getOp op) (subData sdp1) (subData sdp2) (t == "y") g, 
                                                subDataBootstrapSet = []
                                            }
                                        result = zipWith mapOp (dataSet selectedData1) (dataSet sd2) 
                                    modifyState stateRef $ addDataParams (DataParams {dataName = name, dataSet = result}) (Just (currentGraphTab, selectedGraph))
                            Nothing -> 
                                case op of
                                    "Invert" ->
                                        if isDiscrete selectedData1 then do
                                                let
                                                    func i j (Left ds1) _ =
                                                        do
                                                        let
                                                            (xs, ys, ws) = V.unzip3 $ D.values1 ds1
                                                        return $ Left (D.data1 (V.zip3 ys xs ws))
                                                                                
                                                result <- applyToData1 func selectedData1 name (progressUpdate stateRef)
                                                modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                            else return ()
                                    "Split" ->
                                        mapM_ (\(i, sdp) -> 
                                            modifyState stateRef $ addDataParams (DataParams {dataName = name ++ show i, dataSet = [sdp]}) (Just (currentGraphTab, selectedGraph))
                                            ) (zip [1, 2 ..] (dataSet selectedData1))
                                    "Segment by markers" ->
                                        if isDiscrete selectedData1 && segments /= [] then do
                                            let
                                                sortedSegments1 = sort segments
                                            print sortedSegments1
                                            let
                                                mapOp sdp =
                                                    let
                                                        Left d = subData sdp 
                                                        xMin = D.xMin1 d
                                                        xMax = D.xMax1 d
                                                        sortedSegments = 
                                                            if xMin < head sortedSegments1 then [xMin] else [] ++
                                                            sortedSegments1 ++
                                                            if xMax > last sortedSegments1 then [xMax] else [] 
                                                        ds = zipWith (\xLeft xRight ->
                                                                let
                                                                    seg = D.subSet1 (xLeft, xRight) d
                                                                in
                                                                    SubDataParams {
                                                                            subDataRange = ([xLeft], [xRight]),
                                                                            subData = Left seg, 
                                                                            subDataBootstrapSet = []
                                                                        }
                                                            ) (init sortedSegments) (tail sortedSegments)
                                                    in
                                                        ds
                                            modifyState stateRef $ addDataParams (DataParams {dataName = name, dataSet = concat (map mapOp (dataSet selectedData1))}) (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    "Segment by count" ->
                                        if isDiscrete selectedData1 then do
                                            let
                                                mapOp sdp =
                                                    let
                                                        Left d = subData sdp 
                                                        xMin = D.xMin1 d
                                                        xMax = D.xMax1 d
                                                        subSetLength = (xMax - xMin) / constant
                                                        ds = map (\xLeft -> 
                                                                let
                                                                    seg = D.subSet1 (xLeft, xLeft + subSetLength) d
                                                                in
                                                                    if dataLength seg > 0 then Just (SubDataParams {
                                                                            subDataRange = ([xLeft], [xLeft + subSetLength]),
                                                                            subData = Left seg, 
                                                                            subDataBootstrapSet = []
                                                                        }) else Nothing
                                                            ) [xMin, xMin + subSetLength .. xMax - subSetLength]
                                                    in
                                                        catMaybes ds
                                            modifyState stateRef $ addDataParams (DataParams {dataName = name, dataSet = concat (map mapOp (dataSet selectedData1))}) (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    "Segment by gaps" ->
                                        if isDiscrete selectedData1 then do
                                            let
                                                splitByGaps (Left d) = splitByGaps' (D.xMin1 d) (V.toList (D.values1 d)) [] where
                                                    splitByGaps' _ [] segment = 
                                                        let
                                                            d = data1 (V.fromList segment)
                                                        in
                                                            [SubDataParams {
                                                                subDataRange = (D.xMins d, D.xMaxs d),
                                                                subData = Left d, 
                                                                subDataBootstrapSet = []
                                                            }]
                                                    splitByGaps' lastX ((x, y, w):vals) segment =
                                                        if (x - lastX > constant) 
                                                            then 
                                                                let
                                                                    d = data1 (V.fromList segment)
                                                                in
                                                                    SubDataParams {
                                                                            subDataRange = (D.xMins d, D.xMaxs d),
                                                                            subData = Left d, 
                                                                            subDataBootstrapSet = []
                                                                        }:splitByGaps' x vals [(x, y, w)]
                                                            else splitByGaps' x vals (segment ++ [(x, y, w)])
                                            modifyState stateRef $ addDataParams (DataParams {dataName = name, dataSet = splitByGaps (subData (head (dataSet selectedData1)))}) (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    "Unsegment" ->
                                        if isDiscrete selectedData1 then do
                                            let
                                                unsegment (SubDataParams _ (Left d) _:[]) = D.values1 d
                                                unsegment (SubDataParams _ (Left d) _:sdps) = D.values1 d V.++ unsegment sdps
                                            modifyState stateRef $ addDataParams (DataParams {
                                                dataName = name, 
                                                dataSet = 
                                                    let 
                                                        d = D.data1 (sortVectorBy (\(x1, _, _) (x2, _, _) -> compare x1 x2) (unsegment (dataSet selectedData1)))
                                                    in
                                                        [SubDataParams {
                                                            subDataRange = (D.xMins d, D.xMaxs d),
                                                            subData = Left d,
                                                            subDataBootstrapSet = []
                                                        }]
                                            }) (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    "Set weights" ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> return (Left (D.setW (V.replicate (D.dataLength dat) constant) dat))) selectedData1 name (progressUpdate stateRef)
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    --"Moving average" ->
                                    "JD to year" ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> return (Left (D.data1 (V.map (\(x, y, w) -> let TropicalYears year = toTropicalYears (JD x) in (year, y, w) ) (D.values1 dat))))) selectedData1 name (progressUpdate stateRef)
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    "Year to JD" ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> return (Left (D.data1 (V.map (\(x, y, w) -> let JD jd = toJD (TropicalYears x) in (jd, y, w) ) (D.values1 dat))))) selectedData1 name (progressUpdate stateRef)
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    "Round" ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> 
                                                return (Left (D.data1 (V.map (\(x, y, w) -> 
                                                    let 
                                                        e = 10 ^ (round constant) 
                                                    in 
                                                 if (t == "y") then (x, (fromIntegral (round (y * e))) / e, w) 
                                                        else ((fromIntegral (round (x * e))) / e, y, w)) (D.values1 dat))))) selectedData1 name (progressUpdate stateRef)
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    "Remove neighbour" ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> do
                                                let 
                                                        filterFunc [] res (Just lastVal) = res ++ [lastVal]
                                                        filterFunc (val:vals) [] Nothing = filterFunc vals [] (Just val)
                                                        filterFunc (val@(x, y, _):vals) res (Just lastVal@(lastX, lastY, _)) = 
                                                            if (t == "y") && x > lastX + constant || (t == "x") && x < lastX + constant 
                                                                then filterFunc vals (res ++ [lastVal]) (Just val) 
                                                                else 
                                                                    if y > lastY 
                                                                        then filterFunc vals res (Just val)
                                                                        else filterFunc vals res (Just lastVal)
                                                                          
                                                return (Left (D.data1 (V.fromList (filterFunc (V.toList (D.values1 dat)) [] Nothing))))
                                                        ) selectedData1 name (progressUpdate stateRef)
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    "Scale" ->
                                        if isDiscrete selectedData1 && constant /= 0 
                                        then do
                                            let 
                                                func i j d _ = 
                                                    do
                                                        let
                                                            Left dat = d 
                                                            (maxVal, minVal) = if (t == "y") then (D.yMax dat, D.yMin dat) else (D.xMax1 dat, D.xMin1 dat)
                                                            f = F.function ("(x-" ++ show minVal ++")*" ++ show constant ++ "/(" ++ show (maxVal - minVal) ++ ")")
                                                        return $ constantOp f d 0 (t == "y")
                                            result <- applyToData1 func selectedData1 name (progressUpdate stateRef)
                                            modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                        else
                                            return ()
                                    "Add noise" ->
                                        if isDiscrete selectedData1 && constant /= 0 
                                        then do
                                            let 
                                                func i j d _ = 
                                                    do
                                                        let
                                                            Left dat = d 
                                                            newVals = zipWith (\(x, y, w) r -> if (t == "y") then (x, y + r, w) else (x + r, y, w)) (V.toList (D.values1 dat)) (randomRs (-constant, constant) g)
                                                        return $ Left (D.data1 (V.fromList newVals))
                                            result <- applyToData1 func selectedData1 name (progressUpdate stateRef)
                                            modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                        else
                                            return ()
                                    "Interpolate" ->
                                        if isDiscrete selectedData1 && constant > 0
                                        then do
                                            let 
                                                func i j d _ = 
                                                    do
                                                        let
                                                            Left dat = d 
                                                            xVals = D.xs1 dat
                                                            xMin = D.xMin1 dat
                                                            xMax = D.xMax1 dat
                                                            step = (xMax - xMin) / constant
                                                            xVals1 = V.generate (round constant) (\i -> xMin + fromIntegral i * step) -- V.fromList [xMin, xMin + step .. xMax]
                                                            --xVals1 = (V.concatMap (\(x1, x2) -> V.init (V.fromList [x1, x1 + (x2 - x1) / constant .. x2])) (V.zip (V.init xVals) (V.tail xVals))) `V.snoc` (V.last xVals)
                                                            newDat = D.interpolatedData1 xVals1 dat
                                                        return $ Left newDat
                                            result <- applyToData1 func selectedData1 name (progressUpdate stateRef)
                                            modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                        else
                                            return ()
                                    otherwise ->
                                        if constant /= 0 
                                        then do
                                            let func i j d _ = return $ constantOp (getOp op) d constant (t == "y")
                                            result <- applyToData1 func selectedData1 name (progressUpdate stateRef)
                                            modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                        else
                                            return ()
                mapM_ modify (case selectedData1 of 
                        Just dat -> [(name, dat)]
                        Nothing -> map (\gp -> (graphDataParamsName gp ++ "_" ++ name, getDataByName (graphDataParamsName gp) state)) $ graphData graphParms
                    )

                modifyStateParams stateRef $ \params -> params {modifyParams = ModifyParams {
                        modifyCommonParams = updateCommonParams name commonParams,
                        modifyOp = opNo,
                        modifyType = typeNo,
                        modifyConstant = constant
                    }}
                return ()
        else
            do
                widgetDestroy dialog

getOp :: String -> F.Function Double
getOp "Add" = F.add
getOp "Subtract" = F.subtr
getOp "Mean" = (F.function "(x+y)/2")
getOp "Multiply" = F.mult
    
