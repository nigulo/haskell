
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
import Ephem.Types

import Utils.Misc

import Data.IORef
import Data.Maybe
import Data.String
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import Data.List
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Applicative

import System.Random

data ModifyOp = Add 
        | Subtract 
        | Mean 
        | Multiply 
        | Invert -- only 2d data
        | Union
        | Intersection
        | Complement
        | Join
        | Split
        | SegmentByMarkers
        | SegmentByCount
        | SegmentByGaps
        | Unsegment
        | SetWeights
        | MovingAverage
        | JDToYear
        | YearToJD
        | Round
        | RemoveNeighbour
        | FindNeighbour
        | Scale
        | AddNoise
        | Interpolate
        | Convert 
        deriving (Eq, Ord, Show, Read)

getOpText :: ModifyOp -> String
getOpText SegmentByMarkers = "Segment by markers" 
getOpText SegmentByCount = "Segment by count" 
getOpText SegmentByGaps = "Segment by gaps" 
getOpText SetWeights = "Set weights" 
getOpText MovingAverage = "Moving average" 
getOpText JDToYear = "JD to year" 
getOpText YearToJD = "Year to JD" 
getOpText RemoveNeighbour = "Remove neighbour" 
getOpText FindNeighbour = "Find neighbour" 
getOpText AddNoise = "Add noise" 
getOpText op = show op 

modifyOps = [Add, 
        Subtract, 
        Mean,
        Multiply, 
        Invert, -- only 2d data
        Union,
        Intersection,
        Complement,
        Join,
        Split,
        SegmentByMarkers,
        SegmentByCount,
        SegmentByGaps,
        Unsegment,
        SetWeights,
        MovingAverage,
        JDToYear,
        YearToJD,
        Round,
        RemoveNeighbour,
        FindNeighbour,
        Scale,
        AddNoise,
        Interpolate,
        Convert]

getModifyOp :: Int -> ModifyOp
getModifyOp i = modifyOps !! i

data ModifyOpType = 
    Outside
    | Inside
    | ToSpectrum
    | ToData
    | Y
    | X 
    deriving (Eq, Show, Read)

getTypeText :: ModifyOpType -> String
getTypeText ToSpectrum = "To spectrum"
getTypeText ToData = "To data"
getTypeText Y = "y"
getTypeText X = "x"
getTypeText opType = show opType

typeMappings = M.fromList [
        (RemoveNeighbour, [Outside, Inside]),
        (Convert, [ToSpectrum, ToData])
    ]

getOpTypes :: ModifyOp -> [ModifyOpType]
getOpTypes op = case M.lookup op typeMappings of 
    Just types -> types
    Nothing -> [Y, X]

getOpType :: ModifyOp -> Int -> ModifyOpType
getOpType op i = (getOpTypes op)  !! i

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

    opCombo <- createComboBox (map (getOpText) modifyOps)
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

    let
        updateWidgets =
            do
                listStore <- comboBoxGetModelText typeCombo
                numRows <- listStoreGetSize listStore
                mapM_ (\_ -> comboBoxRemoveText typeCombo 0) [1 .. numRows]
                opNo <- comboBoxGetActive opCombo
                mapM_ (\t -> comboBoxAppendText typeCombo (fromString t)) (map (getTypeText) (getOpTypes (getModifyOp opNo)))

    after dialog realize updateWidgets
    on opCombo changed updateWidgets



    widgetShowAll dialog
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                name <- entryGetString nameEntry
                
                selectedData1 <- getSelectedData dataSetCombo1
                selectedData2 <- getSelectedData dataSetCombo2
                opNo <- comboBoxGetActive opCombo
                typeNo <- comboBoxGetActive typeCombo
                constant <- spinButtonGetValue constantSpin
                constant2 <- spinButtonGetValue constant2Spin
                widgetDestroy dialog
                tEnv <- taskEnv stateRef

                let
                    op = getModifyOp opNo
                    opType = getOpType op typeNo
                    graphTabParms = (graphTabs state) !! currentGraphTab
                    selectedGraph = graphTabSelection graphTabParms
                    segments = graphSegments ((graphTabGraphs graphTabParms) !! selectedGraph)
                    modify (name, selectedData1) = 
                        case selectedData2 of
                            Just sd2 ->
                                if (op == Union) || (op == Intersection) || op == Complement
                                    then
                                        if isDiscrete selectedData1 && isDiscrete sd2 then do
                                                let
                                                    func i _ (Left ds1) _ = do
                                                        let
                                                            Left ds2 = getSubDataAt sd2 i
                                                            func = case op of
                                                                Union -> union
                                                                Intersection -> intersect
                                                                Complement -> (\\)
                                                            vals = V.fromList $ func (V.toList (D.values1 ds1)) (V.toList (D.values1 ds2)) 
                                                        return $ Left (D.data1 vals)
                                                                                
                                                result <- applyToData1 func selectedData1 name tEnv
                                                modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                            else return ()
                                else if (op == Join)
                                    then
                                        modifyState stateRef $ addDataParams (merge name selectedData1 sd2) (Just (currentGraphTab, selectedGraph))
                                else if (take 7 (getOpText op) == "Segment") 
                                    then do
                                            let
                                                d = subData $ head $ dataSet selectedData1
                                                minMax sdp = case subData sdp of
                                                    Left d -> (D.xMins d, D.xMaxs d)
                                                    Right (Left s) -> (AD.xMins s, AD.xMaxs s)  
                                                    Right (Right f) -> (AD.xMins f, AD.xMaxs f)
                                                mapOp (minX, maxX) = 
                                                    createSubDataParams 
                                                        (minX, maxX)
                                                        (case d of
                                                            Left d -> Left (D.subSet1 (head minX, head maxX) d)
                                                            Right (Left (AnalyticData [(_, _, s)])) -> Right $ Left (AnalyticData [(minX, maxX, s)])
                                                            Right (Right (AnalyticData [(_, _, f)])) -> Right $ Right (AnalyticData [(minX, maxX, f)])) 
                                                        []
                                                minsMaxs = map minMax (dataSet sd2)
                                                gaps = zipWith (\(_, minX) (maxX, _) -> (minX, maxX)) (init minsMaxs) (tail minsMaxs)
                                                result = if op == SegmentByGaps then map mapOp gaps else map mapOp minsMaxs 
                                            modifyState stateRef $ addDataParams (createDataParams_ name result) (Just (currentGraphTab, selectedGraph))
                                else if op == FindNeighbour then
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
                                        mapOp sdp1 sdp2 = createSubDataParams
                                                (subDataRange sdp1)
                                                (U.binaryOp (getModifyFunc op) (subData sdp1) (subData sdp2) (opType == Y) g) 
                                                []
                                        result = zipWith mapOp (dataSet selectedData1) (dataSet sd2) 
                                    modifyState stateRef $ addDataParams (createDataParams_ name result) (Just (currentGraphTab, selectedGraph))
                            Nothing -> 
                                case op of
                                    Invert ->
                                        if isDiscrete selectedData1 then do
                                                let
                                                    func i j (Left ds1) _ =
                                                        do
                                                        let
                                                            (xs, ys, ws) = V.unzip3 $ D.values1 ds1
                                                        return $ Left (D.data1 (V.zip3 ys xs ws))
                                                                                
                                                result <- applyToData1 func selectedData1 name tEnv
                                                modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                            else return ()
                                    Split ->
                                        mapM_ (\(i, sdp) -> 
                                            modifyState stateRef $ addDataParams (createDataParams_ (name ++ show i) [sdp]) (Just (currentGraphTab, selectedGraph))
                                            ) (zip [1, 2 ..] (dataSet selectedData1))
                                    SegmentByMarkers ->
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
                                                                    createSubDataParams 
                                                                            ([xLeft], [xRight])
                                                                            (Left seg) 
                                                                            []
                                                            ) (init sortedSegments) (tail sortedSegments)
                                                    in
                                                        ds
                                            modifyState stateRef $ addDataParams (createDataParams_ name (concat (map mapOp (dataSet selectedData1)))) (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    SegmentByCount ->
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
                                                                    if dataLength seg > 0 then Just (createSubDataParams
                                                                            ([xLeft], [xLeft + subSetLength])
                                                                            (Left seg)
                                                                            []
                                                                        ) else Nothing
                                                            ) [xMin, xMin + subSetLength .. xMax - subSetLength]
                                                    in
                                                        catMaybes ds
                                            modifyState stateRef $ addDataParams (createDataParams_ name (concat (map mapOp (dataSet selectedData1)))) (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    SegmentByGaps ->
                                        if isDiscrete selectedData1 then do
                                            let
                                                splitByGaps (Left d) = splitByGaps' (D.xMin1 d) (V.toList (D.values1 d)) [] where
                                                    splitByGaps' _ [] segment = 
                                                        let
                                                            d = data1 (V.fromList segment)
                                                        in
                                                            [createSubDataParams__ (Left d)]
                                                    splitByGaps' lastX ((x, y, w):vals) segment =
                                                        if (x - lastX > constant) 
                                                            then 
                                                                let
                                                                    d = data1 (V.fromList segment)
                                                                in
                                                                    createSubDataParams__ (Left d):splitByGaps' x vals [(x, y, w)]
                                                            else splitByGaps' x vals (segment ++ [(x, y, w)])
                                            modifyState stateRef $ addDataParams (createDataParams_ name (splitByGaps (subData (head (dataSet selectedData1))))) (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    Unsegment ->
                                        if isDiscrete selectedData1 then do
                                            let
                                                unsegment (SubDataParams _ (Left d) _:[]) = D.values1 d
                                                unsegment (SubDataParams _ (Left d) _:sdps) = D.values1 d V.++ unsegment sdps
                                            modifyState stateRef $ addDataParams (createDataParams_ name 
                                                ( 
                                                    let 
                                                        d = D.data1 (sortVectorBy (\(x1, _, _) (x2, _, _) -> compare x1 x2) (unsegment (dataSet selectedData1)))
                                                    in
                                                        [createSubDataParams__ (Left d)]
                                                )) (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    SetWeights ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> return (Left (D.setW (V.replicate (D.dataLength dat) constant) dat))) selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    --MovingAverage ->
                                    JDToYear ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> return (Left (D.data1 (V.map (\(x, y, w) -> let TropicalYears year = toTropicalYears (JD x) in (year, y, w) ) (D.values1 dat))))) selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    YearToJD ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> return (Left (D.data1 (V.map (\(x, y, w) -> let JD jd = toJD (TropicalYears x) in (jd, y, w) ) (D.values1 dat))))) selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    Round ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> 
                                                return (Left (D.data1 (V.map (\(x, y, w) -> 
                                                    let 
                                                        e = 10 ^ (round constant) 
                                                    in 
                                                 if (opType == Y) then (x, (fromIntegral (round (y * e))) / e, w) 
                                                        else ((fromIntegral (round (x * e))) / e, y, w)) (D.values1 dat))))) selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    RemoveNeighbour ->
                                        if isDiscrete selectedData1 then do
                                            dataWithWeights <- applyToData1 (\_ _ (Left dat) _ -> do
                                                let 
                                                        filterFunc [] res (Just lastVal) = res ++ [lastVal]
                                                        filterFunc (val:vals) [] Nothing = filterFunc vals [] (Just val)
                                                        filterFunc (val@(x, y, _):vals) res (Just lastVal@(lastX, lastY, _)) = 
                                                            if (opType == Outside) && x > lastX + constant || (opType == Inside) && x < lastX + constant 
                                                                then filterFunc vals (res ++ [lastVal]) (Just val) 
                                                                else 
                                                                    if y > lastY 
                                                                        then filterFunc vals res (Just val)
                                                                        else filterFunc vals res (Just lastVal)
                                                                          
                                                return (Left (D.data1 (V.fromList (filterFunc (V.toList (D.values1 dat)) [] Nothing))))
                                                        ) selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                        else return ()
                                    Scale ->
                                        if isDiscrete selectedData1 && constant /= 0 
                                        then do
                                            let 
                                                func i j d _ = 
                                                    do
                                                        let
                                                            Left dat = d 
                                                            (maxVal, minVal) = if (opType == Y) then (D.yMax dat, D.yMin dat) else (D.xMax1 dat, D.xMin1 dat)
                                                            f = F.function ("(x-" ++ show minVal ++")*" ++ show constant ++ "/(" ++ show (maxVal - minVal) ++ ")")
                                                        return $ constantOp f d 0 (opType == Y)
                                            result <- applyToData1 func selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                        else
                                            return ()
                                    AddNoise ->
                                        if isDiscrete selectedData1 && constant /= 0 
                                        then do
                                            let 
                                                func i j d _ = 
                                                    do
                                                        let
                                                            Left dat = d 
                                                            newVals = zipWith (\(x, y, w) r -> if (opType == Y) then (x, y + r, w) else (x + r, y, w)) (V.toList (D.values1 dat)) (randomRs (-constant, constant) g)
                                                        return $ Left (D.data1 (V.fromList newVals))
                                            result <- applyToData1 func selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                        else
                                            return ()
                                    Interpolate ->
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
                                            result <- applyToData1 func selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                        else
                                            return ()
                                    Convert ->
                                        if isDiscrete selectedData1 && TSA.Data.is2d selectedData1 
                                        then do
                                            let 
                                                func i j d _ = 
                                                    do
                                                        let
                                                            Left dat = d
                                                            dataCreateFunc = case typeNo of
                                                                0 -> D.spectrum1
                                                                1 -> D.data1
                                                            newDat = dataCreateFunc (D.values1 dat)
                                                        return $ Left newDat
                                            result <- applyToData1 func selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                        else
                                            return ()
                                    otherwise ->
                                        if constant /= 0 
                                        then do
                                            let func i j d _ = return $ constantOp (getModifyFunc op) d constant (opType == Y)
                                            result <- applyToData1 func selectedData1 name tEnv
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

getModifyFunc :: ModifyOp -> F.Function Double
getModifyFunc Add = F.add
getModifyFunc Subtract = F.subtr
getModifyFunc Mean = (F.function "(x+y)/2")
getModifyFunc Multiply = F.mult
    
