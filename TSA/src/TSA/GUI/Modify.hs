{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module TSA.GUI.Modify (modifyDialog) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as T
import qualified Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.AnalyticData as AD
import Regression.AnalyticDataWrapper as ADW
import Regression.Data as D
import Regression.Utils as U
import qualified Math.Function as F
import qualified Math.Expression as E

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Data
import TSA.Data
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget hiding (entryGetString)
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
        | Invert
        | Swap -- only 2d data (swith x and y)
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
        | Smooth
        | JDToYear
        | YearToJD
        | Round
        | RemoveNeighbour
        | FindNeighbour
        | Scale
        | Function
        | AddNoise
        | Interpolate
        | FillGaps
        | Convert
        | Histogram
        deriving (Eq, Ord, Show, Read)

getOpText :: ModifyOp -> String
getOpText SegmentByMarkers = "Segment by markers"
getOpText SegmentByCount = "Segment by count"
getOpText SegmentByGaps = "Segment by gaps"
getOpText SetWeights = "Set weights"
getOpText JDToYear = "JD to year"
getOpText YearToJD = "Year to JD"
getOpText RemoveNeighbour = "Remove neighbour"
getOpText FindNeighbour = "Find neighbour"
getOpText AddNoise = "Add noise"
getOpText FillGaps = "Fill gaps"
getOpText op = show op

modifyOps = [Add,
        Subtract,
        Mean,
        Multiply,
        Invert,
        Swap,
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
        Smooth,
        JDToYear,
        YearToJD,
        Round,
        RemoveNeighbour,
        FindNeighbour,
        Scale,
        Function,
        AddNoise,
        Interpolate,
        FillGaps,
        Convert,
        Histogram]

getModifyOp :: Int -> ModifyOp
getModifyOp i = modifyOps !! i

data ModifyOpType =
    Outside
    | Inside
    | ToSpectrum
    | ToData
    | GaussianKernel
    | MovingAverage
    | Y
    | X
    deriving (Eq, Show, Read)

getTypeText :: ModifyOpType -> String
getTypeText ToSpectrum = "To spectrum"
getTypeText ToData = "To data"
getTypeText GaussianKernel = "Gaussian kernel"
getTypeText MovingAverage = "Moving average"
getTypeText Y = "y"
getTypeText X = "x"
getTypeText opType = show opType

--------------------------------------------------
data ExtSettings = OneConstant | TwoConstants | Formula deriving (Eq, Show, Read)
data DataSetSettings = OneDataSet | TwoDataSets deriving (Eq, Show, Read)
type SubSettings = (DataSetSettings, (Maybe ExtSettings))
type ModifySettings = ([ModifyOpType], [SubSettings])

getSettings :: ModifyOp -> ModifySettings

getSettings FindNeighbour = ([], [(TwoDataSets, Just TwoConstants)])
getSettings Union = ([], [(TwoDataSets, Nothing)])
getSettings Intersection = ([], [(TwoDataSets, Nothing)])
getSettings Complement = ([], [(TwoDataSets, Nothing)])
getSettings Join = ([], [(TwoDataSets, Nothing)])

getSettings Add = ([Y, X], [(OneDataSet, Just OneConstant), (TwoDataSets, Nothing)])
getSettings Subtract = ([Y, X], [(OneDataSet, Just OneConstant), (TwoDataSets, Nothing)])
getSettings Mean = ([Y, X], [(OneDataSet, Just OneConstant), (TwoDataSets, Nothing)])
getSettings Multiply = ([Y, X], [(OneDataSet, Just OneConstant), (TwoDataSets, Nothing)])
getSettings Invert = ([Y, X], [(OneDataSet, Just OneConstant), (TwoDataSets, Nothing)])
getSettings SegmentByCount = ([], [(OneDataSet,Just OneConstant), (TwoDataSets, Nothing)])
getSettings SegmentByGaps = ([], [(OneDataSet, Just OneConstant), (TwoDataSets, Nothing)])

getSettings Swap = ([], [(OneDataSet, Nothing)])
getSettings Split = ([], [(OneDataSet, Nothing)])
getSettings JDToYear = ([], [(OneDataSet, Nothing)])
getSettings YearToJD = ([], [(OneDataSet, Nothing)])
getSettings Unsegment = ([], [(OneDataSet, Nothing)])
getSettings Convert = ([ToSpectrum, ToData], [(OneDataSet, Nothing)])
getSettings SegmentByMarkers = ([], [(OneDataSet, Nothing)])

getSettings SetWeights = ([], [(OneDataSet, Just OneConstant)])
getSettings Smooth = ([GaussianKernel, MovingAverage], [(OneDataSet, Just OneConstant)])
getSettings Round = ([Y, X], [(OneDataSet, Just OneConstant)])
getSettings RemoveNeighbour = ([Outside, Inside], [(OneDataSet, Just OneConstant)])
getSettings Scale = ([Y, X], [(OneDataSet, Just OneConstant)])
getSettings AddNoise = ([Y, X], [(OneDataSet, Just OneConstant)])
getSettings Interpolate = ([], [(OneDataSet, Just OneConstant)])
getSettings FillGaps = ([], [(OneDataSet, Just OneConstant)])
getSettings Histogram = ([Y, X], [(OneDataSet, Just OneConstant)])

getSettings Function = ([Y, X], [(OneDataSet, Just Formula), (TwoDataSets, Just Formula)])

getActiveSubSetting :: [SubSettings] -> Bool -> SubSettings
getActiveSubSetting subSettings data2Selected =
    let
        filterFn (OneDataSet, _) = not data2Selected
        filterFn (TwoDataSets, _) = data2Selected
        filteredSubSettings = filter (filterFn) subSettings
    in
        if null filteredSubSettings
            then
                head subSettings
            else
                head filteredSubSettings
--------------------------------------------------


getOpType :: ModifyOp -> Int -> ModifyOpType
getOpType modifyOp i = (fst (getSettings modifyOp))  !! i

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

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry (getNameWithNo commonParams)
    addWidgetToBox (Just "Name: ") nameEntry contentBox

    dataSetCombo1 <- dataSetComboNew2 (\_ -> True) state True
    addWidgetToBox (Just "Data set 1: ") (getComboBox dataSetCombo1) contentBox

    opCombo <- createComboBox (map (getOpText) modifyOps)
    comboBoxSetActive opCombo (modifyOp parms)
    addWidgetToBox (Just "Operation: ") opCombo contentBox

    dataSetCombo2 <- dataSetComboNew2 (\_ -> True) state False
    hBox2 <- Gtk.boxNew Gtk.OrientationHorizontal 4
    dataSetLabel2 <- Gtk.labelNew (Just "Data set 2: ")
    Gtk.widgetSetHalign dataSetLabel2 Gtk.AlignStart
    Gtk.boxAppend hBox2 (getComboBox dataSetCombo2)
    Gtk.widgetSetHexpand (getComboBox dataSetCombo2) True
    Gtk.boxAppend contentBox hBox2

    typeCombo <- createComboBox ["y", "x"]
    comboBoxSetActive typeCombo (modifyType parms)
    hBox3 <- Gtk.boxNew Gtk.OrientationHorizontal 4
    typeLabel <- Gtk.labelNew (Just "Type: ")
    Gtk.widgetSetHalign typeLabel Gtk.AlignStart
    Gtk.boxAppend hBox3 typeCombo
    Gtk.widgetSetHexpand typeCombo True
    Gtk.boxAppend contentBox hBox3

    constantAdjustment <- Gtk.adjustmentNew (modifyConstant parms) (-2**52) (2**52) 1 1 1
    constantSpin <- Gtk.spinButtonNew (Just constantAdjustment) 1 10
    hBox4 <- Gtk.boxNew Gtk.OrientationHorizontal 4
    constantLabel <- Gtk.labelNew (Just "Constant: ")
    Gtk.widgetSetHalign constantLabel Gtk.AlignStart
    Gtk.boxAppend hBox4 constantSpin
    Gtk.widgetSetHexpand constantSpin True
    Gtk.boxAppend contentBox hBox4

    constant2Adjustment <- Gtk.adjustmentNew (modifyConstant parms) (-2**52) (2**52) 1 1 1
    constant2Spin <- Gtk.spinButtonNew (Just constant2Adjustment) 1 10
    hBox5 <- Gtk.boxNew Gtk.OrientationHorizontal 4
    constant2Label <- Gtk.labelNew (Just "Constant 2: ")
    Gtk.widgetSetHalign constant2Label Gtk.AlignStart
    Gtk.boxAppend hBox5 constant2Spin
    Gtk.widgetSetHexpand constant2Spin True
    Gtk.boxAppend contentBox hBox5

    ----
    functionTextBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
    Gtk.textBufferSetText functionTextBuffer "x^2" (-1)
    functionTextView <- Gtk.textViewNewWithBuffer functionTextBuffer
    Gtk.textViewSetMonospace functionTextView True
    Gtk.textViewSetAcceptsTab functionTextView False

    scrolledWindow <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild scrolledWindow (Just functionTextView)
    Gtk.widgetSetVexpand scrolledWindow True

    functionFrame <- Gtk.frameNew (Just "Function")
    Gtk.frameSetChild functionFrame (Just scrolledWindow)
    addWidgetToBox Nothing functionFrame contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    let
        updateWidgets =
            do
                opNo <- comboBoxGetActive opCombo
                let
                    modifyOp = getModifyOp opNo
                    (opTypes, subSettings) = getSettings modifyOp
                    onlyOneDataSet = null $ filter (\(dataSettings, _) -> dataSettings == TwoDataSets) subSettings
                    onlyTwoDataSets = null $ filter (\(dataSettings, _) -> dataSettings == OneDataSet) subSettings
                if onlyTwoDataSets
                    then
                        dataComboBoxSetMandatory dataSetCombo2 True
                    else
                        dataComboBoxSetMandatory dataSetCombo2 False
                if onlyOneDataSet
                    then do
                        Gtk.widgetSetVisible (getComboBox dataSetCombo2) False
                        Gtk.widgetSetVisible dataSetLabel2 False
                    else do
                        Gtk.widgetSetVisible (getComboBox dataSetCombo2) True
                        Gtk.widgetSetVisible dataSetLabel2 True
                selectedData2 <- getSelectedData dataSetCombo2
                let
                    (_, extSettings) =
                            case selectedData2 of
                                Just _ -> getActiveSubSetting subSettings True
                                _ -> getActiveSubSetting subSettings False
                if null opTypes
                    then do
                        Gtk.widgetSetVisible typeCombo False
                        Gtk.widgetSetVisible typeLabel False
                    else do
                        Gtk.widgetSetVisible typeCombo True
                        Gtk.widgetSetVisible typeLabel True
                        stringList <- Gtk.stringListNew (Just (map (T.pack . getTypeText) opTypes))
                        Gtk.dropDownSetModel typeCombo (Just stringList)
                        comboBoxSetActive typeCombo 0

                case extSettings of
                    Nothing -> do
                        Gtk.widgetSetVisible constantSpin False
                        Gtk.widgetSetVisible constantLabel False
                        Gtk.widgetSetVisible constant2Spin False
                        Gtk.widgetSetVisible constant2Label False
                        Gtk.widgetSetVisible functionFrame False
                    Just OneConstant -> do
                        Gtk.widgetSetVisible constantSpin True
                        Gtk.widgetSetVisible constantLabel True
                        Gtk.widgetSetVisible constant2Spin False
                        Gtk.widgetSetVisible constant2Label False
                        Gtk.widgetSetVisible functionFrame False
                    Just TwoConstants -> do
                        Gtk.widgetSetVisible constantSpin True
                        Gtk.widgetSetVisible constantLabel True
                        Gtk.widgetSetVisible constant2Spin True
                        Gtk.widgetSetVisible constant2Label True
                        Gtk.widgetSetVisible functionFrame False
                    Just Formula -> do
                        Gtk.widgetSetVisible constantSpin False
                        Gtk.widgetSetVisible constantLabel False
                        Gtk.widgetSetVisible constant2Spin False
                        Gtk.widgetSetVisible constant2Label True
                        Gtk.widgetSetVisible functionFrame True
    _ <- after dialog #realize updateWidgets
    _ <- on opCombo #notify $ \_ -> updateWidgets
    _ <- on (getComboBox dataSetCombo2) #notify $ \_ -> updateWidgets

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy dialog

    _ <- Gtk.onButtonClicked okButton $ do
        name <- entryGetString nameEntry

        selectedData1 <- getSelectedData dataSetCombo1
        selectedData2 <- getSelectedData dataSetCombo2
        opNo <- comboBoxGetActive opCombo
        typeNo <- comboBoxGetActive typeCombo
        constant <- spinButtonGetValue constantSpin
        constant2 <- spinButtonGetValue constant2Spin

        startIter <- Gtk.textBufferGetStartIter functionTextBuffer
        endIter <- Gtk.textBufferGetEndIter functionTextBuffer
        f <- Gtk.textBufferGetText functionTextBuffer startIter endIter False

        Gtk.windowDestroy dialog
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
                                            func i (SD1 ds1) _ = do
                                                let
                                                    SD1 ds2 = getSubDataAt sd2 i
                                                    listFunc = case op of
                                                        Union -> union
                                                        Intersection -> intersect
                                                        Complement -> (\\)
                                                    vals = V.fromList $ listFunc (V.toList (D.values1 ds1)) (V.toList (D.values1 ds2))
                                                return $ SD1 (D.data1 (sortVectorBy (\(x1, _, _) (x2, _, _) -> compare x1 x2) vals))

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
                                            minMax sdp = case unboxSubData $ subData sdp of
                                                Left d -> (D.xMins d, D.xMaxs d)
                                                Right ad -> (ADW.xMins ad, ADW.xMaxs ad)
                                            mapOp (minX, maxX) =
                                                createSubDataParams
                                                    (minX, maxX)
                                                    (case d of
                                                        SD1 d -> SD1 (D.subSet1 (head minX, head maxX) d)
                                                        SD2 (AnalyticData [(_, _, ad)]) -> SD2 (AnalyticData [(minX, maxX, ad)])
                                                        SD3 (AnalyticData [(_, _, ad)]) -> SD3 (AnalyticData [(minX, maxX, ad)])
                                                        SD4 (AnalyticData [(_, _, ad)]) -> SD4 (AnalyticData [(minX, maxX, ad)]))
                                            minsMaxs = map minMax (dataSet sd2)
                                            gaps = zipWith (\(_, minX) (maxX, _) -> (minX, maxX)) (init minsMaxs) (tail minsMaxs)
                                            result = if op == SegmentByGaps then map mapOp gaps else map mapOp minsMaxs
                                        modifyState stateRef $ addDataParams (createDataParams_ name result) (Just (currentGraphTab, selectedGraph))
                            else if op == FindNeighbour then
                                    if isDiscrete selectedData1 && isDiscrete sd2 then do
                                        let
                                            SD1 d1 = subData $ head $ dataSet selectedData1
                                            SD1 d2 = subData $ head $ dataSet sd2
                                            mapFunc val@(x1, y1, _) =
                                                let
                                                    vals2 = V.toList $ D.values1 d2
                                                    nearestVals = filter (\(x2, y2, _) -> abs (x1 - x2) <= constant && abs (y1 - y2) <= constant2) vals2
                                                in
                                                    if null nearestVals
                                                    then Nothing
                                                    else Just val
                                            nearest = SD1 (D.data1 (V.fromList (catMaybes (map mapFunc (V.toList (D.values1 d1))))))
                                        modifyState stateRef $ addData nearest name (Just (currentGraphTab, selectedGraph))
                                    else return ()
                            else if op == Function then
                                if isDiscrete selectedData1
                                then do
                                    maybeFunc <- getFunction dialog (T.unpack f) 2
                                    case maybeFunc of
                                        Just func -> do
                                            let
                                                mapOp sdp1 sdp2 = createSubDataParams
                                                        (subDataRange sdp1)
                                                        (subDataBinaryOp func (subData sdp1) (subData sdp2) (opType == Y) g)
                                                result = zipWith mapOp (dataSet selectedData1) (dataSet sd2)
                                            modifyState stateRef $ addDataParams (createDataParams_ name result) (Just (currentGraphTab, selectedGraph))
                                        Nothing -> return ()
                                else
                                    return ()
                            else do
                                let
                                    mapOp sdp1 sdp2 = createSubDataParams
                                            (subDataRange sdp1)
                                            (subDataBinaryOp (getModifyFunc op) (subData sdp1) (subData sdp2) (opType == Y) g)
                                    result = zipWith mapOp (dataSet selectedData1) (dataSet sd2)
                                modifyState stateRef $ addDataParams (createDataParams_ name result) (Just (currentGraphTab, selectedGraph))
                    Nothing ->
                        case op of
                            Swap ->
                                if isDiscrete selectedData1 then do
                                        let
                                            func i (SD1 ds1) _ =
                                                do
                                                let
                                                    (xs, ys, ws) = V.unzip3 $ D.values1 ds1
                                                return $ SD1 (D.data1 (V.zip3 ys xs ws))

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
                                                SD1 d = subData sdp
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
                                                                    (SD1 seg)
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
                                                SD1 d = subData sdp
                                                xMin = D.xMin1 d
                                                xMax = D.xMax1 d
                                                subSetLength = (xMax - xMin) / constant
                                                ds = map (\xLeft ->
                                                        let
                                                            seg = D.subSet1 (xLeft, xLeft + subSetLength) d
                                                        in
                                                            if dataLength seg > 0 then Just (createSubDataParams
                                                                    ([xLeft], [xLeft + subSetLength])
                                                                    (SD1 seg)
                                                                ) else Nothing
                                                    ) [xMin, xMin + subSetLength .. xMax - subSetLength]
                                            in
                                                catMaybes ds
                                    modifyState stateRef $ addDataParams (createDataParams_ name (concat (map mapOp (dataSet selectedData1)))) (Just (currentGraphTab, selectedGraph))
                                else return ()
                            SegmentByGaps ->
                                if isDiscrete selectedData1 then do
                                    let
                                        splitByGaps (SD1 d) = splitByGaps' (D.xMin1 d) (V.toList (D.values1 d)) [] where
                                            splitByGaps' _ [] segment =
                                                let
                                                    d = data1 (V.fromList segment)
                                                in
                                                    [createSubDataParams_ (SD1 d)]
                                            splitByGaps' lastX ((x, y, w):vals) segment =
                                                if (x - lastX > constant)
                                                    then
                                                        let
                                                            d = data1 (V.fromList segment)
                                                        in
                                                            createSubDataParams_ (SD1 d):splitByGaps' x vals [(x, y, w)]
                                                    else splitByGaps' x vals (segment ++ [(x, y, w)])
                                    modifyState stateRef $ addDataParams (createDataParams_ name (splitByGaps (subData (head (dataSet selectedData1))))) (Just (currentGraphTab, selectedGraph))
                                else return ()
                            Unsegment ->
                                if isDiscrete selectedData1 then do
                                    let
                                        unsegment (SubDataParams _ (SD1 d) :[]) = D.values1 d
                                        unsegment (SubDataParams _ (SD1 d) :sdps) = D.values1 d V.++ unsegment sdps
                                    modifyState stateRef $ addDataParams (createDataParams_ name
                                        (
                                            let
                                                d = D.data1 (sortVectorBy (\(x1, _, _) (x2, _, _) -> compare x1 x2) (unsegment (dataSet selectedData1)))
                                            in
                                                [createSubDataParams_ (SD1 d)]
                                        )) (Just (currentGraphTab, selectedGraph))
                                else return ()
                            SetWeights ->
                                if isDiscrete selectedData1 then do
                                    dataWithWeights <- applyToData1 (\_ (SD1 dat) _ -> return (SD1 (D.setW (V.replicate (D.dataLength dat) constant) dat))) selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                else return ()
                            Smooth ->
                                if isDiscrete selectedData1 && constant > 0 then do
                                    smoothedData <- applyToData1 (\_ (SD1 dat) _ ->
                                        case opType of
                                            GaussianKernel -> do -- constant refers to stdev of gaussian kernel
                                                let
                                                    xys = D.xys1 dat
                                                    xMin = D.xMin1 dat
                                                    xMax = D.xMax1 dat
                                                    xStep = (xMax - xMin) / (fromIntegral (V.length xys))
                                                    sigma = constant
                                                    sigma2 = sigma * sigma
                                                    threeSigma = 3 * sigma
                                                    newXYs =
                                                        map (\x ->
                                                                let
                                                                    xysKernel = V.filter (\(xDat, yDat) -> xDat >= x - threeSigma && xDat <= x + threeSigma) xys
                                                                    (y, w) = V.foldl' (\(ySum, wSum) (xDat, yDat) ->
                                                                            let
                                                                                weight = exp (-(xDat - x) ^ 2 / 2 / sigma2)
                                                                            in
                                                                                (ySum + yDat * weight, wSum + weight)
                                                                            ) (0, 0) xysKernel
                                                                in
                                                                    (x, y / w)
                                                            ) [xMin, xMin + xStep .. xMax]
                                                return $ SD1 $ D.data1' $ V.fromList newXYs
                                            MovingAverage -> do -- for evenly sampled data only (constant refers to number of neighbouring points)
                                                let
                                                    numNeighbours = round constant
                                                    xys = D.xys1 dat
                                                    newXYs =
                                                        map (\i ->
                                                                let
                                                                    mean = (foldl' (\sum j -> sum + snd (xys V.! j)) 0 [i - numNeighbours .. i + numNeighbours]) / (2 * constant + 1)
                                                                in
                                                                    (fst (xys V.! i), mean)
                                                            ) [numNeighbours .. D.dataLength dat - numNeighbours - 1]
                                                return $ SD1 $ D.data1' $ V.fromList newXYs
                                        ) selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams smoothedData (Just (currentGraphTab, selectedGraph))
                                else return ()
                            JDToYear ->
                                if isDiscrete selectedData1 then do
                                    dataWithWeights <- applyToData1 (\_ (SD1 dat) _ -> return (SD1 (D.data1 (V.map (\(x, y, w) -> let TropicalYears year = toTropicalYears (JD x) in (year, y, w) ) (D.values1 dat))))) selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                else return ()
                            YearToJD ->
                                if isDiscrete selectedData1 then do
                                    dataWithWeights <- applyToData1 (\_ (SD1 dat) _ -> return (SD1 (D.data1 (V.map (\(x, y, w) -> let JD jd = toJD (TropicalYears x) in (jd, y, w) ) (D.values1 dat))))) selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                else return ()
                            Round ->
                                if isDiscrete selectedData1 then do
                                    dataWithWeights <- applyToData1 (\_ (SD1 dat) _ ->
                                        return (SD1 (D.data1 (V.map (\(x, y, w) ->
                                            let
                                                e = 10 ^ (round constant)
                                            in
                                         if (opType == Y) then (x, (fromIntegral (round (y * e))) / e, w)
                                                else ((fromIntegral (round (x * e))) / e, y, w)) (D.values1 dat))))) selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                else return ()
                            RemoveNeighbour ->
                                if isDiscrete selectedData1 then do
                                    dataWithWeights <- applyToData1 (\_ (SD1 dat) _ -> do
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

                                        return (SD1 (D.data1 (V.fromList (filterFunc (V.toList (D.values1 dat)) [] Nothing))))
                                                ) selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams dataWithWeights (Just (currentGraphTab, selectedGraph))
                                else return ()
                            Scale ->
                                if isDiscrete selectedData1 && constant /= 0
                                then do
                                    let
                                        func i d _ =
                                            do
                                                let
                                                    SD1 dat = d
                                                    (maxVal, minVal) = if (opType == Y) then (D.yMax dat, D.yMin dat) else (D.xMax1 dat, D.xMin1 dat)
                                                    f = F.function ("(x-" ++ show minVal ++")*" ++ show constant ++ "/(" ++ show (maxVal - minVal) ++ ")")
                                                return $ subDataConstantOp f d 0 (opType == Y) g
                                    result <- applyToData1 func selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                else
                                    return ()
                            Function -> -- Applicable only to y-values
                                if isDiscrete selectedData1
                                then do
                                    maybeFunc <- getFunction dialog (T.unpack f) 1
                                    case maybeFunc of
                                        Just func -> do
                                            result <- applyToData1 (\i d _ -> return (subDataConstantOp func d 0 True g)) selectedData1 name tEnv
                                            modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                        Nothing -> return ()
                                else
                                    return ()
                            AddNoise ->
                                if isDiscrete selectedData1 && constant /= 0
                                then do
                                    let
                                        func i d _ =
                                            do
                                                let
                                                    SD1 dat = d
                                                    newVals = zipWith (\(x, y, w) r -> if (opType == Y) then (x, y + r, w) else (x + r, y, w)) (V.toList (D.values1 dat)) (randomRs (-constant, constant) g)
                                                return $ SD1 (D.data1 (V.fromList newVals))
                                    result <- applyToData1 func selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                else
                                    return ()
                            Interpolate ->
                                if isDiscrete selectedData1 && constant > 0
                                then do
                                    let
                                        func i d _ =
                                            do
                                                let
                                                    SD1 dat = d
                                                    xVals = D.xs1 dat
                                                    xMin = D.xMin1 dat
                                                    xMax = D.xMax1 dat
                                                    step = (xMax - xMin) / constant
                                                    xVals1 = V.generate (round constant) (\i -> xMin + fromIntegral i * step) -- V.fromList [xMin, xMin + step .. xMax]
                                                    --xVals1 = (V.concatMap (\(x1, x2) -> V.init (V.fromList [x1, x1 + (x2 - x1) / constant .. x2])) (V.zip (V.init xVals) (V.tail xVals))) `V.snoc` (V.last xVals)
                                                    newDat = D.interpolatedData1 xVals1 dat
                                                return $ SD1 newDat
                                    result <- applyToData1 func selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                else
                                    return ()
                            FillGaps ->
                                if isDiscrete selectedData1
                                then do
                                    let
                                        func i d _ =
                                            do
                                                let
                                                    SD1 dat = d
                                                    vals = D.values1 dat
                                                    filteredVals = V.filter (\(_, y, _) -> y /= constant) vals
                                                    (xMin, _, _) = V.head filteredVals
                                                    (xMax, _, _) = V.last filteredVals
                                                    vals1 = V.filter (\(x, _, w) -> x >= xMin && x <= xMax) vals
                                                    fillGaps (x, y, _) = if y == constant then (x, D.interpolate1' x filteredVals) else (x, y)
                                                    newDat = D.data1' $ V.map fillGaps vals1
                                                return $ SD1 newDat
                                    result <- applyToData1 func selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                else
                                    return ()
                            Convert ->
                                if isDiscrete selectedData1 && TSA.Data.is2d selectedData1
                                then do
                                    let
                                        func i d _ =
                                            do
                                                let
                                                    SD1 dat = d
                                                    dataCreateFunc = case typeNo of
                                                        0 -> D.spectrum1
                                                        1 -> D.data1
                                                    newDat = dataCreateFunc (D.values1 dat)
                                                return $ SD1 newDat
                                    result <- applyToData1 func selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                else
                                    return ()
                            Histogram ->
                                if isDiscrete selectedData1 && TSA.Data.dim selectedData1 <= 1 && constant > 1
                                then do
                                    let
                                        opType = case TSA.Data.dim selectedData1 of
                                            0 -> Y
                                            otherwise -> opType
                                        func i d _ =
                                            do
                                                let
                                                    SD1 dat = d
                                                    (vals, minVal, maxVal) = if opType == Y
                                                        then (D.ys dat, D.yMin dat, D.yMax dat)
                                                        else (D.xs1 dat, D.xMin1 dat, D.xMax1 dat)
                                                    step = (maxVal - minVal) / constant
                                                    ranges = [(minVal + step * fromIntegral i, minVal + step * fromIntegral (i + 1)) | i <- [0 .. (round constant) - 1]]
                                                    hist = map (\(xLeft, xRight) ->
                                                        ((xLeft + xRight) / 2, fromIntegral (V.length (V.filter (\x -> x >= xLeft && x < xRight) vals)))) ranges
                                                return $ SD1 $ D.data1' $ V.fromList hist
                                    result <- applyToData1 func selectedData1 name tEnv
                                    modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
                                else
                                    return ()
                            otherwise -> do
                                let
                                    func i sd _ = return $ subDataConstantOp (getModifyFunc op) sd constant (opType == Y) g
                                result <- applyToData1 func selectedData1 name tEnv
                                modifyState stateRef $ addDataParams result (Just (currentGraphTab, selectedGraph))
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

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowSetDefaultSize dialog 600 400
    Gtk.windowPresent dialog

getModifyFunc :: ModifyOp -> F.Function Double
getModifyFunc Add = F.add
getModifyFunc Subtract = F.subtr
getModifyFunc Mean = (F.function "(x+y)/2")
getModifyFunc Multiply = F.mult
getModifyFunc Invert = (F.function "1/x")


getFunction :: Gtk.Window -> String -> Int -> IO (Maybe (F.Function Double))
getFunction dialog f numData = do
    let
        func = F.function f
        varNames = F.varNames func
        opNames = F.funcNames func
    if F.isValid func
        then
            if opNames /= []
                then
                    do
                        showMessageBox dialog ("Missing definitions for " ++ (show opNames))
                        return Nothing
                else
                    if length varNames > numData
                        then
                            do
                                showMessageBox dialog ("Too many function arguments " ++ (show varNames))
                                return Nothing
                        else
                            return $ Just func

        else
            do
                showMessageBox dialog "Error while parsing function"
                return Nothing

showMessageBox :: Gtk.Window -> String -> IO ()
showMessageBox parent msg = do
    messageWindow <- Gtk.windowNew
    Gtk.windowSetTitle messageWindow (Just "Message")
    Gtk.windowSetModal messageWindow True
    Gtk.windowSetTransientFor messageWindow (Just parent)
    vBox <- Gtk.boxNew Gtk.OrientationVertical 8
    Gtk.widgetSetMarginTop vBox 16
    Gtk.widgetSetMarginBottom vBox 16
    Gtk.widgetSetMarginStart vBox 16
    Gtk.widgetSetMarginEnd vBox 16
    label <- Gtk.labelNew (Just (T.pack msg))
    Gtk.boxAppend vBox label
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend vBox buttonBox
    _ <- Gtk.onButtonClicked okButton $ Gtk.windowDestroy messageWindow
    Gtk.windowSetChild messageWindow (Just vBox)
    Gtk.windowPresent messageWindow