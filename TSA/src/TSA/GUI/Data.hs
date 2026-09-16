{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

module TSA.GUI.Data (
    module TSA.Data,
    infoDialog,
    dataSetComboNew,
    dataSetComboNew2,
    dataComboBoxSetMandatory,
    only2d,
    onlyData,
    onlySpectrum,
    onlyEvenlySampled,
    splineAndSpectrum,
    dataAndSpectrum,
    onlyAnalytic,
    andFilter,
    orFilter,
    ---------------
    DataFilter,
    DataComboBox,
    getComboBox,
    getSelectedData,
    ---------------
    DataSetChooser,
    dataSetChooserNew,
    dataSetChooserToWidget,
    dataSetChooserGetChoice,
    ---------------
    addOrUpdateSegmentedData,
    addSegmentedData,
    addOrUpdateData,
    addData,
    addSpline,
    addFunction,
    addDiscreteData,
    addOrUpdateDataParams,
    addDataParams,
    getDataByName,
    findDataByName,
    removeDataByName,
    removeDataByNameFromTab,
    updateData

    ) where

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.GObject as GObject
import Data.GI.Base
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

import Debug.Trace
import qualified Data.Map as M

import Regression.Data as D
import Regression.AnalyticData as AD
import Regression.Functions as FS
import Regression.Spline as S
import Regression.Utils as U
import Math.Expression as E


import TSA.CommonParams
import TSA.Params
import TSA.Data
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Common
import GUI.Widget hiding (entryGetString)
import GUI.Plot
import Utils.List
import Utils.Concurrent
import Utils.Xml as Xml

import Data.IORef
import Data.Either
import Data.Maybe
import Control.Concurrent.MVar
import Data.List
import Data.String
import Control.Applicative

import qualified Data.Vector.Unboxed as V
import Control.Monad.IO.Class
import Control.Monad
import Statistics.Sample

infoDialog :: StateRef -> IO ()
infoDialog stateRef = do
    state <- readMVar stateRef
    dParams <- return (dataParams (params state))

    let
        createDataPage pages i =
            do
                let dp = dParams !! i

                dataPage <- Gtk.boxNew Gtk.OrientationVertical 0

                descEntry <- Gtk.entryNew
                addWidgetToBox (Just "Description: ") descEntry dataPage
                entrySetText descEntry (dataDesc dp)
                typeLabel <- Gtk.labelNew $ Just $ T.pack $ TSA.Data.getDataType dp
                addWidgetToBox (Just "Type:") typeLabel dataPage
                componentCountLabel <- Gtk.labelNew $ Just $ T.pack $ show (length (dataSet dp))
                addWidgetToBox (Just "Components:") componentCountLabel dataPage
                if (TSA.Data.isDiscrete dp)
                    then do
                        numPointsLabel <- Gtk.labelNew $ Just $ T.pack $ show (sum (map (\sdp -> let SD1 d = subData sdp in D.dataLength d) (dataSet dp)))
                        addWidgetToBox (Just "Number of points:") numPointsLabel dataPage
                    else
                        return ()

                hBox <- Gtk.boxNew Gtk.OrientationHorizontal 2
                addWidgetToBox Nothing hBox dataPage

                detailsButton <- Gtk.buttonNewWithLabel "Show info..."
                _ <- Gtk.onButtonClicked detailsButton $ showInfo dp
                Gtk.boxAppend hBox detailsButton
                dataButton <- Gtk.buttonNewWithLabel "Show data..."
                _ <- Gtk.onButtonClicked dataButton $ showData dp
                Gtk.boxAppend hBox dataButton

                exportButton <- Gtk.buttonNewWithLabel "Export"
                _ <- Gtk.onButtonClicked exportButton $ exportData state dp
                Gtk.boxAppend hBox exportButton

                return $ pages ++ [(dataPage, dataName dp, descEntry, typeLabel)]

    dataPages <- foldM (createDataPage) [] [0 .. length dParams - 1]
    pagesRef <- newIORef dataPages

    win <- dialogWithTitle state "Data sets"
    contentBox <- Gtk.boxNew Gtk.OrientationVertical 2
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    notebook <- Gtk.notebookNew
    Gtk.notebookSetTabPos notebook Gtk.PositionTypeLeft
    Gtk.notebookSetScrollable notebook True
    Gtk.widgetSetVexpand notebook True
    Gtk.boxAppend contentBox notebook

    let
        deleteData page =
            do
                pageIndex <- Gtk.notebookPageNum notebook page
                pages <- readIORef pagesRef
                let dataPage@(_, name, _, _) = pages !! (fromIntegral pageIndex)
                Gtk.notebookRemovePage notebook pageIndex
                modifyIORef pagesRef (\pages ->
                    deleteBy (\(_, name1, _, _) (_, name2, _, _) -> name1 == name2) dataPage pages)
                modifyMVar_ stateRef $ \state -> return $ removeDataByName name state

        mapOp (page, name, _, _) =
            do
                label <- labelWithButton (Just (T.pack name)) "edit-delete" (deleteData page)
                _ <- Gtk.notebookAppendPage notebook page (Just label)
                return ()

    mapM_ mapOp dataPages

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy win
    _ <- Gtk.onButtonClicked okButton $ do
        pages <- readIORef pagesRef
        mapM_ (\i -> modifyMVar_ stateRef $ \state ->
                do
                    let
                        (_, name, descEntry, _) = pages !! i
                        dParams = getDataByName name state
                    desc <- entryGetString descEntry
                    return $ updateData (dParams {dataDesc = desc}) state
            ) [0 .. length pages - 1]
        Gtk.windowDestroy win

    Gtk.windowSetChild win (Just contentBox)
    Gtk.windowSetDefaultSize win 600 400
    Gtk.windowPresent win

formatRangeBound :: [Double] -> String
formatRangeBound (rangeBound1:rangeBound) = show rangeBound1 ++ concatMap (\rb -> ", " ++ show rb) rangeBound

showInfo :: DataParams -> IO ()
showInfo dp = do
    textBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)

    Gtk.textBufferSetText textBuffer (T.pack $ "No Left Right Count Mean Var\n" ++ (concatMap (\(sdp, i) ->
        let
            (rangeStart, rangeEnd) = subDataRange sdp
            (left, right, n, (mean, var)) = case subData sdp of
                SD1 d -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, D.dataLength d, meanVarianceUnb (D.ys d))
                SD2 s -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, 0, (0, 0))
                SD3 f -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, 0, (0, 0))
                SD4 rbf -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, 0, (0, 0))
        in
            (show i) ++ ": " ++ left ++ " - " ++ right ++ (if TSA.Data.isDiscrete dp then " " ++ (show n ++ " " ++ show mean ++ " " ++ show var) else "") ++ "\n"
        ) $ zip (dataSet dp) [1, 2 ..])) (-1)

    textView <- Gtk.textViewNewWithBuffer textBuffer
    Gtk.textViewSetEditable textView False

    win <- Gtk.windowNew
    Gtk.windowSetTitle win (Just (T.pack (dataName dp)))

    scrolledWindow <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild scrolledWindow (Just textView)
    Gtk.widgetSetVexpand scrolledWindow True
    Gtk.windowSetChild win (Just scrolledWindow)

    Gtk.windowSetDefaultSize win 640 480
    Gtk.windowPresent win

showData :: DataParams -> IO ()
showData dp = do
    textBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
    let
        useSegmentPrefix = length (dataSet dp) > 1

    Gtk.textBufferSetText textBuffer (T.pack $ concatMap (\(sdp, i) ->
        let
            (rangeStart, rangeEnd) = subDataRange sdp

            format :: SubData -> String
            format (SD1 d) = concatMap (\(xs, y, w) -> (concatMap (\x -> show x ++ " ") xs) ++ show y ++ " " ++ show w ++ "\n") (D.values d)
            format (SD2 ad) = show ad
            format (SD3 ad) = show ad
            format (SD4 ad) = show ad
        in
            (if useSegmentPrefix
                then "Segment " ++ show i ++ " (" ++ formatRangeBound rangeStart ++ " - " ++ formatRangeBound rangeEnd ++ ")\n"
                else ""
            ) ++ format (subData sdp) ++ "\n"
        ) $ zip (dataSet dp) [1, 2 ..]) (-1)

    textView <- Gtk.textViewNewWithBuffer textBuffer
    Gtk.textViewSetEditable textView False

    win <- Gtk.windowNew
    Gtk.windowSetTitle win (Just (T.pack (dataName dp)))

    scrolledWindow <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild scrolledWindow (Just textView)
    Gtk.widgetSetVexpand scrolledWindow True
    Gtk.windowSetChild win (Just scrolledWindow)

    Gtk.windowSetDefaultSize win 640 480
    Gtk.windowPresent win

exportData :: State -> DataParams -> IO ()
exportData state dp = do
    win <- Gtk.windowNew
    Gtk.windowSetTitle win (Just "Export data")
    Gtk.windowSetModal win True
    Gtk.windowSetTransientFor win (Just (getWindow state))

    vBox <- Gtk.boxNew Gtk.OrientationVertical 8
    Gtk.widgetSetMarginTop vBox 8
    Gtk.widgetSetMarginBottom vBox 8
    Gtk.widgetSetMarginStart vBox 8
    Gtk.widgetSetMarginEnd vBox 8

    fileEntry <- Gtk.entryNew
    entrySetText fileEntry (dataName dp)
    _ <- addWidget (Just "File name: ") fileEntry vBox

    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    saveButton <- Gtk.buttonNewWithLabel "Save"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox saveButton
    Gtk.boxAppend vBox buttonBox

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy win
    _ <- Gtk.onButtonClicked saveButton $ do
        fileName <- entryGetString fileEntry
        Xml.renderToFile (Xml.toDocument dp) fileName
        Gtk.windowDestroy win

    Gtk.windowSetChild win (Just vBox)
    Gtk.windowPresent win

type DataFilter = DataParams -> Bool

only2d :: DataFilter
only2d dp =
    case subData (head (dataSet dp)) of
        SD1 d -> D.is2d d
        SD2 s -> AD.is2d s
        SD3 ad -> AD.is2d ad
        SD4 ad -> AD.is2d ad

dataFilter :: Bool -> DataFilter
dataFilter trueFalse dp =
    case subData (head (dataSet dp)) of
        SD1 (Data2 _) -> trueFalse
        SD1 (Data3 _) -> trueFalse
        SD1 (Spectrum2 _) -> not trueFalse
        _ -> False

onlyData :: DataFilter
onlyData = dataFilter True

onlySpectrum :: DataFilter
onlySpectrum = dataFilter False

onlyEvenlySampled :: DataFilter
onlyEvenlySampled dp =
    case subData (head (dataSet dp)) of
        SD1 d -> D.isEvenlySampled d
        _ -> False

dataAndSpectrum :: DataFilter
dataAndSpectrum dp =
    case subData (head (dataSet dp)) of
        SD1 _ -> True
        _ -> False

splineAndSpectrum :: DataFilter
splineAndSpectrum dp =
    case subData (head (dataSet dp)) of
        SD1 _ -> onlySpectrum dp
        SD2 _ -> True
        _ -> False

onlyAnalytic :: DataFilter
onlyAnalytic = TSA.Data.isAnalytic

andFilter :: DataFilter -> DataFilter -> DataFilter
andFilter filter1 filter2 = \dp -> filter1 dp && filter2 dp

orFilter :: DataFilter -> DataFilter -> DataFilter
orFilter filter1 filter2 = \dp -> filter1 dp || filter2 dp

--------------------------------------------------------------------------------
-- | Data set combo box (uses GTK4 DropDown)
data DataComboBox = DataComboBox Gtk.DropDown (IORef [Maybe DataParams])

dataSetComboNew :: DataFilter -> State -> IO DataComboBox
dataSetComboNew filterFunc state = dataSetComboNew2 filterFunc state True

dataSetComboNew2 :: DataFilter -> State -> Bool -> IO DataComboBox
dataSetComboNew2 filterFunc state mandatory = do
    let
        dataSets = (if mandatory then [] else [Nothing]) ++ (map Just $ filter filterFunc (dataParams (params state)))
        names = map (\maybeDp ->
                case maybeDp of
                    Just dp -> T.pack (dataName dp)
                    Nothing -> "[Select data]"
            ) dataSets
    dropDown <- Gtk.dropDownNewFromStrings names
    Gtk.dropDownSetSelected dropDown 0
    dataSetsRef <- newIORef dataSets
    return (DataComboBox dropDown dataSetsRef)

getComboBox :: DataComboBox -> Gtk.DropDown
getComboBox (DataComboBox combo _) = combo

dataComboBoxSetMandatory :: DataComboBox -> Bool -> IO ()
dataComboBoxSetMandatory (DataComboBox dropDown dataSetsRef) True = do
    dataSets <- readIORef dataSetsRef
    let
        removeVoidEntry =
            if null dataSets
                then False
                else
                    case head dataSets of
                        Nothing -> True
                        _ -> False
    if removeVoidEntry
        then do
            idx <- Gtk.dropDownGetSelected dropDown
            let newDataSets = tail dataSets
            writeIORef dataSetsRef newDataSets
            -- Rebuild dropdown
            let names = map (\case Just dp -> T.pack (dataName dp); Nothing -> "[Select data]") newDataSets
            rebuildDropDown dropDown names
            Gtk.dropDownSetSelected dropDown (max 0 (idx - 1))
        else
            return ()
dataComboBoxSetMandatory (DataComboBox dropDown dataSetsRef) False = do
    dataSets <- readIORef dataSetsRef
    let
        addVoidEntry =
            if null dataSets
                then True
                else
                    case head dataSets of
                        Nothing -> False
                        _ -> True
    if addVoidEntry
        then do
            idx <- Gtk.dropDownGetSelected dropDown
            let newDataSets = Nothing:dataSets
            writeIORef dataSetsRef newDataSets
            let names = map (\case Just dp -> T.pack (dataName dp); Nothing -> "[Select data]") newDataSets
            rebuildDropDown dropDown names
            if idx /= maxBound
                then Gtk.dropDownSetSelected dropDown (idx + 1)
                else return ()
        else
            return ()

-- Helper to rebuild a DropDown's model
rebuildDropDown :: Gtk.DropDown -> [T.Text] -> IO ()
rebuildDropDown dropDown names = do
    newModel <- Gtk.stringListNew (Just names)
    Gtk.dropDownSetModel dropDown (Just newModel)

getSelectedData :: DataComboBox -> IO (Maybe DataParams)
getSelectedData (DataComboBox dropDown dataSetsRef) =
    do
        idx <- Gtk.dropDownGetSelected dropDown
        dataSets <- readIORef dataSetsRef
        let index = fromIntegral idx
        if index >= 0 && index < length dataSets
            then
                return $ dataSets !! index
            else
                return Nothing

--------------------------------------------------------------------------------
-- | Data set chooser
type DataSetChooser = (ItemChooser, M.Map String DataParams)

dataSetChooserNew :: DataFilter -> State -> IO DataSetChooser
dataSetChooserNew filterFunc state =
    do
        let
            dataNamesAndSets = map (\dp -> (dataName dp, dp)) $ filter filterFunc (dataParams (params state))
            nameDataMap = M.fromList dataNamesAndSets
        itemChooser <- itemChooserNew (Just "Select data") "" (map (T.pack . fst) dataNamesAndSets) "" []
        return (itemChooser, nameDataMap)

dataSetChooserToWidget :: DataSetChooser -> Gtk.Frame
dataSetChooserToWidget (itemChooser, _) = itemChooserGetWidget itemChooser

dataSetChooserGetChoice :: DataSetChooser -> IO [DataParams]
dataSetChooserGetChoice (itemChooser, nameDataMap) =
    do
        dataSetNames <- itemChooserGetChoice itemChooser
        return $ map (\name -> fromJust (M.lookup (T.unpack name) nameDataMap)) dataSetNames


--------------------------------------------------------------------------------

addOrUpdateDataParams :: DataParams -> Maybe (Int, Int) -> Bool -> State -> State
addOrUpdateDataParams dp tabIndex update state =
    if ((dataName dp) `elem` (map dataName (dataParams (params state))))
        then if update then updateData dp state else state
        else
            state {
                params = (params state) {dataParams = (dataParams (params state)) ++ [dp]},
                graphTabs =
                    case tabIndex of
                        Just (i, graphIndex) ->
                            let
                                graphTabParms = (graphTabs state) !! i
                                graphParms = graphTabGraphs graphTabParms !! graphIndex
                                (pointSize, lineWidth) =
                                    case subData (head (dataSet dp)) of
                                        SD1 dat ->
                                            case D.dim dat of
                                                0 -> (0, 1)
                                                1 -> if D.isSpectrum dat then (0, 1) else (1, 0)
                                                2 -> (0, 0)
                                        SD2 spline -> (0, 1)
                                        SD3 ad ->
                                            if AD.is3d ad
                                                then (0, 0)
                                                else (0, 1)
                                        SD4 ad ->
                                            if AD.is3d ad
                                                then (0, 0)
                                                else (0, 1)
                            in
                                updateAt i (
                                    graphTabParms {
                                        graphTabGraphs = updateAt graphIndex (
                                            graphParms {
                                                graphData = (graphData graphParms) ++
                                                    [GraphDataParams {
                                                        graphDataParamsName = (dataName dp),
                                                        graphDataParamsDesc = (dataDesc dp),
                                                        graphDataParamsColor = (0, 0, 0),
                                                        graphDataParamsPointType = GUI.Plot.Plus,
                                                        graphDataParamsPointSize = pointSize,
                                                        graphDataParamsLineDash = [1, 0],
                                                        graphDataParamsLineWidth = lineWidth,
                                                        graphDataParamsErrorBars = False
                                                    }]
                                            }
                                        ) (graphTabGraphs graphTabParms)
                                    }
                                ) (graphTabs state)
                        Nothing -> graphTabs state
            }

addDataParams :: DataParams -> Maybe (Int, Int) -> State -> State
addDataParams dp tabIndex state = addOrUpdateDataParams dp tabIndex False state

addOrUpdateSegmentedData :: [SubData] -> String -> Maybe (Int, Int) -> Bool -> State -> State
addOrUpdateSegmentedData ds name tabIndex update state =
    addOrUpdateDataParams (TSA.Data.createDataParams_ name (map (\d -> TSA.Data.createSubDataParams_ d) ds)) tabIndex update state

addSegmentedData :: [SubData] -> String -> Maybe (Int, Int) -> State -> State
addSegmentedData ds name tabIndex = addOrUpdateSegmentedData ds name tabIndex False

addOrUpdateData :: SubData -> String -> Maybe (Int, Int) -> Bool -> State -> State
addOrUpdateData d = addOrUpdateSegmentedData [d]

addData :: SubData -> String -> Maybe (Int, Int) -> State -> State
addData d name tabIndex = addOrUpdateSegmentedData [d] name tabIndex False

addSpline :: S.Spline -> String -> Maybe (Int, Int) -> State -> State
addSpline s = addData (SD2 s)

addFunction :: FS.Functions -> String -> Maybe (Int, Int) -> State -> State
addFunction f = addData (SD3 f)

addDiscreteData :: D.Data -> String -> Maybe (Int, Int) -> State -> State
addDiscreteData d name tabIndex state = addData (SD1 d) name tabIndex state

getDataByName :: String -> State -> DataParams
getDataByName name state =
    let
        Just d = find (\dp -> dataName dp == name) (dataParams (params state))
    in d

findDataByName :: String -> State -> Maybe DataParams
findDataByName name state = find (\dp -> dataName dp == name) (dataParams (params state))

updateData :: DataParams -> State -> State
updateData dParams state =
    let i = findIndex (\dp -> dataName dp == dataName dParams) (dataParams (params state))
    in
        case i of
            Nothing ->  state
            Just i -> state {params = (params state) {dataParams = h ++ [dParams] ++ (tail t)}} where
                (h, t) = splitAt i (dataParams (params state))

removeDataByName :: String -> State -> State
removeDataByName name state =
    let
        dataSets = filter (\dp -> dataName dp /= name) (dataParams (params state))
        numTabs = length (graphTabs state)
        newState = foldl' (\s i -> removeDataByNameFromTab i name s) state [0 .. numTabs - 1]
    in
        newState {params = (params newState) {dataParams = dataSets}}

removeDataByNameFromTab :: Int -> String -> State -> State
removeDataByNameFromTab tabIndex name state =
    let
        graphTabParms = (graphTabs state) !! tabIndex
        cols = graphTabCols graphTabParms
        newState = foldl' (\s i -> removeDataByNameFromTab' tabIndex (i `quot` cols) (i `rem` cols) name s) state [0 .. length (graphTabGraphs graphTabParms) - 1]
    in
        newState

removeDataByNameFromTab' :: Int -> Int -> Int -> String -> State -> State
removeDataByNameFromTab' tabIndex row col name state =
    let
        graphTabParms = (graphTabs state) !! tabIndex
        graphIndex = (graphTabCols graphTabParms) * row + col
        graphParms = graphTabGraphs graphTabParms !! graphIndex
        dataSets = filter (\dp -> graphDataParamsName dp /= name) (graphData graphParms)
    in
        state {graphTabs = updateAt tabIndex (graphTabParms {graphTabGraphs = updateAt graphIndex (graphParms {graphData = dataSets}) (graphTabGraphs graphTabParms)}) (graphTabs state)}
