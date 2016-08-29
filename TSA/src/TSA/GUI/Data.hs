
module TSA.GUI.Data (
    module TSA.Data,
    infoDialog,
    dataSetComboNew,
    dataSetComboNew2,
    dataComboBoxSetMandatory,
    only2d,
    onlyData,
    onlySpectrum,
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

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.VBox
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
import GUI.Widget
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
                
                dataPage <- vBoxNew True 0
                
                descEntry <- entryNew
                addWidgetToBox (Just "Description: ") descEntry PackNatural dataPage
                descEntry `entrySetText` (dataDesc dp)
                typeLabel <- labelNew $ Just $ TSA.Data.getDataType dp
                addWidgetToBox (Just "Type:") typeLabel PackNatural dataPage
                componentCountLabel <- labelNew $ Just $ show (length (dataSet dp))
                addWidgetToBox (Just "Components:") componentCountLabel PackNatural dataPage
                if (TSA.Data.isDiscrete dp) 
                    then do
                        numPointsLabel <- labelNew $ Just $ show (sum (map (\sdp -> let SD1 d = subData sdp in D.dataLength d) (dataSet dp)))
                        addWidgetToBox (Just "Number of points:") numPointsLabel PackNatural dataPage
                    else
                        return ()

                hBox <- hBoxNew False 2
                addWidgetToBox Nothing hBox PackNatural dataPage

                detailsButton <- buttonNewWithLabel "Show info..."
                on detailsButton buttonReleaseEvent $ liftIO (showInfo dp >> return True)
                boxPackStart hBox detailsButton PackNatural 2
                dataButton <- buttonNewWithLabel "Show data..."
                on dataButton buttonReleaseEvent $ liftIO (showData dp >> return True)
                boxPackStart hBox dataButton PackNatural 2

                exportButton <- buttonNewFromStock stockSave
                on exportButton buttonReleaseEvent $ liftIO (exportData state dp >> return True)
                boxPackEnd hBox exportButton PackNatural 2
                
                return $ pages ++ [(dataPage, dataName dp, descEntry, typeLabel)]
    
    dataPages <- foldM (createDataPage) [] [0 .. length dParams - 1] 
    pagesRef <- newIORef dataPages
    
    dialog <- dialogWithTitle state "Data sets"
    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    notebook <- notebookNew
    notebookSetTabPos notebook PosLeft
    notebookSetScrollable notebook True
    boxPackStart vBox notebook PackGrow 2
    
    let
        deleteData page = 
            do
                Just pageIndex <- notebookPageNum notebook page
                pages <- readIORef pagesRef
                let dataPage@(page, name, _, _) = pages !! pageIndex
                notebookRemovePage notebook pageIndex
                modifyIORef pagesRef (\pages -> 
                    deleteBy (\(_, name1, _, _) (_, name2, _, _) -> name1 == name2) dataPage pages)
                modifyMVar_ stateRef $ \state -> return $ removeDataByName name state

        mapOp (page, name, _, _) =
            do
                pageIndex <- notebookGetNPages notebook
                label <- labelWithButton (Just name) stockDelete (deleteData page)
                notebookAppendPageMenu notebook page label label
                widgetShowAll label
    
    mapM_ mapOp dataPages

    let
        onClose response =
            do
                if response == ResponseOk
                    then 
                        do
                            pages <- readIORef pagesRef
                            mapM_ (\i -> modifyMVar_ stateRef $ \state ->
                                    do
                                        let 
                                            (_, name, descEntry, _) = pages !! i
                                            dParams = getDataByName name state
                                        desc <- entryGetString descEntry
                                        return $ updateData (dParams {dataDesc = desc}) state
                                ) [0 .. length pages - 1] 
                    else
                        return ()
                
    dialogAddButton dialog "Ok" ResponseOk
    widgetShowAll dialog
    --notebookSetPopup notebook True
    response <- dialogRun dialog 
    onClose response
    widgetDestroy dialog

formatRangeBound :: [Double] -> String
formatRangeBound (rangeBound1:rangeBound) = show rangeBound1 ++ concatMap (\rb -> ", " ++ show rb) rangeBound

showInfo :: DataParams -> IO ()
showInfo dp = do
    textBuffer <- textBufferNew Nothing

    textBufferSetText textBuffer $ "No Left Right Count Mean Var\n" ++ (concatMap (\(sdp, i) -> 
        let
            (rangeStart, rangeEnd) = subDataRange sdp
            (left, right, n, (mean, var)) = case subData sdp of
                SD1 d -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, D.dataLength d, meanVarianceUnb (D.ys d))
                SD2 s -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, 0, (0, 0))
                SD3 f -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, 0, (0, 0))  
                SD4 rbf -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, 0, (0, 0))  
        in
            (show i) ++ ": " ++ left ++ " - " ++ right ++ (if TSA.Data.isDiscrete dp then " " ++ (show n ++ " " ++ show mean ++ " " ++ show var) else "") ++ "\n"
        ) $ zip (dataSet dp) [1, 2 ..])

    textView <- textViewNewWithBuffer textBuffer
    font <- fontDescriptionNew
    fontDescriptionSetFamily font TSA.GUI.Common.defaultFontFamily
    widgetModifyFont textView (Just font)
    textViewSetEditable textView False 
    
    win <- windowNew
    icon <- pixbufNewFromFile "tsa.bmp"
    win `set` [windowTitle := dataName dp, windowIcon := Just icon]
    
    scrolledWindow <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledWindow textView
    containerAdd win scrolledWindow
    
    widgetShowAll win
    windowResize win 640 480

showData :: DataParams -> IO ()
showData dp = do
    textBuffer <- textBufferNew Nothing
    let
        useSegmentPrefix = length (dataSet dp) > 1 

    textBufferSetText textBuffer $ concatMap (\(sdp, i) ->
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
        ) $ zip (dataSet dp) [1, 2 ..]

    textView <- textViewNewWithBuffer textBuffer
    font <- fontDescriptionNew
    fontDescriptionSetFamily font TSA.GUI.Common.defaultFontFamily
    widgetModifyFont textView (Just font)
    textViewSetEditable textView False 

    win <- windowNew
    icon <- pixbufNewFromFile "tsa.bmp"
    win `set` [windowTitle := dataName dp, windowIcon := Just icon]
    
    scrolledWindow <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledWindow textView
    containerAdd win scrolledWindow
    
    widgetShowAll win
    windowResize win 640 480

exportData :: State -> DataParams -> IO ()
exportData state dp = do
    dialog <- fileChooserDialogNew (Just "Export data") (Just (getWindow state)) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseAccept)]
    
    fileFilter <- fileFilterNew
    fileFilter `fileFilterAddPattern` "*"

    (castToFileChooser dialog) `fileChooserAddFilter` fileFilter
    (castToFileChooser dialog) `fileChooserSetFilter` fileFilter

    fileChooserSetFilename (castToFileChooser dialog) (dataName dp)
    widgetShowAll dialog
    response <- dialogRun dialog
    file <- fileChooserGetFilename (castToFileChooser dialog)
    
    if response == ResponseAccept && (file /= Nothing)
        then
            do
                widgetDestroy dialog
                let
                    Just f = file
                Xml.renderToFile (Xml.toDocument dp) f
        else 
            widgetDestroy dialog

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

dataAndSpectrum :: DataFilter
dataAndSpectrum dp = 
    case subData (head (dataSet dp)) of 
        SD1 _ -> True
        otherwise -> False

splineAndSpectrum :: DataFilter
splineAndSpectrum dp = 
    case subData (head (dataSet dp)) of 
        SD1 _ -> onlySpectrum dp
        SD2 _ -> True
        otherwise -> False

onlyAnalytic :: DataFilter
onlyAnalytic = TSA.Data.isAnalytic

andFilter :: DataFilter -> DataFilter -> DataFilter
andFilter filter1 filter2 = \dp -> filter1 dp && filter2 dp

orFilter :: DataFilter -> DataFilter -> DataFilter
orFilter filter1 filter2 = \dp -> filter1 dp || filter2 dp

--------------------------------------------------------------------------------
-- | Data set combo box
type DataComboBox = (ComboBox, IORef [Maybe DataParams])

dataSetComboNew :: DataFilter -> State -> IO DataComboBox
dataSetComboNew filterFunc state = dataSetComboNew2 filterFunc state True

dataSetComboNew2 :: DataFilter -> State -> Bool -> IO DataComboBox
dataSetComboNew2 filterFunc state mandatory = do
    dataSetCombo <- createComboBox []
    let 
        dataSets = (if mandatory then [] else [Nothing]) ++(map (Just) $ filter filterFunc (dataParams (params state)))
    mapM_ (\maybeDp -> 
            case maybeDp of
                Just dp ->
                    comboBoxAppendText dataSetCombo (fromString (dataName dp))
                Nothing -> 
                    comboBoxAppendText dataSetCombo (fromString "[Select data]")
        ) dataSets
    comboBoxSetActive dataSetCombo 0
    dataSetsRef <- newIORef dataSets
    return (dataSetCombo, dataSetsRef)

getComboBox :: DataComboBox -> ComboBox
getComboBox (combo, _) = combo

--dataComboBoxSetOnChanged :: DataComboBox -> IO () -> IO ()
--dataComboBoxSetOnChanged (comboBox, dataSetsRef, fn) fn

dataComboBoxSetMandatory :: DataComboBox -> Bool -> IO ()
dataComboBoxSetMandatory (comboBox, dataSetsRef) True = do
    dataSets <- readIORef dataSetsRef
    let
        removeVoidEntry = 
            if null dataSets 
                then False
                else
                    case head dataSets of
                        Nothing -> True
                        otherwise -> False
    if removeVoidEntry
        then do
            index <- comboBoxGetActive comboBox
            writeIORef dataSetsRef (tail dataSets)
             -- writeIORef must be called before changing the state of comboBox,
             -- otherwise we might get infinite loop, if this function is inside
             -- registered change event handler
            comboBoxRemoveText comboBox 0
            comboBoxSetActive comboBox (max 0 (index - 1))
        else 
            return ()
dataComboBoxSetMandatory (comboBox, dataSetsRef) False = do
    dataSets <- readIORef dataSetsRef
    let
        addVoidEntry = 
            if null dataSets 
                then True
                else
                    case head dataSets of
                        Nothing -> False
                        otherwise -> True
    if addVoidEntry
        then do
            index <- comboBoxGetActive comboBox
             -- writeIORef must be called before changing the state of comboBox,
             -- otherwise we might get infinite loop, if this function is inside
             -- registered change event handler
            writeIORef dataSetsRef (Nothing:dataSets)
            comboBoxPrependText comboBox (fromString "[Select data]")
            if (index >= 0)
                then
                    comboBoxSetActive comboBox (index + 1)
                else
                    return ()
        else
            return ()

getSelectedData :: DataComboBox -> IO (Maybe DataParams)
getSelectedData (comboBox, dataSetsRef) = 
    do
        index <- comboBoxGetActive comboBox
        dataSets <- readIORef dataSetsRef
        if index >= 0
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
            dataNamesAndSets = map (\dp -> (dataName dp, dp)) $ filter filterFunc (dataParams (params state)) where
            nameDataMap = M.fromList dataNamesAndSets
        itemChooser <- itemChooserNew (Just "Select data") "" (map (fst) dataNamesAndSets) "" [] 
        return (itemChooser, nameDataMap)

dataSetChooserToWidget (itemChooser, _) = itemChooserToWidget itemChooser

dataSetChooserGetChoice :: DataSetChooser -> IO [DataParams]
dataSetChooserGetChoice (itemChooser, nameDataMap) = 
    do
        dataSetNames <- itemChooserGetChoice itemChooser
        return $ map (\name -> fromJust (M.lookup name nameDataMap)) dataSetNames 

        
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
                                            if D.is3d dat
                                                then (0, 0)
                                                else
                                                    if D.isSpectrum dat then (0, 1) else (1, 0)
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

