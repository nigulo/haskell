
module TSA.GUI.Data (
    module TSA.Data,
    infoDialog,
    dataSetComboNew,
    dataSetComboNew2,
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


import TSA.Params
import TSA.Data
import TSA.GUI.State
import TSA.GUI.Dialog
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
                addWidgetToVBox (Just "Description: ") descEntry PackNatural dataPage
                descEntry `entrySetText` (dataDesc dp)
                typeLabel <- labelNew $ Just $ getDataType dp
                addWidgetToVBox (Just "Type:") typeLabel PackNatural dataPage
                componentCountLabel <- labelNew $ Just $ show (length (dataSet dp))
                addWidgetToVBox (Just "Components:") componentCountLabel PackNatural dataPage
                if (isDiscrete dp) 
                    then do
                        numPointsLabel <- labelNew $ Just $ show (sum (map (\sdp -> let Left d = subData sdp in D.dataLength d) (dataSet dp)))
                        addWidgetToVBox (Just "Number of points:") numPointsLabel PackNatural dataPage
                    else
                        return ()
                bootstrapCountLabel <- labelNew $ Just $ show $ length (subDataBootstrapSet (head (dataSet dp))) 
                addWidgetToVBox (Just "Bootstrap samples:") bootstrapCountLabel PackNatural dataPage

                detailsButton <- buttonNewWithLabel "Show info..."
                on detailsButton buttonReleaseEvent $ liftIO (showInfo state dp >> return True)
                addWidgetToVBox Nothing detailsButton PackNatural dataPage
                dataButton <- buttonNewWithLabel "Show data..."
                on dataButton buttonReleaseEvent $ liftIO (showData dp >> return True)
                addWidgetToVBox Nothing dataButton PackNatural dataPage

                exportButton <- buttonNewFromStock stockSave
                on exportButton buttonReleaseEvent $ liftIO (exportData state dp >> return True)
                addWidgetToVBox Nothing exportButton PackNatural dataPage
                
                return $ pages ++ [(dataPage, dataName dp, descEntry, typeLabel)]
    
    dataPages <- foldM (createDataPage) [] [0 .. length dParams - 1] 
    pagesRef <- newIORef dataPages
    
    dialog <- dialogWithTitle state "Data sets"
    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    notebook <- notebookNew
    notebookSetScrollable notebook True
    
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
    boxPackStart vBox notebook PackNatural 2

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
                
    dialogAddButton dialog "Cancel" ResponseCancel
    dialogAddButton dialog "Ok" ResponseOk

    widgetShowAll dialog
    
    response <- dialogRun dialog 

    onClose response

    widgetDestroy dialog

formatRangeBound :: [Double] -> String
formatRangeBound (rangeBound1:rangeBound) = show rangeBound1 ++ concatMap (\rb -> ", " ++ show rb) rangeBound

showInfo :: State -> DataParams -> IO ()
showInfo state dp = do
    textBuffer <- textBufferNew Nothing

    textBufferSetText textBuffer $ "No Left Right Count\n" ++ (concatMap (\(sdp, i) -> 
        let
            (rangeStart, rangeEnd) = subDataRange sdp
            (left, right, n) = case subData sdp of
                Left d -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, D.dataLength d)
                Right (Left s) -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, 0)
                Right (Right f) -> (formatRangeBound rangeStart, formatRangeBound rangeEnd, 0)  
        in
            (show i) ++ ": " ++ left ++ " - " ++ right ++ (if isDiscrete dp then " " ++ show n else "") ++ "\n"
        ) $ zip (dataSet dp) [1, 2 ..])
    textView <- textViewNewWithBuffer textBuffer
    font <- fontDescriptionNew
    fontDescriptionSetFamily font "Arial"
    widgetModifyFont textView (Just font)
    textViewSetEditable textView False 

    win <- windowNew
    icon <- pixbufNewFromFile "tsa.bmp"
    win `set` [windowTitle := dataName dp, windowIcon := Just icon]
    
    vAdjustment <- adjustmentNew 0 0 100 1 10 10
    scrolledWindow <- scrolledWindowNew Nothing (Just vAdjustment)

    containerAdd win scrolledWindow
    
    containerAdd scrolledWindow textView
    windowResize win 640 480
    widgetShowAll win

showData :: DataParams -> IO ()
showData dp = do
    textBuffer <- textBufferNew Nothing
    let
        useSegmentPrefix = length (dataSet dp) > 1 
    textBufferSetText textBuffer $ concatMap (\(sdp, i) ->
        let
            (rangeStart, rangeEnd) = subDataRange sdp
        in     
            (if useSegmentPrefix 
                then "Segment " ++ show i ++ " (" ++ formatRangeBound rangeStart ++ " - " ++ formatRangeBound rangeEnd ++ ")\n" 
                else ""
            ) ++ U.format (subData sdp) ++ "\n"
        ) $ zip (dataSet dp) [1, 2 ..]
    textView <- textViewNewWithBuffer textBuffer
    font <- fontDescriptionNew
    fontDescriptionSetFamily font "Arial"
    widgetModifyFont textView (Just font)
    textViewSetEditable textView False 

    win <- windowNew
    icon <- pixbufNewFromFile "tsa.bmp"
    win `set` [windowTitle := dataName dp, windowIcon := Just icon]
    
    vAdjustment <- adjustmentNew 0 0 100 1 10 10
    scrolledWindow <- scrolledWindowNew Nothing (Just vAdjustment)

    containerAdd win scrolledWindow
    
    containerAdd scrolledWindow textView
    windowResize win 640 480
    widgetShowAll win

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
             Left d -> D.is2d d
             Right (Left s) -> AD.is2d s
             Right (Right ad) -> AD.is2d ad

dataFilter :: Bool -> DataFilter
dataFilter trueFalse dp =
    case subData (head (dataSet dp)) of 
             Left (Data _) -> trueFalse
             Left (Data2 _) -> trueFalse
             Left (Data3 _) -> trueFalse
             Left (Spectrum _) -> not trueFalse
             Left (Spectrum2 _) -> not trueFalse
             _ -> False

onlyData :: DataFilter
onlyData = dataFilter True

onlySpectrum :: DataFilter
onlySpectrum = dataFilter False

dataAndSpectrum :: DataFilter
dataAndSpectrum dp = 
    case subData (head (dataSet dp)) of 
             Left _ -> True
             _ -> False

splineAndSpectrum :: DataFilter
splineAndSpectrum dp = 
    case subData (head (dataSet dp)) of 
             Left _ -> onlySpectrum dp
             Right _ -> True

onlyAnalytic :: DataFilter
onlyAnalytic = isAnalytic

andFilter :: DataFilter -> DataFilter -> DataFilter
andFilter filter1 filter2 = \dp -> filter1 dp && filter2 dp

orFilter :: DataFilter -> DataFilter -> DataFilter
orFilter filter1 filter2 = \dp -> filter1 dp || filter2 dp

--------------------------------------------------------------------------------
-- | Data set combo box
type DataComboBox = (ComboBox, M.Map String DataParams)

dataSetComboNew :: DataFilter -> State -> IO DataComboBox
dataSetComboNew filterFunc state = dataSetComboNew2 filterFunc state True

dataSetComboNew2 :: DataFilter -> State -> Bool -> IO DataComboBox
dataSetComboNew2 filterFunc state allowNothing = do
    dataSetCombo <- createComboBox []
    let 
        dataNamesAndSets = map (\dp -> (dataName dp, dp)) $ filter filterFunc (dataParams (params state)) where
        --dataNames = map dataName $ filter filterFunc (dataParams state) where
        nameDataMap = M.fromList dataNamesAndSets
    mapM_ (\str -> comboBoxAppendText dataSetCombo (fromString str)) (fst (unzip dataNamesAndSets))
    if allowNothing then comboBoxSetActive dataSetCombo 0 else return ()
    return (dataSetCombo, nameDataMap)

getComboBox :: DataComboBox -> ComboBox
getComboBox (combo, _) = combo

getSelectedData :: DataComboBox -> IO (Maybe DataParams)
getSelectedData (comboBox, nameDataMap) = 
    do
        selectedText <- comboBoxGetActiveString comboBox
        case selectedText of
            Just text -> return $ M.lookup text nameDataMap
            Nothing -> return Nothing

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
                                        Left dat ->
                                            if D.is3d dat
                                                then (0, 0)
                                                else
                                                    if D.isSpectrum dat then (0, 1) else (1, 0)
                                        Right (Left spline) -> (0, 1)
                                        Right (Right ad) ->
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

addOrUpdateSegmentedData :: [Either D.Data (Either S.Spline FS.Functions)] -> String -> Maybe (Int, Int) -> Bool -> State -> State
addOrUpdateSegmentedData ds name tabIndex update state = 
    addOrUpdateDataParams (createDataParams_ name (map (\d -> createSubDataParams__ d) ds)) tabIndex update state

addSegmentedData :: [Either D.Data (Either S.Spline FS.Functions)] -> String -> Maybe (Int, Int) -> State -> State
addSegmentedData ds name tabIndex = addOrUpdateSegmentedData ds name tabIndex False

addOrUpdateData :: Either D.Data (Either S.Spline FS.Functions) -> String -> Maybe (Int, Int) -> Bool -> State -> State
addOrUpdateData d = addOrUpdateSegmentedData [d]

addData :: Either D.Data (Either S.Spline FS.Functions) -> String -> Maybe (Int, Int) -> State -> State
addData d name tabIndex = addOrUpdateSegmentedData [d] name tabIndex False

addSpline :: S.Spline -> String -> Maybe (Int, Int) -> State -> State
addSpline s = addData (Right (Left s))

addFunction :: FS.Functions -> String -> Maybe (Int, Int) -> State -> State
addFunction f = addData (Right (Right f))

addDiscreteData :: D.Data -> String -> Maybe (Int, Int) -> State -> State
addDiscreteData d name tabIndex state = addData (Left d) name tabIndex state

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
