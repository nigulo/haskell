{-# LANGUAGE TypeApplications #-}

module TSA.GUI.IO (importData, exportData, loadDataDialog, newDataDialog) where

import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import Data.GI.Base
import qualified Data.Text as T
import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent
import Control.Monad
import Debug.Trace

import Regression.Data as D hiding (ws)
import Regression.Utils

import ISDA.InOut

import TSA.CommonParams
import TSA.Params
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Data
import TSA.GUI.Common

import GUI.Widget hiding (entryGetString)

import Utils.Misc
import Utils.Str
import Utils.List
import Data.Char
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Control.Concurrent.MVar
import System.IO
import System.Random
import System.FilePath

import Ephem.Types

import qualified Data.Vector.Unboxed as V
import Control.Applicative

loadDataDialog :: StateRef -> IO ()
loadDataDialog stateRef = do
    state <- readMVar stateRef
    dialog <- Gtk.fileDialogNew
    Gtk.fileDialogSetTitle dialog "Read ASCII"

    fileFilter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern fileFilter "*.dat"
    Gtk.fileFilterAddPattern fileFilter "*.DAT"
    Gtk.fileFilterAddPattern fileFilter "*.txt"
    Gtk.fileFilterAddPattern fileFilter "*.TXT"
    Gtk.fileFilterAddPattern fileFilter "*.csv"
    Gtk.fileFilterAddPattern fileFilter "*.CSV"
    Gtk.fileFilterSetName fileFilter (Just "ASCII data files")

    filters <- Gio.listStoreNew =<< glibType @Gtk.FileFilter
    Gio.listStoreAppend filters fileFilter
    Gtk.fileDialogSetFilters dialog (Just filters)
    Gtk.fileDialogSetDefaultFilter dialog (Just fileFilter)

    Gtk.fileDialogOpen dialog (Just (getWindow state)) (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
        file <- Gtk.fileDialogOpenFinish dialog result
        maybePath <- Gio.fileGetPath file
        case maybePath of
            Just path -> do
                let fileName = path
                fileContents <- readFile fileName
                let
                    fileName' = take (length fileName - 4) fileName
                    shortName =
                        let indices = (elemIndices (System.FilePath.pathSeparator) fileName')
                        in if length indices <= 0 then fileName' else drop (last indices + 1) fileName'
                    fileLines = filter (\line -> trim line /= "") $ lines fileContents
                (currentGraphTab, _) <- getCurrentGraphTab state
                dataFormatDialog stateRef (readAscii stateRef) fileLines shortName
            Nothing -> return ()

newDataDialog :: StateRef -> IO ()
newDataDialog stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    let
        graphTabParms = (graphTabs state) !! currentGraphTab
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        ga = graphArea graphParms

    dialog <- dialogWithTitle state "Create new data"

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 4
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    textBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
    Gtk.textBufferSetText textBuffer (T.pack "") (-1)
    textView <- Gtk.textViewNewWithBuffer textBuffer
    Gtk.textViewSetMonospace textView True

    scrolledWindow <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild scrolledWindow (Just textView)
    Gtk.widgetSetVexpand scrolledWindow True

    Gtk.boxAppend contentBox scrolledWindow

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    okButton <- Gtk.buttonNewWithLabel "Ok"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox okButton
    Gtk.boxAppend contentBox buttonBox

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy dialog

    _ <- Gtk.onButtonClicked okButton $ do
        startIter <- Gtk.textBufferGetStartIter textBuffer
        endIter <- Gtk.textBufferGetEndIter textBuffer
        text <- Gtk.textBufferGetText textBuffer startIter endIter True

        Gtk.windowDestroy dialog
        dataFormatDialog stateRef (readAscii stateRef) (filter (not . null) (lines (T.unpack text))) "New data"

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowSetDefaultSize dialog 640 480
    Gtk.windowPresent dialog


data ColumnType = ColGeneral | ColJD | ColYear | ColMonth | ColDay | ColYYYYMMDD deriving (Eq)
columnTypes = [ColGeneral, ColJD, ColYear, ColMonth, ColDay, ColYYYYMMDD]

dataFormatDialog :: StateRef -> ([ColumnType] -> [[String]] -> String -> Bool -> IO ()) -> [String] -> String -> IO ()
dataFormatDialog stateRef callback lines name = do
    state <- readMVar stateRef

    assistant <- Gtk.assistantNew

    Gtk.windowSetTitle assistant (Just "Format data columns")

    ------------------------
    -- Page 1
    page1 <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.widgetSetName page1 "page1"
    Gtk.assistantSetPageType assistant page1 Gtk.AssistantPageTypeConfirm
    headerLabel1 <- Gtk.labelNew (Just "Data to import:")
    addWidgetToBox Nothing headerLabel1 page1
    ------------------------
    textBuffer <- Gtk.textBufferNew (Nothing :: Maybe Gtk.TextTagTable)
    Gtk.textBufferSetText textBuffer (T.pack (concatMap (++ "\n") lines)) (-1)
    textView <- Gtk.textViewNewWithBuffer textBuffer
    Gtk.textViewSetMonospace textView True
    Gtk.textViewSetEditable textView False
    scrolledWindow <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetChild scrolledWindow (Just textView)
    Gtk.widgetSetVexpand scrolledWindow True
    Gtk.boxAppend page1 scrolledWindow
    --dataLabel <- Gtk.labelNew (Just (T.pack (concatMap (++ "\n") (take 5 lines))))
    --addWidgetToBox Nothing dataLabel page1
    ------------------------
    separator1 <- Gtk.separatorNew Gtk.OrientationHorizontal
    addWidgetToBox Nothing separator1 page1
    separatorIndicesEntry <- Gtk.entryNew
    addWidgetToBox (Just "Separator indices:") separatorIndicesEntry page1
    omitSeparatorsCheck <- Gtk.checkButtonNew
    addWidgetToBox (Just "Omit separators: ") omitSeparatorsCheck page1
    separator2 <- Gtk.separatorNew Gtk.OrientationHorizontal
    addWidgetToBox Nothing separator2 page1
    delimitersLabel <- Gtk.labelNew (Just "Delimiters:")
    addWidgetToBox Nothing delimitersLabel page1
    spaceCheck <- Gtk.checkButtonNew
    Gtk.checkButtonSetActive spaceCheck True
    addWidgetToBox (Just "Space: ") spaceCheck page1
    tabCheck <- Gtk.checkButtonNew
    Gtk.checkButtonSetActive tabCheck True
    addWidgetToBox (Just "Tab: ") tabCheck page1
    commaCheck <- Gtk.checkButtonNew
    addWidgetToBox (Just "Comma: ") commaCheck page1
    semicolonCheck <- Gtk.checkButtonNew
    addWidgetToBox (Just "Semicolon: ") semicolonCheck page1
    skipRowsEntry <- Gtk.entryNew
    addWidgetToBox (Just "Skip rows:") skipRowsEntry page1
    --assistant `set` [assistantChildComplete := True]
    _ <- Gtk.assistantAppendPage assistant page1

    ------------------------
    -- Page 2
    page2 <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.widgetSetName page2 "page2"
    headerLabel2 <- Gtk.labelNew (Just "Columns:")
    addWidgetToBox Nothing headerLabel2 page2
    scrolledWindow2 <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetPolicy scrolledWindow2 Gtk.PolicyTypeNever Gtk.PolicyTypeAutomatic
    Gtk.widgetSetVexpand scrolledWindow2 True
    Gtk.boxAppend page2 scrolledWindow2
    table2 <- Gtk.gridNew
    Gtk.scrolledWindowSetChild scrolledWindow2 (Just table2)
    colIndexComboTable <- Gtk.gridNew
    addWidgetToBox Nothing colIndexComboTable page2
    _ <- Gtk.assistantAppendPage assistant page2

    ------------------------
    -- Page 3
    page3 <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.widgetSetName page3 "page3"
    headerLabel3 <- Gtk.labelNew (Just "Columns:")
    addWidgetToBox Nothing headerLabel3 page3
    scrolledWindow3 <- Gtk.scrolledWindowNew
    Gtk.scrolledWindowSetPolicy scrolledWindow3 Gtk.PolicyTypeNever Gtk.PolicyTypeAutomatic
    Gtk.widgetSetVexpand scrolledWindow3 True
    Gtk.boxAppend page3 scrolledWindow3
    table3 <- Gtk.gridNew
    Gtk.scrolledWindowSetChild scrolledWindow3 (Just table3)
    typeComboTable <- Gtk.gridNew
    addWidgetToBox Nothing typeComboTable page3

    nameEntry <- Gtk.entryNew
    entrySetText nameEntry name
    addWidgetToBox (Just "Name: ") nameEntry page3
    dataTypeCombo <- createComboBox ["Data", "Spectrum"]
    comboBoxSetActive dataTypeCombo 0
    addWidgetToBox (Just "Type: ") dataTypeCombo page3
    _ <- Gtk.assistantAppendPage assistant page3

    ------------------------
    Gtk.assistantSetPageComplete assistant page1 True
    Gtk.assistantSetPageComplete assistant page2 True
    Gtk.assistantSetPageComplete assistant page3 True
    Gtk.assistantSetPageType assistant page1 Gtk.AssistantPageTypeIntro
    Gtk.assistantSetPageType assistant page2 Gtk.AssistantPageTypeContent
    Gtk.assistantSetPageType assistant page3 Gtk.AssistantPageTypeConfirm

    assistantStateRef <- newIORef ([], [])
    typeCombosRef <- newIORef []
    selectedColsRef <- newIORef []
    _ <- Gtk.onAssistantCancel assistant $ Gtk.windowDestroy assistant
    _ <- Gtk.onAssistantPrepare assistant $ \page -> do
        pageName <- Gtk.widgetGetName page
        if pageName == "page1"
            then
                do
                    writeIORef assistantStateRef ([], [])
                    Gtk.widgetShow page1
            else if pageName == "page2"
                then
                    do
                        useSpace <- Gtk.checkButtonGetActive spaceCheck
                        useTab <- Gtk.checkButtonGetActive tabCheck
                        useComma <- Gtk.checkButtonGetActive commaCheck
                        useSemicolon <- Gtk.checkButtonGetActive semicolonCheck
                        separatorIndicesStr <- entryGetString separatorIndicesEntry
                        omitSeparators <- Gtk.checkButtonGetActive omitSeparatorsCheck
                        skippedRowsStr <- entryGetString skipRowsEntry
                        let
                            separatorOffset = if omitSeparators then 1 else 0
                            separatorIndices :: [Int] = sort $ map (read) $ filter (\ind -> trim ind /= "") $ concat $ map (splitBy ';') $ concat $ map (splitBy ',') $ words separatorIndicesStr
                            splitFunc useChar char line = if useChar then concat (map (splitBy char) line) else line
                            splittedLines' = map (\line -> filter (\col -> trim col /= "") (
                                ((splitFunc useSpace ' ') . (splitFunc useTab '\t') . (splitFunc useComma ',') . (splitFunc useSemicolon ';'))
                                    (if separatorIndices /= [] then map (\(i1, i2) -> take (i2 - i1 - separatorOffset) (drop (i1 + separatorOffset) line)) (zip (-separatorOffset:separatorIndices) (separatorIndices ++ [length line])) else [line])
                                )) lines
                            skippedRows :: [Int] = sort $ map (read) $ filter (\ind -> trim ind /= "") $ concat $ map (splitBy ';') $ concat $ map (splitBy ',') $ words skippedRowsStr
                            splittedLines = foldl' (\lines (skippedRow, i) -> removeAt (skippedRow - i) lines) splittedLines' (zip skippedRows [1 ..])
                            first5Rows = splittedLines --take 5 splittedLines
                            colsAndLengths = map (\(i, line) -> (i, length line)) (zip [1, 2 ..] splittedLines)
                            (minLine, minCols) = minimumBy (\(_, len1) (_, len2) -> compare len1 len2) colsAndLengths
                            (maxLine, maxCols) = maximumBy (\(_, len1) (_, len2) -> compare len1 len2) colsAndLengths
                            cols = transpose $ map (take minCols) splittedLines
                            numRows = length first5Rows
                            numCols = length cols
                        if minCols /= maxCols
                            then do
                                showMessageBox Nothing ("Unequal column numbers detected. Line " ++ (show minLine) ++ " has " ++ (show minCols) ++ " columns, but line " ++ (show maxLine) ++ " has " ++ (show maxCols) ++ " columns.")
                            else return ()
                        clearGrid table2
                        zipWithM_ (\row line -> do
                                zipWithM_ (\col elem -> do
                                        cell <- Gtk.labelNew (Just (T.pack elem))
                                        Gtk.gridAttach table2 cell (fromIntegral col) (fromIntegral row) 1 1
                                    ) [0, 1 ..] (take numCols line)
                            ) [0, 1 ..] first5Rows
                        (colIndexCombos, _) <- readIORef assistantStateRef
                        if colIndexCombos == []
                            then
                                do
                                    clearGrid colIndexComboTable
                                    colIndexCombos <- mapM (\col -> do
                                            colIndexCombo <- createComboBox ["-", "x", "y", "z", "w"]
                                            if col <= 3
                                                then
                                                    comboBoxSetActive colIndexCombo (col + 1)
                                                else
                                                    comboBoxSetActive colIndexCombo 0
                                            Gtk.gridAttach colIndexComboTable colIndexCombo (fromIntegral col) 0 1 1
                                            return colIndexCombo
                                        ) [0 .. numCols - 1]
                                    writeIORef assistantStateRef (colIndexCombos, cols)
                            else
                                return ()
            else if pageName == "page3" then
                do
                    (colIndexCombos, cols) <- readIORef assistantStateRef
                    maybeColIndexes <- zipWithM (\col colIndexCombo -> do
                            colIndex <- comboBoxGetActive colIndexCombo
                            case colIndex of
                                0 -> return Nothing
                                i -> return $ Just (i, col)
                        ) [0, 1 ..] colIndexCombos
                    let
                        colIndexes = sortBy (\(i1, _) (i2, _) -> compare i1 i2) (catMaybes maybeColIndexes)
                        selectedCols = map (\(_, col) -> cols !! col) colIndexes

                    clearGrid table3
                    zipWithM_ (\colIndex col -> do
                            zipWithM_ (\rowIndex elem -> do
                                    cell <- Gtk.labelNew (Just (T.pack elem))
                                    Gtk.gridAttach table3 cell (fromIntegral colIndex) (fromIntegral rowIndex) 1 1
                                ) [0, 1 ..] col
                        ) [0, 1 ..] selectedCols

                    clearGrid typeComboTable
                    typeCombos <- mapM (\col -> do
                            typeCombo <- createComboBox ["General", "JD", "Year", "Month", "Day", "YYYY-MM-DD"]
                            comboBoxSetActive typeCombo 0
                            Gtk.gridAttach typeComboTable typeCombo (fromIntegral col) 0 1 1
                            return typeCombo
                        ) [0 .. (length colIndexes) - 1]
                    writeIORef typeCombosRef typeCombos
                    writeIORef selectedColsRef selectedCols
            else
                return ()
    _ <- Gtk.onAssistantClose assistant $ do
        typeCombos <- readIORef typeCombosRef
        selectedCols <- readIORef selectedColsRef
        colTypesAndCols <- mapM (\(col, typeCombo) -> do
                colType <- comboBoxGetActive typeCombo
                return (columnTypes !! colType, selectedCols !! col)
            ) (zip [0, 1 ..] typeCombos)
        let
            (colTypes, selectedCols') = unzip colTypesAndCols
        name <- entryGetString nameEntry
        dataType <- comboBoxGetActive dataTypeCombo
        callback colTypes selectedCols' name (dataType == 0)
        Gtk.windowDestroy assistant
    Gtk.windowPresent assistant
    return ()

showMessageBox :: Maybe Gtk.Window -> String -> IO ()
showMessageBox parent msg = do
    messageWindow <- Gtk.windowNew
    Gtk.windowSetTitle messageWindow (Just "Message")
    Gtk.windowSetModal messageWindow True
    case parent of
        Just w -> Gtk.windowSetTransientFor messageWindow (Just w)
        Nothing -> return ()
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

clearGrid :: Gtk.Grid -> IO ()
clearGrid grid = do
    maybeChild <- Gtk.widgetGetFirstChild grid
    case maybeChild of
        Nothing -> return ()
        Just child -> do
            maybeNext <- Gtk.widgetGetNextSibling child
            Gtk.gridRemove grid child
            clearGridFrom grid maybeNext

clearGridFrom :: Gtk.Grid -> Maybe Gtk.Widget -> IO ()
clearGridFrom grid (Just child) = do
    maybeNext <- Gtk.widgetGetNextSibling child
    Gtk.gridRemove grid child
    clearGridFrom grid maybeNext
clearGridFrom _ Nothing = return ()

readValue :: ColumnType -> String -> Integer -> Double
readValue ColYYYYMMDD str currentYear =
    let
        str2 = filter (/= '-') str
        (year, monthDateStr) = if length str2 > 6 then (read (take 4 str2), drop 4 str2) else
            let
                y =  read $ take 2 str2
            in
                (if 2000 + y > currentYear then 1900  + y else 2000 + y, drop 2 str2)
        (monthStr, dateStr) = splitAt 2 monthDateStr
        TropicalYears result = toTropicalYears (YMD year (read monthStr) (read dateStr))
    in
        result
readValue _ val _ = read val

readAscii :: StateRef -> [ColumnType] -> [[String]] -> String -> Bool -> IO ()
readAscii stateRef [] _ _ _ = return ()
readAscii stateRef colTypes cols name dataOrSpectrum = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    (year, month, day) <- getCurrentTime >>= return . toGregorian . utctDay
    let
            ymdToYears = colTypes !! 0 == ColYear && colTypes !! 1 == ColMonth && colTypes !! 2 == ColDay
            dataLines =
                map (\line ->
                        let
                            lineVals = zipWith (\i val -> readValue (colTypes !! i) val year) [0, 1 ..] line
                        in
                            if ymdToYears
                                then
                                     let
                                         [year, month, day] = take 3 lineVals
                                         TropicalYears result = toTropicalYears (YMD (floor year) (floor month) day)
                                     in
                                         result:drop 3 lineVals
                                else lineVals
                    ) $ transpose cols
            numCols = if ymdToYears then length colTypes - 2 else length colTypes
            is1d = numCols == 1
            is2d = numCols == 2
            dat =
                if is1d then
                        D.data0 $ V.fromList $ map (\line ->
                            if length line == 1
                                then (head line, 1)
                                else (head line, last line)) dataLines
                else if is2d
                    then
                        let
                            dataCreateFunc = if dataOrSpectrum then D.data1 else D.spectrum1
                        in
                            dataCreateFunc $ V.fromList $ map (\line ->
                                if length line == 2
                                    then (head line, last line, 1)
                                    else (head line, line !! 1, last line)) dataLines
                    else
                        D.data2 $ V.fromList $ map (\line ->
                            if length line == 3
                                then (head line, line !! 1, last line, 1)
                                else (head line, line !! 1, line !! 2, last line)) dataLines
            graphTabParms = (graphTabs state) !! currentGraphTab
            selectedGraph = graphTabSelection graphTabParms
    modifyMVar_ stateRef $ \state -> return $ addDiscreteData dat name (Just (currentGraphTab, selectedGraph)) state

-------------------------------------------------------------------------------
-- Import/Export from ISDA

importData :: StateRef -> IO ()
importData stateRef = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    dialog <- Gtk.fileDialogNew
    Gtk.fileDialogSetTitle dialog "Import from ISDA"

    fileFilter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern fileFilter "*.WRK"
    Gtk.fileFilterAddPattern fileFilter "*.SPC"
    Gtk.fileFilterSetName fileFilter (Just "ISDA files")

    filters <- Gio.listStoreNew =<< glibType @Gtk.FileFilter
    Gio.listStoreAppend filters fileFilter
    Gtk.fileDialogSetFilters dialog (Just filters)
    Gtk.fileDialogSetDefaultFilter dialog (Just fileFilter)

    Gtk.fileDialogOpen dialog (Just (getWindow state)) (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
        file <- Gtk.fileDialogOpenFinish dialog result
        maybePath <- Gio.fileGetPath file
        case maybePath of
            Just path -> do
                let
                    f = path
                    graphTabParms = (graphTabs state) !! currentGraphTab
                    selectedGraph = graphTabSelection graphTabParms

                    extension = reverse (take 4 (reverse f))
                    fileName = take (length f - 4) f
                    mode = if (map toUpper extension) == ".WRK" then "D_" else "S_"
                    shortName =
                        let indices = (elemIndices '\\' fileName)
                        in if length indices <= 0 then fileName else drop (last indices + 1) fileName
                isdaState <- readStateFromFile fileName (ISDA.InOut.Params (Map.fromList [("CONFIGURATION", mode)]))
                modifyMVar_ stateRef $ \state -> return $ addDiscreteData (decode isdaState) shortName (Just (currentGraphTab, selectedGraph)) state
            Nothing -> return ()

exportData :: State -> IO ()
exportData state = do
    g <- getStdGen

    dialog <- Gtk.windowNew
    Gtk.windowSetTitle dialog (Just "Export to ISDA")
    Gtk.windowSetModal dialog True
    Gtk.windowSetTransientFor dialog (Just (getWindow state))

    contentBox <- Gtk.boxNew Gtk.OrientationVertical 8
    Gtk.widgetSetMarginTop contentBox 8
    Gtk.widgetSetMarginBottom contentBox 8
    Gtk.widgetSetMarginStart contentBox 8
    Gtk.widgetSetMarginEnd contentBox 8

    dataSetCombo <- dataSetComboNew (\_ -> True) state
    addWidgetToBox (Just "Data set: ") (getComboBox dataSetCombo) contentBox

    precisionAdjustment <- Gtk.adjustmentNew 1024 1 100000 1 1 1
    precisionSpin <- Gtk.spinButtonNew (Just precisionAdjustment) 1 0
    addWidgetToBox (Just "Precision: ") precisionSpin contentBox

    fileEntry <- Gtk.entryNew
    addWidgetToBox (Just "File name: ") fileEntry contentBox

    -- Button box
    buttonBox <- Gtk.boxNew Gtk.OrientationHorizontal 4
    Gtk.widgetSetHalign buttonBox Gtk.AlignEnd
    cancelButton <- Gtk.buttonNewWithLabel "Cancel"
    saveButton <- Gtk.buttonNewWithLabel "Save"
    Gtk.boxAppend buttonBox cancelButton
    Gtk.boxAppend buttonBox saveButton
    Gtk.boxAppend contentBox buttonBox

    _ <- Gtk.onButtonClicked cancelButton $ Gtk.windowDestroy dialog
    _ <- Gtk.onButtonClicked saveButton $ do
        selectedData <- getSelectedData dataSetCombo
        precision <- spinButtonGetValue precisionSpin
        f <- entryGetString fileEntry
        case selectedData of
            Just _ -> do
                let
                    fileName =
                        if any (\suffix -> suffix `isSuffixOf` (map toLower f)) [".wrk", ".spc"]
                            then (take (length f - 4) f)
                            else f
                    isdaState = encode d where
                        d = case unboxSubData $ subData (head (dataSet (fromJust selectedData))) of
                            Left dataOrSpec -> dataOrSpec
                            Right ad -> sampleAnalyticData_ ad [round precision] g
                Gtk.windowDestroy dialog
                writeStateToFile fileName isdaState
            Nothing -> return ()

    Gtk.windowSetChild dialog (Just contentBox)
    Gtk.windowSetDefaultSize dialog 500 200
    Gtk.windowPresent dialog

decode :: ISDAState -> D.Data
decode (params, datOrSpec) =
    case datOrSpec of
        Left (dataHeader, dataBlocks) ->
            D.data1 $ V.fromList $ (zip3 (map (+tOff dataHeader) (ts dataBlocks))
                         (map realToFrac (fs dataBlocks))
                         (if length (ws dataBlocks) <= 0 then
                            replicate (length (ts dataBlocks)) 1
                          else map realToFrac (ws dataBlocks)))
        Right (specHeader, specBlocks) ->
            D.Spectrum2 ((swMin specHeader,
                        (if nLim specHeader <= 1 then 0
                         else (swMax specHeader - swMin specHeader) / (fromIntegral (nLim specHeader) - 1))),
                         V.zip (V.map realToFrac (V.fromList (pty specBlocks))) (V.replicate (length (pty specBlocks)) 1))


encode :: D.Data -> ISDAState
encode dat =
    if D.isData dat
        then
            let
                (xs, ys, wgs) = unzip3 $ D.values dat
            in
                (ISDA.InOut.Params (Map.fromList [("CONFIGURATION", "_D")]),
                    Left (DataHeader {bands = 3,
                                  curSegs = 0,
                                  nData = fromIntegral (length xs),
                                  tOff = 0,
                                  fMin = minimum ys,
                                  fMax = maximum ys,
                                  isIndex = False},
                       DataBlocks {ts = map head xs,
                                  dataSegs = [],
                                  fs = map realToFrac ys,
                                  ws = map realToFrac wgs,
                                  is = []}))
        else
                let
                    D.Spectrum2 ((offset, step), valuesAndWeights) = dat
                    values = V.toList $ fst $ V.unzip valuesAndWeights
                in
                    (ISDA.InOut.Params (Map.fromList [("CONFIGURATION", "_S")]),
                         Right (SpecHeader {nLim = fromIntegral (length values),
                                           swMin = offset,
                                           swMax = offset + (if length values <= 1 then 0 else fromIntegral (length values - 1) * step)},
                               SpecBlocks {pty = map realToFrac values}))