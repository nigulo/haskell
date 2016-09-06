
module TSA.GUI.IO (importData, exportData, loadDataDialog, newDataDialog) where

import Graphics.UI.Gtk hiding (addWidget)
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

import GUI.Widget

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
    (currentGraphTab, _) <- getCurrentGraphTab state
    dialog <- fileChooserDialogNew (Just "Read ASCII") (Just (getWindow state)) FileChooserActionOpen [("Cancel", ResponseCancel), ("Open", ResponseAccept)]
    --nameEntry <- entryNew
    --nameEntry `entrySetText` "Input"
    --addWidget (Just "Name: ") nameEntry dialog

    fileFilter <- fileFilterNew
    fileFilter `fileFilterAddPattern` "*.dat"
    fileFilter `fileFilterAddPattern` "*.DAT"
    fileFilter `fileFilterAddPattern` "*.txt"
    fileFilter `fileFilterAddPattern` "*.TXT"
    fileFilter `fileFilterAddPattern` "*.csv"
    fileFilter `fileFilterAddPattern` "*.CSV"
    --fileFilter `fileFilterAddPattern` "*.*"

    (castToFileChooser dialog) `fileChooserAddFilter` fileFilter
    (castToFileChooser dialog) `fileChooserSetFilter` fileFilter

    widgetShowAll dialog
    response <- dialogRun dialog
    --name <- entryGetString nameEntry
    file <- fileChooserGetFilename (castToFileChooser dialog)
    
    if response == ResponseAccept && (file /= Nothing)
        then
            do
                fileContents <- readFile (fromJust file)
                let 
                    fileName = take (length (fromJust file) - 4) (fromJust file)
                    shortName = 
                        let indices = (elemIndices (System.FilePath.pathSeparator) fileName)
                        in if length indices <= 0 then fileName else drop (last indices + 1) fileName
                    fileLines = filter (\line -> trim line /= "") $ lines fileContents
                widgetDestroy dialog
                dataFormatDialog stateRef (readAscii stateRef) fileLines shortName
        else
            do
                widgetDestroy dialog

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
    
    dialogAddButton dialog "Cancel" ResponseCancel
    fitButton <- dialogAddButton dialog "Ok" ResponseOk

    contentBox <- castToBox <$> dialogGetContentArea dialog
    vBox <- vBoxNew False 2
    boxPackStart contentBox vBox PackGrow 2
    
    textBuffer <- textBufferNew Nothing
    textBufferSetText textBuffer ""
    textView <- textViewNewWithBuffer textBuffer
    font <- fontDescriptionNew
    fontDescriptionSetFamily font TSA.GUI.Common.defaultFontFamily
    widgetModifyFont textView (Just font)
    
    scrolledWindow <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledWindow textView
    
    boxPackStart vBox scrolledWindow PackGrow 2

    widgetShowAll dialog
    windowResize dialog 640 480
    response <- dialogRun dialog
    
    if response == ResponseOk 
        then
            do
                startIter <- textBufferGetStartIter textBuffer
                endIter <- textBufferGetEndIter textBuffer
                text <- textBufferGetText textBuffer startIter endIter True
                
                widgetDestroy dialog
                dataFormatDialog stateRef (readAscii stateRef) (filter (not . null) (lines text)) "New data"
        else
            do
                widgetDestroy dialog


data ColumnType = ColGeneral | ColJD | ColYear | ColMonth | ColDay | ColYYYYMMDD deriving (Eq)
columnTypes = [ColGeneral, ColJD, ColYear, ColMonth, ColDay, ColYYYYMMDD]

dataFormatDialog :: StateRef -> ([ColumnType] -> [[String]] -> String -> Bool -> IO ()) -> [String] -> String -> IO ()
dataFormatDialog stateRef callback lines name = do
    state <- readMVar stateRef
    
    assistant <- assistantNew
    
    let
        win = getWindow state
    icon <- windowGetIcon win
    assistant `set` [windowTitle := "Format data columns", windowIcon := icon]
    
    ------------------------
    -- Page 1
    page1 <- vBoxNew False 0
    assistantSetPageType assistant page1 AssistantPageConfirm
    headerLabel1 <- labelNew $ Just "Data to import:"
    addWidgetToBox Nothing headerLabel1 PackNatural page1
    ------------------------
    textBuffer <- textBufferNew Nothing
    textBufferSetText textBuffer (concatMap (++ "\n") lines)
    textView <- textViewNewWithBuffer textBuffer
    font <- fontDescriptionNew
    fontDescriptionSetFamily font TSA.GUI.Common.defaultFontFamily
    widgetModifyFont textView (Just font)
    textViewSetEditable textView False 
    scrolledWindow <- scrolledWindowNew Nothing Nothing
    containerAdd scrolledWindow textView
    boxPackStart page1 scrolledWindow PackGrow 2
    --dataLabel <- labelNew $ Just $ concatMap (++ "\n") (take 5 lines)
    --addWidgetToBox Nothing dataLabel PackNatural page1
    ------------------------
    separator1 <- hSeparatorNew
    addWidgetToBox Nothing separator1 PackNatural page1
    separatorIndicesEntry <- entryNew
    addWidgetToBox (Just "Separator indices:") separatorIndicesEntry PackNatural page1 
    omitSeparatorsCheck <- checkButtonNew
    addWidgetToBox (Just "Omit separators: ") omitSeparatorsCheck PackNatural page1 
    separator2 <- hSeparatorNew
    addWidgetToBox Nothing separator2 PackNatural page1
    delimitersLabel <- labelNew $ Just "Delimiters:"
    addWidgetToBox Nothing delimitersLabel PackNatural page1 
    spaceCheck <- checkButtonNew >>= \button -> toggleButtonSetActive button True >> return button
    addWidgetToBox (Just "Space: ") spaceCheck PackNatural page1 
    tabCheck <- checkButtonNew >>= \button -> toggleButtonSetActive button True >> return button
    addWidgetToBox (Just "Tab: ") tabCheck PackNatural page1
    commaCheck <- checkButtonNew
    addWidgetToBox (Just "Comma: ") commaCheck PackNatural page1
    semicolonCheck <- checkButtonNew
    addWidgetToBox (Just "Semicolon: ") semicolonCheck PackNatural page1
    skipRowsEntry <- entryNew
    addWidgetToBox (Just "Skip rows:") skipRowsEntry PackNatural page1 
    --assistant `set` [assistantChildComplete := True]
    assistantAppendPage assistant page1
    
    ------------------------
    -- Page 2
    page2 <- vBoxNew False 0
    headerLabel2 <- labelNew $ Just "Columns:"
    addWidgetToBox Nothing headerLabel2 PackNatural page2
    scrolledWindow2 <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scrolledWindow2  PolicyNever PolicyAutomatic
    boxPackStart page2 scrolledWindow2 PackGrow 2
    table2 <- tableNew 0 0 True
    scrolledWindowAddWithViewport scrolledWindow2 table2
    --addWidgetToBox Nothing table PackNatural page2
    colIndexComboTable <- tableNew 0 0 True
    addWidgetToBox Nothing colIndexComboTable PackNatural page2
    assistantAppendPage assistant page2
    
    ------------------------
    -- Page 3
    page3 <- vBoxNew False 0
    headerLabel3 <- labelNew $ Just "Columns:"
    addWidgetToBox Nothing headerLabel3 PackNatural page3
    scrolledWindow3 <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scrolledWindow3  PolicyNever PolicyAutomatic
    boxPackStart page3 scrolledWindow3 PackGrow 2
    table3 <- tableNew 0 0 True
    scrolledWindowAddWithViewport scrolledWindow3 table3
    --addWidgetToBox Nothing table3 PackNatural page3
    typeComboTable <- tableNew 0 0 True
    addWidgetToBox Nothing typeComboTable PackNatural page3
    
    nameEntry <- entryNew
    nameEntry `entrySetText` name
    addWidgetToBox (Just "Name: ") nameEntry PackNatural page3
    dataTypeCombo <- createComboBox ["Data", "Spectrum"]
    comboBoxSetActive dataTypeCombo 0
    addWidgetToBox (Just "Type: ") dataTypeCombo PackNatural page3
    assistantAppendPage assistant page3
    
    ------------------------
    widgetShowAll assistant
    assistantSetPageComplete assistant page1 True
    assistantSetPageComplete assistant page2 True
    assistantSetPageComplete assistant page3 True
    assistantSetPageType assistant page1 AssistantPageIntro
    assistantSetPageType assistant page2 AssistantPageContent
    assistantSetPageType assistant page3 AssistantPageConfirm
    
    assistantStateRef <- newIORef ([], [])
    on assistant assistantCancel $ widgetDestroy assistant 
    on assistant assistantPrepare $ \page -> do
        if page == castToWidget page1 then
            do
                writeIORef assistantStateRef ([], [])
                widgetShowAll page1
        else if page == castToWidget page2 then
            do
                useSpace <- toggleButtonGetActive spaceCheck
                useTab <- toggleButtonGetActive tabCheck
                useComma <- toggleButtonGetActive commaCheck
                useSemicolon <- toggleButtonGetActive semicolonCheck
                separatorIndicesStr <- entryGetString separatorIndicesEntry
                omitSeparators <- toggleButtonGetActive omitSeparatorsCheck
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
                        msgDialog <- messageDialogNew (Just (castToWindow assistant)) [DialogDestroyWithParent] MessageWarning ButtonsOk 
                            ("Unequal column numbers detected. Line " ++ (show minLine) ++ " has " ++ (show minCols) ++ " columns, but line " ++ (show maxLine) ++ " has " ++ (show maxCols) ++ " columns.")
                        dialogRun msgDialog
                        widgetDestroy msgDialog
                        return ()
                    else return ()
                tableCells <- containerGetChildren table2
                mapM_ (\cell -> do 
                        containerRemove table2 cell
                        widgetDestroy cell
                    ) tableCells
                zipWithM_ (\row line -> do
                        zipWithM_ (\col elem -> do
                                cell <- labelNew $ Just elem
                                tableAttachDefaults table2 cell col (col + 1) row (row + 1)
                            ) [0, 1 ..] (take numCols line)
                    ) [0, 1 ..] first5Rows
                widgetShowAll table2
                --tableResize table (numRows + 1) numCols
                (colIndexCombos, _) <- readIORef assistantStateRef
                if colIndexCombos == []
                    then
                        do
                            colIndexComboTableCells <- containerGetChildren colIndexComboTable
                            mapM_ (\combo -> do
                                    containerRemove colIndexComboTable combo
                                    widgetDestroy combo
                                ) colIndexComboTableCells
                            colIndexCombos <- mapM (\col -> do
                                    colIndexCombo <- createComboBox ["-", "x", "y", "z", "w"]
                                    if col <= 3
                                        then 
                                            comboBoxSetActive colIndexCombo (col + 1)
                                        else
                                            comboBoxSetActive colIndexCombo 0
                                    tableAttachDefaults colIndexComboTable colIndexCombo col (col + 1) 0 1
                                    return colIndexCombo
                                ) [0 .. numCols - 1] 
                            writeIORef assistantStateRef (colIndexCombos, cols)
                            widgetShowAll colIndexComboTable
                    else
                        return ()
                widgetShowAll page2
        else if page == castToWidget page3 then 
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

                tableCells3 <- containerGetChildren table3
                mapM_ (\cell -> do 
                        containerRemove table3 cell
                        widgetDestroy cell
                    ) tableCells3
                zipWithM_ (\colIndex col -> do
                        zipWithM_ (\rowIndex elem -> do
                                cell <- labelNew $ Just elem
                                tableAttachDefaults table3 cell colIndex (colIndex + 1) rowIndex (rowIndex + 1)
                            ) [0, 1 ..] col
                    ) [0, 1 ..] selectedCols
                widgetShowAll table3

                typeComboTableCells <- containerGetChildren typeComboTable
                mapM_ (\combo -> do
                        containerRemove typeComboTable combo
                        widgetDestroy combo
                    ) typeComboTableCells
                typeCombos <- mapM (\col -> do
                        typeCombo <- createComboBox ["General", "JD", "Year", "Month", "Day", "YYYY-MM-DD"]
                        comboBoxSetActive typeCombo 0
                        tableAttachDefaults typeComboTable typeCombo col (col + 1) 0 1
                        return typeCombo
                    ) [0 .. (length colIndexes) - 1] 
                widgetShowAll typeComboTable
                widgetShowAll page3
        
                on assistant assistantClose $ do
                    colTypesAndCols <- mapM (\(col, typeCombo) -> do
                            colType <- comboBoxGetActive typeCombo
                            return (columnTypes !! colType, selectedCols !! col)
                        ) (zip [0, 1 ..] typeCombos)
                    let
                        (colTypes, selectedCols) = unzip colTypesAndCols
                    name <- entryGetString nameEntry
                    dataType <- comboBoxGetActive dataTypeCombo
                    callback colTypes selectedCols name (dataType == 0)
                    widgetDestroy assistant 
                return ()
        else
            return ()
    return ()

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
    dialog <- fileChooserDialogNew (Just "Import from ISDA") (Just (getWindow state)) FileChooserActionOpen [("Cancel", ResponseCancel), ("Open", ResponseAccept)]
    --nameEntry <- entryNew
    --nameEntry `entrySetText` "Input"
    --addWidget (Just "Name: ") nameEntry dialog

    fileFilter <- fileFilterNew
    fileFilter `fileFilterAddPattern` "*.WRK"
    fileFilter `fileFilterAddPattern` "*.SPC"

    (castToFileChooser dialog) `fileChooserAddFilter` fileFilter
    (castToFileChooser dialog) `fileChooserSetFilter` fileFilter

    widgetShowAll dialog
    response <- dialogRun dialog
    --name <- entryGetString nameEntry
    file <- fileChooserGetFilename (castToFileChooser dialog)
    
    if response == ResponseAccept && (file /= Nothing)
        then
            do
                let 
                    graphTabParms = (graphTabs state) !! currentGraphTab
                    selectedGraph = graphTabSelection graphTabParms

                    extension = reverse (take 4 (reverse (fromJust file)))
                    fileName = take (length (fromJust file) - 4) (fromJust file)
                    mode = if (map toUpper extension) == ".WRK" then "D_" else "S_"
                    shortName = 
                        let indices = (elemIndices '\\' fileName)
                        in if length indices <= 0 then fileName else drop (last indices + 1) fileName
                widgetDestroy dialog
                isdaState <- readStateFromFile fileName (ISDA.InOut.Params (Map.fromList [("CONFIGURATION", mode)]))
                modifyMVar_ stateRef $ \state -> return $ addDiscreteData (decode isdaState) shortName (Just (currentGraphTab, selectedGraph)) state
        else
            do
                widgetDestroy dialog

exportData :: State -> IO ()
exportData state = do
    dialog <- fileChooserDialogNew (Just "Export to ISDA") (Just (getWindow state)) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseAccept)]
    g <- getStdGen 
    
    fileFilter <- fileFilterNew
    fileFilter `fileFilterAddPattern` "*.WRK"
    fileFilter `fileFilterAddPattern` "*.SPC"

    (castToFileChooser dialog) `fileChooserAddFilter` fileFilter
    (castToFileChooser dialog) `fileChooserSetFilter` fileFilter
    
    --yesButton <- dialogAddButton dialog "Yes" ResponseYes
    --yesButton `widgetSetSensitivity` False
    --dialogAddButton dialog "No" ResponseNo
    dataSetCombo <- dataSetComboNew (\_ -> True) state
    addWidget (Just "Data set: ") (getComboBox dataSetCombo) dialog

    precisionAdjustment <- adjustmentNew 1024 1 100000 1 1 1
    precisionSpin <- spinButtonNew precisionAdjustment 1 0
    addWidget (Just "Precision: ") precisionSpin dialog

    {--let 
        toggleYesButton :: IO ()
        toggleYesButton = 
            do
                selectedData <- getSelectedData dataSetCombo
                let sensitivity =
                        case selectedData of 
                            Just _ -> True
                            Nothing -> False
                yesButton `widgetSetSensitivity` sensitivity
                        
    (getComboBox dataSetCombo) `onChanged` toggleYesButton
    --}

    widgetShowAll dialog
    response <- dialogRun dialog
    selectedData <- getSelectedData dataSetCombo
    precision <- spinButtonGetValue precisionSpin
    file <- fileChooserGetFilename (castToFileChooser dialog)
    
    if response == ResponseAccept && (not (isNothing selectedData)) && (file /= Nothing)
        then
            do
                widgetDestroy dialog
                let
                    Just f = file
                    fileName = 
                        if any (\suffix -> suffix `isSuffixOf` (map toLower f)) [".wrk", ".spc"]  
                            then (take (length f - 4) f) 
                            else f 
                    isdaState = encode d where
                        d = case unboxSubData $ subData (head (dataSet (fromJust selectedData))) of
                            Left dataOrSpec -> dataOrSpec
                            Right ad -> sampleAnalyticData_ ad [round precision] g
                writeStateToFile fileName isdaState
        else 
            widgetDestroy dialog

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

