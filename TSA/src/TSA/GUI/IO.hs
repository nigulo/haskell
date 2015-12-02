
module TSA.GUI.IO (importData, exportData, loadAsciiDialog) where

import Graphics.UI.Gtk hiding (addWidget)
import Data.IORef
import qualified Data.Map as Map
import Control.Concurrent
import Debug.Trace

import Regression.Data as D hiding (ws)
import Regression.Utils

import ISDA.InOut

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Data

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

loadAsciiDialog :: StateRef -> IO ()
loadAsciiDialog stateRef = do
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
                dataFormatDialog stateRef (readAscii stateRef shortName) fileLines
        else
            do
                widgetDestroy dialog

data ColumnType = ColGeneral | ColJD | ColYear | ColMonth | ColDay | ColYYYYMMDD | ColError deriving (Eq)
columnTypes = [ColGeneral, ColJD, ColYear, ColMonth, ColDay, ColYYYYMMDD, ColError]

dataFormatDialog :: StateRef -> ([ColumnType] -> [[String]] -> String -> Bool -> IO ()) -> [String] -> IO ()
dataFormatDialog stateRef callback lines = do
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
    headerLabel <- labelNew $ Just "First rows of data:"
    addWidgetToBox Nothing headerLabel PackNatural page1
    dataLabel <- labelNew $ Just $ concatMap (++ "\n") (take 5 lines)
    addWidgetToBox Nothing dataLabel PackNatural page1
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
    headerLabel <- labelNew $ Just "Columns:"
    addWidgetToBox Nothing headerLabel PackNatural page2
    table <- tableNew 0 0 True
    addWidgetToBox Nothing table PackNatural page2
    nameEntry <- entryNew
    addWidgetToBox (Just "Name: ") nameEntry PackNatural page2
    dataTypeCombo <- createComboBox ["Data", "Spectrum"]
    comboBoxSetActive dataTypeCombo 0
    addWidgetToBox (Just "Type: ") dataTypeCombo PackNatural page2
    assistantAppendPage assistant page2
    
    widgetShowAll assistant
    assistantSetPageComplete assistant page1 True
    assistantSetPageComplete assistant page2 True
    assistantSetPageType assistant page2 AssistantPageConfirm
    
    on assistant assistantCancel $ widgetDestroy assistant 
    on assistant assistantPrepare $ \page -> do
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
            first5Rows = take 5 splittedLines
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
        tableCells <- containerGetChildren table
        mapM_ (containerRemove table) tableCells
        tableResize table (numRows + 1) numCols
        mapM_ (\(row, line) -> mapM_ (\(col, elem) -> do
                        cell <- labelNew $ Just elem
                        tableAttachDefaults table cell col (col + 1) row (row + 1)
                ) (zip [0, 1 ..] line)
            ) (zip [0, 1 ..] (map (take numCols) first5Rows)) 
        typeCombos <- mapM (\col -> do
                typeCombo <- createComboBox ["-", "General", "JD", "Year", "Month", "Day", "YYYY-MM-DD", "Error"]
                comboBoxSetActive typeCombo 1
                tableAttachDefaults table typeCombo col (col + 1) (numRows) (numRows + 1)
                return typeCombo
            ) [0 .. numCols - 1] 
        widgetShowAll page2
        on assistant assistantClose $ do
            colTypesAndCols <- mapM (\(col, typeCombo) -> do
                    colType <- comboBoxGetActive typeCombo
                    case colType of
                        0 -> return Nothing
                        i -> return $ Just (columnTypes !! (i - 1), cols !! col)
                ) (zip [0, 1 ..] typeCombos)
            let
                (colTypes, selectedCols) = unzip $ catMaybes colTypesAndCols
            name <- entryGetString nameEntry
            dataType <- comboBoxGetActive dataTypeCombo
            callback colTypes selectedCols name (dataType == 0)
            widgetDestroy assistant 
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

readAscii :: StateRef -> String -> [ColumnType] -> [[String]] -> String -> Bool -> IO ()
readAscii stateRef _ [] _ _ _ = return ()
readAscii stateRef fileName colTypes cols name dataOrSpectrum = do
    state <- readMVar stateRef
    (currentGraphTab, _) <- getCurrentGraphTab state
    (year, month, day) <- getCurrentTime >>= return . toGregorian . utctDay
    let
            ymdToYears = colTypes !! 0 == ColYear && colTypes !! 1 == ColMonth && colTypes !! 2 == ColDay
            dataLines =
                map (\line -> 
                        let
                            lineVals = map (\(i, val) -> readValue (colTypes !! i) val year) (zip [0, 1 ..] line)
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
            is1d = numCols == 1 || numCols == 2 && ColError `elem` colTypes 
            is2d = numCols == 2 || numCols == 3 && ColError `elem` colTypes 
            dataCreateFunc = if dataOrSpectrum then D.data1 else D.spectrum1
            dat = 
                if is1d then 
                        dataCreateFunc $ V.fromList $ map (\line -> 
                            if length line == 1 
                                then (head line, 1, 1) 
                                else (head line, 1, last line)) dataLines
                else if is2d 
                    then
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
    modifyMVar_ stateRef $ \state -> return $ addDiscreteData dat (if name == "" then fileName else name) (Just (currentGraphTab, selectedGraph)) state

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
                        d = case subData (head (dataSet (fromJust selectedData))) of
                            Left dataOrSpec -> dataOrSpec
                            Right (Left s) -> sampleAnalyticData_ s [round precision] g
                            Right (Right f) -> sampleAnalyticData_ f [round precision] g
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

