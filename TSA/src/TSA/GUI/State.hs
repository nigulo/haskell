module TSA.GUI.State  (
    State (..), 
    GuiParams (..),
    GraphParams (..),
    GraphTabParams (..),
    GraphDataParams (..),
    GnuParams (..),
    GraphSelection (..),
    SettingsParams (..),
    StateRef,
    Task (..),
    newState,
    progressUpdate,
    readState,
    getWindow,
    getCurrentGraphTab,
    getGraphTabs,
    getProgressBar,
    setProgressBarPercent,
    getStatusBar,
    setStatusBarText,
    newGraphTab,
    newGraph,
    getNormalizedScreenArea,
    insertGraphRowsCols,
    appendLog,
    refreshLog
    ) where

import TSA.Params
import Graphics.UI.Gtk hiding (addWidget, Plus, Cross, Circle)
import Control.Concurrent.MVar
import Data.List
import Data.Word
import Data.Array
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace
import Regression.Data as D
import Regression.Spline as S
import Regression.Functions as F
import Regression.AnalyticData as A
import Regression.Statistic
import Utils.Misc
import Utils.List
import System.Random

import GUI.Plot

import Prelude hiding (log)
import Control.Concurrent
import qualified Utils.Xml as Xml


import Utils.Str
import Utils.Misc

data GraphDataParams = GraphDataParams {
    graphDataParamsName :: String, -- references DataParams.dataName
    graphDataParamsColor :: (Word16, Word16, Word16),
    graphDataParamsPointType :: PlotPointType,
    graphDataParamsPointSize :: Double,
    graphDataParamsLineDash :: [Double],
    graphDataParamsLineWidth :: Double,
    graphDataParamsErrorBars :: Bool
} deriving (Show, Read)

convertLegacyPointType :: Int -> PlotPointType
convertLegacyPointType 0 = Square
convertLegacyPointType 1 = Cross
convertLegacyPointType 2 = Plus
convertLegacyPointType 3 = Diamond
convertLegacyPointType 4 = Circle
convertLegacyPointType 5 = Plus -- Not supported anymore
convertLegacyPointType 6 = Plus -- Not supported anymore
convertLegacyPointType 7 = Impulse

instance Xml.XmlElement GraphDataParams where
    toElement params = Xml.element "graphdataparams"
        [("dataname", graphDataParamsName params), 
         ("color", show (graphDataParamsColor params)), 
         ("pointtype", show (graphDataParamsPointType params)),
         ("pointsize", show (graphDataParamsPointSize params)),
         ("linedash", show (graphDataParamsLineDash params)),
         ("linewidth", show (graphDataParamsLineWidth params)),
         ("errorbars", show (graphDataParamsErrorBars params))
        ]
        []
        
    fromElement e =
        GraphDataParams {
            graphDataParamsName = Xml.attrValue e "dataname",
            graphDataParamsColor = read $ Xml.attrValue e "color",
            graphDataParamsPointType =
                let
                    maybePointType = Xml.maybeAttrValue e "pointtype"
                in
                    case maybePointType of
                        Just pointType -> read pointType
                        -- legacy point types
                        otherwise -> convertLegacyPointType $ read $ Xml.attrValue e "symbol",
            graphDataParamsPointSize =
                let
                    maybePointSize = Xml.maybeAttrValue e "pointsize"
                    maybeSymbolSize = Xml.maybeAttrValue e "symbolsize"
                in
                    case maybePointSize of
                        Just pointSize -> read pointSize
                        otherwise -> 
                            case maybeSymbolSize of
                                Just symbolSize -> read symbolSize
                                otherwise -> 1,
            graphDataParamsLineDash =
                let
                    maybeLineDash = Xml.maybeAttrValue e "linedash"
                in
                    case maybeLineDash of
                        Just lineDash -> read lineDash
                        otherwise -> [1, 0],
            graphDataParamsLineWidth =
                let
                    maybeLineWidth = Xml.maybeAttrValue e "linewidth"
                in
                    case maybeLineWidth of
                        Just lineWidth -> read lineWidth
                        otherwise -> 1,
            graphDataParamsErrorBars =
                let
                    maybeErrorBars = Xml.maybeAttrValue e "errorbars"
                in
                    case maybeErrorBars of
                        Just errorBars -> read errorBars
                        otherwise -> False
        } 


data GraphParams = GraphParams {
    graphName :: String, -- unused
    graphGranularity :: Int,
    graphAreaAutomatic :: Bool,
    graphPeriod :: Double,
    graphOffset :: Double,
    graphArea :: PlotArea,
    graphMinorXUnit :: Either Bool Double,
    graphMinorYUnit :: Either Bool Double,
    graphMajorXUnit :: Either Bool Double,
    graphMajorYUnit :: Either Bool Double,
    graphSelection :: Maybe GraphSelection,
    graphSegments :: [Double],
    graphData :: [GraphDataParams],
    graphGnuParams :: GnuParams,
    graphWidth :: Double, -- relative width 
    graphHeight :: Double -- relative height
} deriving (Show, Read)

instance Xml.XmlElement GraphParams where
    toElement params = Xml.element "graphparams" 
        [("version", "1"),
         ("name", graphName params),
         ("granularity", show (graphGranularity params)),
         ("automatic", show (graphAreaAutomatic params)),
         ("period", show (graphPeriod params)),
         ("offset", show (graphOffset params)),
         ("xminorunit", show (graphMinorXUnit params)),
         ("yminorunit", show (graphMinorYUnit params)),
         ("xmajorunit", show (graphMajorXUnit params)),
         ("ymajorunit", show (graphMajorYUnit params)),
         ("width", show (graphWidth params)),
         ("height", show (graphHeight params)),
         ("segments", show (graphSegments params))
        ]
        ([Left (Xml.toElement (graphArea params))] ++ (
            case (graphSelection params) of
                Just selection -> [Left (Xml.toElement selection)]
                otherwise -> []
        ) ++ (
            map (\d -> Left (Xml.toElement d)) (graphData params)
        ) ++ [Left (Xml.toElement (graphGnuParams params))])

    fromElement e = 
        let
            version = maybe_ "0" $ Xml.maybeAttrValue e "version"
        in
            GraphParams {
                graphName = Xml.attrValue e "name",
                graphGranularity = case Xml.maybeAttrValue e "granularity" of
                    Just precision -> read precision
                    Nothing -> 1, 
                graphSegments = case Xml.maybeAttrValue e "segments" of
                    Just segments -> read segments
                    Nothing -> [], 
                graphAreaAutomatic = read $ Xml.attrValue e "automatic",
                graphPeriod = (if version == "0" then 0 else read $ Xml.attrValue e "period"),
                graphOffset =
                    case Xml.maybeAttrValue e "offset" of
                        Just offset -> read offset
                        otherwise -> 0,
                graphArea = Xml.fromElement $ Xml.contentElement e "grapharea",
                graphMinorXUnit = read $ Xml.attrValue e "xminorunit",
                graphMinorYUnit = read $ Xml.attrValue e "yminorunit",
                graphMajorXUnit = read $ Xml.attrValue e "xmajorunit",
                graphMajorYUnit = read $ Xml.attrValue e "ymajorunit",
                graphWidth =
                    case Xml.maybeAttrValue e "width" of
                        Just width -> read width
                        otherwise -> 1,
                graphHeight =
                    case Xml.maybeAttrValue e "height" of
                        Just height -> read height
                        otherwise -> 1,
                graphSelection =
                    (case Xml.contentElements e "graphselection" of
                        [selectionElem] -> Just $ Xml.fromElement selectionElem 
                        otherwise -> Nothing),
                graphData = map Xml.fromElement (Xml.contentElements e "graphdataparams"),
                graphGnuParams =
                    let 
                        maybeGnuParams =  Xml.maybeContentElement e "gnuparams"
                    in
                        case maybeGnuParams of
                            Just params -> Xml.fromElement params
                            otherwise -> newGnuParams
                            
            }

instance Xml.XmlElement PlotArea where

    toElement graphArea = Xml.element "grapharea" 
        [("left", show (plotAreaLeft graphArea)), ("right", show (plotAreaRight graphArea)), 
         ("top", show (plotAreaTop graphArea)), ("bottom", show (plotAreaBottom graphArea)),
         ("front", show (plotAreaFront graphArea)), ("back", show (plotAreaBack graphArea))
        ] 
        []
    
    fromElement e = 
        PlotArea {
            plotAreaLeft = read $ Xml.attrValue e "left",
            plotAreaRight = read $ Xml.attrValue e "right",
            plotAreaTop = read $ Xml.attrValue e "top",
            plotAreaBottom = read $ Xml.attrValue e "bottom",
            plotAreaFront =
                case Xml.maybeAttrValue e "front" of
                    Just front -> read front
                    otherwise -> 1,
            plotAreaBack =
                case Xml.maybeAttrValue e "back" of
                    Just back -> read back
                    otherwise -> -1
        }

data GraphTabParams = GraphTabParams {
    graphTabName :: String,
    graphTabRows :: Int,
    graphTabCols :: Int,
    graphTabSelection :: Int,
    graphTabGraphs :: [GraphParams],
    graphTabTool :: PlotTool
} deriving (Show, Read)

instance Xml.XmlElement GraphTabParams where
    toElement params = Xml.element "graphtabparams" 
        [
         ("name", graphTabName params),
         ("rows", show (graphTabRows params)),
         ("cols", show (graphTabCols params)),
         ("selection", show (graphTabSelection params)),
         ("tool", show (graphTabTool params))
        ]
        (
            map (\gp -> Left (Xml.toElement gp)) (graphTabGraphs params)
        )

    fromElement e = 
            GraphTabParams {
                graphTabName = Xml.attrValue e "name",
                graphTabRows = read $ Xml.attrValue e "rows",
                graphTabCols = read $ Xml.attrValue e "cols",
                graphTabSelection = read $ Xml.attrValue e "selection",
                graphTabTool = case Xml.maybeAttrValue e "tool" of
                    Just tool -> read tool
                    Nothing -> PlotToolSelect,
                graphTabGraphs = map Xml.fromElement (Xml.contentElements e "graphparams")
            }

data GnuParams = GnuParams {
    gnuTitle :: String,
    gnuTitleOpts :: String,
    gnuXLabel :: String,
    gnuXLabelOpts :: String,
    gnuYLabel :: String,
    gnuYLabelOpts :: String,
    gnuZLabel :: String,
    gnuZLabelOpts :: String,
    gnuCBLabel :: String,
    gnuCBLabelOpts :: String,
    gnuXTics :: String, 
    gnuYTics :: String, 
    gnuZTics :: String, 
    gnuCBTics :: String, 
    gnuBorder :: String, 
    gnuMap :: Bool,
    gnuNegativePalette :: Bool,
    gnuLogScaleX :: Bool,
    gnuLogScaleY :: Bool,
    gnuLogScaleZ :: Bool,
    gnuGrid :: Bool
} deriving (Show, Read)

defaultGnuTitleOpts = "font \"Arial,12\""
defaultGnuLabelOpts = "font \"Arial,10\""
defaultGnuTics = "font \"Arial,8\" border"

instance Xml.XmlElement GnuParams where
    toElement params = Xml.element "gnuparams"
        [("title", gnuTitle params),
         ("xlabel", gnuXLabel params),
         ("ylabel", gnuYLabel params),
         ("zlabel", gnuZLabel params),
         ("cblabel", gnuCBLabel params),
         ("border", gnuBorder params),
         ("map", show (gnuMap params)),
         ("negativepalette", show (gnuNegativePalette params)),
         ("logscalex", show (gnuLogScaleX params)),
         ("logscaley", show (gnuLogScaleY params)),
         ("logscalez", show (gnuLogScaleZ params)),
         ("grid", show (gnuGrid params))
        ]
        [Left (Xml.element "titleopts" [] [Right (gnuTitleOpts params)]),
        Left (Xml.element "xlabelopts" [] [Right (gnuXLabelOpts params)]),
        Left (Xml.element "ylabelopts" [] [Right (gnuYLabelOpts params)]),
        Left (Xml.element "zlabelopts" [] [Right (gnuZLabelOpts params)]),
        Left (Xml.element "cblabelopts" [] [Right (gnuCBLabelOpts params)]),
        Left (Xml.element "xtics" [] [Right (gnuXTics params)]),
        Left (Xml.element "ytics" [] [Right (gnuYTics params)]),
        Left (Xml.element "ztics" [] [Right (gnuZTics params)]),
        Left (Xml.element "cbtics" [] [Right (gnuCBTics params)])
        ]
        
    fromElement e =
        GnuParams {
            gnuTitle = Xml.attrValue e "title",
            gnuXLabel = Xml.attrValue e "xlabel",
            gnuYLabel = Xml.attrValue e "ylabel",
            gnuZLabel = case Xml.maybeAttrValue e "zlabel" of
                Just zLabel -> zLabel
                Nothing -> "zLabel", -- old version
            gnuCBLabel = maybe_ "cbLabel" $ Xml.maybeAttrValue e "cblabel",
            gnuLogScaleX = case Xml.maybeAttrValue e "logscalex" of
                Just logScale -> read logScale
                Nothing -> False, 
            gnuLogScaleY = case Xml.maybeAttrValue e "logscaley" of
                Just logScale -> read logScale
                Nothing -> False,
            gnuLogScaleZ = case Xml.maybeAttrValue e "logscalez" of
                Just logScale -> read logScale
                Nothing -> False,
            gnuTitleOpts = case Xml.maybeContentElement e "titleopts" of
                Just optsElem -> head $ Xml.contentTexts optsElem 
                Nothing -> defaultGnuTitleOpts,
            gnuXLabelOpts = case Xml.maybeContentElement e "xlabelopts" of
                Just optsElem -> head $ Xml.contentTexts optsElem 
                Nothing -> defaultGnuLabelOpts,
            gnuYLabelOpts = case Xml.maybeContentElement e "ylabelopts" of
                Just optsElem -> head $ Xml.contentTexts optsElem 
                Nothing -> defaultGnuLabelOpts,
            gnuZLabelOpts = case Xml.maybeContentElement e "zlabelopts" of
                Just optsElem -> head $ Xml.contentTexts optsElem 
                Nothing -> defaultGnuLabelOpts,
            gnuCBLabelOpts = case Xml.maybeContentElement e "cblabelopts" of
                Just optsElem -> head $ Xml.contentTexts optsElem 
                Nothing -> defaultGnuLabelOpts,
            gnuXTics = case Xml.maybeContentElement e "xtics" of
                Just ticsElem -> head $ Xml.contentTexts ticsElem 
                Nothing -> maybe_ defaultGnuTics (Xml.maybeAttrValue e "xtics"), -- old version
            gnuYTics = case Xml.maybeContentElement e "ytics" of
                Just ticsElem -> head $ Xml.contentTexts ticsElem 
                Nothing -> maybe_ defaultGnuTics (Xml.maybeAttrValue e "ytics"), -- old version
            gnuZTics = case Xml.maybeContentElement e "ztics" of
                Just ticsElem -> head $ Xml.contentTexts ticsElem 
                Nothing -> defaultGnuTics,
            gnuCBTics = case Xml.maybeContentElement e "cbtics" of
                Just ticsElem -> head $ Xml.contentTexts ticsElem 
                Nothing -> defaultGnuTics,
            gnuBorder = maybe_ "" (Xml.maybeAttrValue e "border"),
            gnuMap = case Xml.maybeAttrValue e "map" of
                Just map -> read map
                Nothing -> True,
            gnuNegativePalette = read $ Xml.attrValue e "negativepalette",
            gnuGrid = case Xml.maybeAttrValue e "grid" of
                Just grid -> read grid
                Nothing -> True
        } 

data GraphSelection = GraphSelection {
    graphSelectionLeft :: Double,
    graphSelectionRight :: Double,
    graphSelectionTop :: Double,
    graphSelectionBottom :: Double
} deriving (Show, Read)

instance Xml.XmlElement GraphSelection where

    toElement graphSelection = Xml.element "graphselection" 
        [("left", show (graphSelectionLeft graphSelection)), ("right", show (graphSelectionRight graphSelection)), 
         ("top", show (graphSelectionTop graphSelection)), ("bottom", show (graphSelectionBottom graphSelection))
        ] 
        []
    
    fromElement e = 
        GraphSelection {
            graphSelectionLeft = read $ Xml.attrValue e "left",
            graphSelectionRight = read $ Xml.attrValue e "right",
            graphSelectionTop = read $ Xml.attrValue e "top",
            graphSelectionBottom = read $ Xml.attrValue e "bottom"
        }

data SettingsParams = SettingsParams {
    settingsSaveChangesOnExit :: Bool,
    settingsSaveInZippedFormat :: Bool,
    settingsActiveTab :: Int
} deriving (Show, Read)

instance Xml.XmlElement SettingsParams where
    toElement params = Xml.element "settingsparams" 
        [
         ("savechanges", show (settingsSaveChangesOnExit params)),
         ("savezipped", show (settingsSaveInZippedFormat params)),
         ("activetab", show (settingsActiveTab params))
        ] []

    fromElement e = 
            SettingsParams {
                settingsSaveChangesOnExit = read $ Xml.attrValue e "savechanges",
                settingsSaveInZippedFormat = case Xml.maybeAttrValue e "savezipped" of
                    Just zip -> read zip
                    Nothing -> True,
                settingsActiveTab = read $ Xml.attrValue e "activetab"
            }


data GuiParams = GuiParams {
    guiWindow :: Window, 
    guiGraphTabs :: Notebook,
    guiProgressBar :: (ProgressBar, Double {- percent -}),
    guiStatusBar :: (Statusbar, String, ContextId, MessageId),
    guiMousePos :: Maybe (Double, Double),
    guiLog :: Maybe TextView,
    guiChanged :: Bool
}

data Task = Task ThreadId String {- task name-} [Task {- subtasks -}]

data State = State {
    params :: Params,
    guiParams :: Maybe GuiParams,
    graphTabs :: [GraphTabParams],
    log :: String,
    settingsParams :: SettingsParams,
    tasks :: [Task]
}

instance Xml.XmlElement State where
    toElement state = Xml.element "tsastate" [("version", "2")] 
        ([Left (Xml.toElement (params state)),
         Left (Xml.toElement (settingsParams state)),
         Left (Xml.element "log" [] [Right (log state)])
        ] ++ (map (\(i, gp) -> Left (Xml.element "graphtabs" [("index", show i)] [Left (Xml.toElement gp)])) (zip [0 .. length (graphTabs state) - 1] (graphTabs state))))

    fromElement e = 
        let
            version = maybe_ "0" $ Xml.maybeAttrValue e "version"
        in
            case version of 
                "1" ->
                    State {
                        params = Xml.fromElement e,
                        guiParams = Nothing,
                        graphTabs = 
                            let 
                                graphTabElements = Xml.contentElements e "graphtabs"
                            in
                                if length graphTabElements > 0 
                                    then
                                        let
                                            gts :: Map.Map Int GraphTabParams = case Xml.maybeContentElement (head graphTabElements) "graphtabparams" of
                                                Just e ->
                                                    Map.fromList $ map 
                                                        (\gpElem -> (read (Xml.attrValue gpElem "index"), 
                                                            Xml.fromElement (Xml.contentElement gpElem "graphtabparams"))) graphTabElements
                                                otherwise ->
                                                    Map.fromList $ map 
                                                        (\gpElem -> (read (Xml.attrValue gpElem "index"), 
                                                            GraphTabParams {
                                                                graphTabName = "GraphTab", 
                                                                graphTabRows = 1, 
                                                                graphTabCols = 1, 
                                                                graphTabSelection = 0, 
                                                                graphTabGraphs = [Xml.fromElement (Xml.contentElement gpElem "graphparams")],
                                                                graphTabTool = PlotToolSelect
                                                                }
                                                             )) graphTabElements                            
                                        in
                                            snd $ unzip $ Map.toAscList gts
                                    else
                                        [newGraphTab "Graph 1"],
                        settingsParams =
                            (case Xml.maybeContentElement e "settingsparams" of
                                Just e1 -> Xml.fromElement e1
                                Nothing -> newSettings
                                ),
                        log = 
                            (case Xml.maybeContentElement e "log" of
                                Just e1 -> head $ Xml.contentTexts e1
                                Nothing -> ""
                                ),
                        tasks = []
                }
                otherwise ->
                    State {
                        params = Xml.fromElement $ Xml.contentElement e "tsaparams",
                        guiParams = Nothing,
                        graphTabs = 
                            let 
                                graphTabElements = Xml.contentElements e "graphtabs"
                            in
                                if length graphTabElements > 0 
                                    then
                                        let
                                            gts :: Map.Map Int GraphTabParams = case Xml.maybeContentElement (head graphTabElements) "graphtabparams" of
                                                Just e ->
                                                    Map.fromList $ map 
                                                        (\gpElem -> (read (Xml.attrValue gpElem "index"), 
                                                            Xml.fromElement (Xml.contentElement gpElem "graphtabparams"))) graphTabElements
                                                otherwise ->
                                                    Map.fromList $ map 
                                                        (\gpElem -> (read (Xml.attrValue gpElem "index"), 
                                                            GraphTabParams {
                                                                graphTabName = "GraphTab", 
                                                                graphTabRows = 1, 
                                                                graphTabCols = 1, 
                                                                graphTabSelection = 0, 
                                                                graphTabGraphs = [Xml.fromElement (Xml.contentElement gpElem "graphparams")],
                                                                graphTabTool = PlotToolSelect
                                                                }
                                                             )) graphTabElements                            
                                        in
                                            snd $ unzip $ Map.toAscList gts
                                    else
                                        [newGraphTab "Graph 1"],
                        settingsParams =
                            (case Xml.maybeContentElement e "settingsparams" of
                                Just e1 -> Xml.fromElement e1
                                Nothing -> newSettings
                                ),
                        log = 
                            (case Xml.maybeContentElement e "log" of
                                Just e1 -> head $ Xml.contentTexts e1
                                Nothing -> ""
                                ),
                        tasks = []
                }

instance Show State where
    show s =
        "1\n" ++ --version 
        (show (params s)) ++ "\n" ++
        (show (graphTabs s)) ++ "\n" ++
        (show (log s)) ++ "\n"
        
readState :: StateRef -> String -> IO ()
readState stateRef s =
    do
        oldState <- readMVar stateRef
        let 
            state = (Xml.fromDocument (Xml.parse "State" s)) {guiParams = guiParams oldState}
        modifyMVar_ stateRef $ \_ -> return state
        

type StateRef = MVar State

getWindow :: State -> Window
getWindow state = guiWindow $ fromJust $ guiParams state

getCurrentGraphTab :: State -> IO (Int, DrawingArea)
getCurrentGraphTab state = 
    do
        let 
            graphTabs = guiGraphTabs $ fromJust $ guiParams state
        i <- notebookGetCurrentPage graphTabs
        Just page <- notebookGetNthPage graphTabs i
        return (i, castToDrawingArea page)

getGraphTabs :: State -> Notebook
getGraphTabs state = 
    let
        Just guiParms = guiParams state 
    in
        guiGraphTabs guiParms 

getProgressBar :: State -> (ProgressBar, Double)
getProgressBar state = guiProgressBar $ fromJust $ guiParams state

setProgressBarPercent :: Double -> State -> State
setProgressBarPercent percent state =
    let
        guiParms = fromJust $ guiParams state
        (progressBar, _) = guiProgressBar guiParms
    in
        state {guiParams = Just (guiParms {guiProgressBar = (progressBar, percent)})}

getStatusBar :: State -> (Statusbar, String, ContextId, MessageId)
getStatusBar state = guiStatusBar $ fromJust $ guiParams state

setStatusBarText :: String -> State -> State
setStatusBarText text state =
    let
        guiParms = fromJust $ guiParams state
        (statusBar, _, contextId, messageId) = guiStatusBar guiParms
    in
        state {guiParams = Just (guiParms {guiStatusBar = (statusBar, text, contextId, messageId)})}




getGraph :: Int -> Int -> Int -> State -> GraphParams 
getGraph tabIndex row col state =
    let
        graphTab = graphTabs state !! tabIndex
        rows = graphTabRows graphTab 
        cols = graphTabCols graphTab
        graphs = listArray ((0, 0), (rows - 1, cols - 1)) (graphTabGraphs graphTab) 
    in 
        graphs ! (row, col)

updateGraph :: Int -> Int -> Int -> GraphParams -> State -> State
updateGraph tabIndex row col graph state =
    let
        graphTab = graphTabs state !! tabIndex
        rows = graphTabRows graphTab 
        cols = graphTabCols graphTab
        graphs = elems $ (listArray ((0, 0), (rows - 1, cols - 1)) (graphTabGraphs graphTab)) // [((row, col), graph)] 
    in
        state {
            graphTabs = updateAt tabIndex (
                graphTab {
                    graphTabGraphs = graphs
                }
            ) $ graphTabs state
        }

insertGraphRowsCols :: Int -> (Int, Int) -> Int -> Int -> State -> State
insertGraphRowsCols tabIndex (row, col) rowsToAdd colsToAdd state = 
    let
        graphTab = graphTabs state !! tabIndex
        rows = graphTabRows graphTab 
        cols = graphTabCols graphTab
        updateGraphSize graph =
            let
                heightChange = fromIntegral rows / fromIntegral (rows + rowsToAdd)
                widthChange = fromIntegral cols / fromIntegral (cols + colsToAdd)
            in
                graph {
                    graphHeight = heightChange * (graphHeight graph),
                    graphWidth = widthChange * (graphWidth graph)
                }
        updateNewGraphSize graph =
            graph {
                graphHeight = 1 / fromIntegral (rows + rowsToAdd),
                graphWidth = 1 / fromIntegral (cols + colsToAdd)
            }
        graphs =
            elems $
            array ((0, 0), (rows - 1 + rowsToAdd, cols - 1 + colsToAdd)) $
            [((r, c), updateNewGraphSize (newGraph "New Graph")) | c <- [0 .. cols + colsToAdd  - 1], r <- [row .. row + rowsToAdd - 1]] ++
            [((r, c), updateNewGraphSize (newGraph "New Graph")) | c <- [col .. col + colsToAdd - 1], r <- [0 .. rows + rowsToAdd - 1]] ++ (
            map (\((r, c), g) ->
                let
                    r1 = if r >= row then r + rowsToAdd else r
                    c1 = if c >= col then c + colsToAdd else c
                in 
                    ((r1, c1), updateGraphSize g)
            ) $ assocs $ listArray ((0, 0), (rows - 1, cols - 1)) (graphTabGraphs graphTab))
    in
        state {
            graphTabs = updateAt tabIndex (
                graphTab {
                    graphTabRows = rows + rowsToAdd,
                    graphTabCols = cols + colsToAdd,
                    graphTabGraphs = graphs
                }
            ) $ graphTabs state
        }


newState :: Params -> State
newState p =
    State {
            params = p,
            guiParams = Nothing,
            graphTabs = [],
            settingsParams = newSettings,
            log = "",
            tasks = []
            
    }

newGraphTab :: String -> GraphTabParams
newGraphTab name =
    GraphTabParams {
        graphTabName = name,
        graphTabRows = 1,
        graphTabCols = 1,
        graphTabSelection = 0,
        graphTabTool = PlotToolSelect,
        graphTabGraphs = [newGraph name]
    }

newGraph :: String -> GraphParams
newGraph name =
    GraphParams {
        graphName = name,
        graphGranularity = 1,
        graphAreaAutomatic = True,
        graphPeriod = 0,
        graphOffset = 0,
        graphArea = PlotArea {
           plotAreaLeft = -1,
           plotAreaRight = 1,
           plotAreaBottom = -1,
           plotAreaTop = 1,
           plotAreaFront = 1,
           plotAreaBack = -1},
       graphSelection = Nothing,
       graphSegments = [],
       graphMinorXUnit = Left True,
       graphMinorYUnit = Left True,
       graphMajorXUnit = Left True,
       graphMajorYUnit = Left True,
       graphData = [],
       graphGnuParams = newGnuParams,
       graphWidth = 1,
       graphHeight = 1
   }

newGnuParams :: GnuParams
newGnuParams =
    GnuParams {
       gnuTitle = "Title",
       gnuTitleOpts = defaultGnuTitleOpts,
       gnuXLabel = "xLabel",
       gnuXLabelOpts = defaultGnuLabelOpts,
       gnuYLabel = "yLabel",
       gnuYLabelOpts = defaultGnuLabelOpts,
       gnuZLabel = "zLabel",
       gnuZLabelOpts = defaultGnuLabelOpts,
       gnuCBLabel = "cbLabel",
       gnuCBLabelOpts = defaultGnuLabelOpts,
       gnuXTics = defaultGnuTics,
       gnuYTics = defaultGnuTics,
       gnuZTics = defaultGnuTics,
       gnuCBTics = defaultGnuTics,
       gnuBorder = "4095",
       gnuMap = True,
       gnuNegativePalette = True,
       gnuLogScaleX = False,
       gnuLogScaleY = False,
       gnuLogScaleZ = False,
       gnuGrid = True
   }

newSettings :: SettingsParams
newSettings = 
    SettingsParams {
        settingsSaveChangesOnExit = False,
        settingsSaveInZippedFormat = True,
        settingsActiveTab = 0
    }


progressUpdate :: StateRef -> Double -> IO ()
progressUpdate stateRef percent = 
    modifyMVar_ stateRef $ \state -> return $ setProgressBarPercent percent state
--    do
--        state <- readMVar stateRef
--        let (pb, _) = getProgressBar state
--        postGUIAsync $ pb `progressBarSetFraction` percent
--        --threadDelay (100 * 1000)

-- | Returns normalized screen area for given graph, where
--   top left = (0,0) and bottom right = (1, 1)
getNormalizedScreenArea :: GraphTabParams -> Int -> (Double, Double, Double, Double)
getNormalizedScreenArea graphTab selectedGraph =
    let
        rows = graphTabRows graphTab 
        cols = graphTabCols graphTab 
        row = selectedGraph `quot` cols 
        col = selectedGraph `rem` cols 
        graphArray = assocs $ listArray ((0, 0), (rows - 1, cols - 1)) (graphTabGraphs graphTab)
        
        left = sum $ map (\((_, _), params) -> graphWidth params) $ filter (\((r, c), _) -> r == row && c < col) graphArray
        top = sum $ map (\((_, _), params) -> graphHeight params) $ filter (\((r, c), _) -> r < row && c == col) graphArray
        right = left + graphWidth (graphTabGraphs graphTab!! selectedGraph)
        bottom = top + graphHeight (graphTabGraphs graphTab!! selectedGraph)
    in
        (left, top, right, bottom)

appendLog :: StateRef -> String -> IO ()
appendLog stateRef text = do
    state <- readMVar stateRef
    let 
        newText = TSA.GUI.State.log state ++ text ++ "\n"
    modifyMVar_ stateRef $ \state -> return $ state {
        TSA.GUI.State.log = newText
        }
    refreshLog stateRef

refreshLog :: StateRef -> IO ()
refreshLog stateRef = postGUIAsync $ do
    state <- readMVar stateRef
    case guiLog (fromJust (guiParams state)) of
        Just textView -> do
            textBuffer <- textViewGetBuffer textView
            textBufferSetText textBuffer (TSA.GUI.State.log state)
            textMark <- textMarkNew Nothing True 
            textIter <- textBufferGetEndIter textBuffer
            textBufferAddMark textBuffer textMark textIter
            textViewScrollToMark textView textMark 0 Nothing 
        otherwise -> return ()
