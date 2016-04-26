module TSA.GUI.Gnu (plotDialog, previewDialog, paramsDialog, gnuPointTypes, gnuPointTypeNames) where

import Graphics.UI.Gtk hiding (addWidget, Plus, Cross, Circle)
import Data.IORef
import Data.Word (Word8)
import Data.List.HT (padLeft)
import Numeric (showHex)
import Data.Bits
import qualified Data.Map as Map
import Control.Concurrent
import Debug.Trace

import Regression.Data as D
import Regression.Spline as S
import qualified Regression.AnalyticDataWrapper as ADW
import Regression.Utils as U

import TSA.Params
import TSA.GUI.State
import TSA.GUI.Dialog
import TSA.GUI.Data

import GUI.Plot
import GUI.Widget

import Utils.Misc
import Utils.List
import Utils.IO
import Data.Monoid
import Data.Char
import Data.List as List
import Control.Concurrent.MVar
import System.IO
import System.Random

import Graphics.Gnuplot.Simple

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.WXT as WXT
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.PostScript as EPS
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

import qualified Graphics.Gnuplot.MultiPlot as MultiPlot

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale)
import Graphics.Gnuplot.ColorSpecification (rgb8, name)
import qualified Graphics.Gnuplot.File as File

import Data.Array (listArray)
import qualified Data.Vector.Unboxed as V

gnuPointTypes = [Point, Plus, Cross, PlusCross, Square, FilledSquare, Circle, FilledCircle, 
    Triangle, FilledTriangle, DownTriangle, FilledDownTriangle, Diamond, FilledDiamond,
    Pentagon, FilledPentagon, Impulse]
gnuPointTypeNames = ["point", "plus", "cross", "plus and cross", "square", "filled square", "circle", "filled circle", 
    "triangle", "filled triangle", "triangle (upside down)", "filled triangle (upside down)", "diamond", "filled diamond",
    "pentagon", "filled pentagon", "impulses"]

plotDialog :: StateRef -> IO ()
plotDialog stateRef = do
    state <- readMVar stateRef
    dialog <- fileChooserDialogNew (Just "Export to GNU") (Just (getWindow state)) FileChooserActionSave [("Cancel", ResponseCancel), ("Save", ResponseAccept)]
    
    fileFilter <- fileFilterNew
    fileFilter `fileFilterAddPattern` "*.EPS"
    fileFilter `fileFilterAddPattern` "*.PNG"

    (castToFileChooser dialog) `fileChooserAddFilter` fileFilter
    (castToFileChooser dialog) `fileChooserSetFilter` fileFilter
    
    widgetShowAll dialog
    response <- dialogRun dialog
    if response == ResponseAccept || response == ResponseOk 
        then
            do
                file <- fileChooserGetFilename (castToFileChooser dialog)
                widgetDestroy dialog
                g <- getStdGen 
                if (file /= Nothing)
                    then
                        do
                            (currentTabIndex, _) <- getCurrentGraphTab state
                            let
                                Just f = file
                                grphTabParams = (graphTabs state) !! currentTabIndex
                                plotFunc = doPlot state grphTabParams
                            if ".png" `isSuffixOf` (map toLower f) 
                                then
                                    let 
                                        term = PNG.trueColor $ PNG.cons ((take (length f - 4) f) ++ ".png")
                                    in
                                        plotFunc (\d -> do Plot.plot term d; return ())
                                else
                                    let
                                        eps fileName = EPS.color $ EPS.cons $ fileName ++ ".eps"
                                        term = 
                                            if ".eps" `isSuffixOf` (map toLower f)
                                                then eps $ take (length f - 4) f
                                                else eps f                    
                                    in                                        
                                        plotFunc (\d -> do Plot.plot term d; return ())
                    else
                        return ()
                return ()
        else 
            widgetDestroy dialog


previewDialog :: StateRef -> IO ()
previewDialog stateRef = do
    state <- readMVar stateRef
    (currentTabIndex, _) <- getCurrentGraphTab state
    let
        wxt = WXT.persist WXT.cons
        grphTabParams = (graphTabs state) !! currentTabIndex
    forkIO $ doPlot state grphTabParams (\d -> do Plot.plot wxt d; return ())
    forkIO $ doPlot state grphTabParams (\d -> do
            let (script, dataSets) = Plot.fileContents "." wxt d
            writeToFile "preview.gp" script
            mapM_ (\dat -> do
                    writeToFile (File.name dat) (File.content dat)
                ) dataSets
            return ()
        )
    return ()


paramsDialog :: StateRef -> IO ()
paramsDialog stateRef = do 
    state <- readMVar stateRef
    (currentTabIndex, _) <- getCurrentGraphTab state

    dialog <- dialogWithTitle state "Gnuplot settings"
    dialogAddButton dialog "Cancel" ResponseCancel
    dialogAddButton dialog "Ok" ResponseOk
    let
        graphTabParms = (graphTabs state) !! currentTabIndex
        selectedGraph = graphTabSelection graphTabParms
        graphParms = (graphTabGraphs graphTabParms) !! selectedGraph
        gnuParms = graphGnuParams graphParms 


    hBox1 <- hBoxNew True 0
    titleEntry <- entryNew
    addWidgetToBox (Just "Title: ") titleEntry PackNatural hBox1
    titleEntry `entrySetText` (gnuTitle gnuParms)
    titleOptsEntry <- entryNew
    addWidgetToBox Nothing titleOptsEntry PackNatural hBox1
    titleOptsEntry `entrySetText` (gnuTitleOpts gnuParms)
    addWidget Nothing hBox1 dialog

    hBox2 <- hBoxNew True 0
    xLabelEntry <- entryNew
    addWidgetToBox (Just "X-label: ") xLabelEntry PackNatural hBox2
    xLabelEntry `entrySetText` (gnuXLabel gnuParms)
    xLabelOptsEntry <- entryNew
    addWidgetToBox Nothing xLabelOptsEntry PackNatural hBox2
    xLabelOptsEntry `entrySetText` (gnuXLabelOpts gnuParms)
    addWidget Nothing hBox2 dialog

    hBox3 <- hBoxNew True 0
    yLabelEntry <- entryNew
    addWidgetToBox (Just "Y-label: ") yLabelEntry PackNatural hBox3
    yLabelEntry `entrySetText` (gnuYLabel gnuParms)
    yLabelOptsEntry <- entryNew
    addWidgetToBox Nothing yLabelOptsEntry PackNatural hBox3
    yLabelOptsEntry `entrySetText` (gnuYLabelOpts gnuParms)
    addWidget Nothing hBox3 dialog

    hBox4 <- hBoxNew True 0
    zLabelEntry <- entryNew
    addWidgetToBox (Just "Z-label: ") zLabelEntry PackNatural hBox4
    zLabelEntry `entrySetText` (gnuZLabel gnuParms)
    zLabelOptsEntry <- entryNew
    addWidgetToBox Nothing zLabelOptsEntry PackNatural hBox4
    zLabelOptsEntry `entrySetText` (gnuZLabelOpts gnuParms)
    addWidget Nothing hBox4 dialog

    hBox5 <- hBoxNew True 0
    cbLabelEntry <- entryNew
    addWidgetToBox (Just "CB-label: ") cbLabelEntry PackNatural hBox5
    cbLabelEntry `entrySetText` (gnuCBLabel gnuParms)
    cbLabelOptsEntry <- entryNew
    addWidgetToBox Nothing cbLabelOptsEntry PackNatural hBox5
    cbLabelOptsEntry `entrySetText` (gnuCBLabelOpts gnuParms)
    addWidget Nothing hBox5 dialog

    xTicsEntry <- entryNew
    addWidget (Just "X-tics: ") xTicsEntry dialog
    xTicsEntry `entrySetText` (gnuXTics gnuParms)
    
    yTicsEntry <- entryNew
    addWidget (Just "Y-tics: ") yTicsEntry dialog
    yTicsEntry `entrySetText` (gnuYTics gnuParms)

    zTicsEntry <- entryNew
    addWidget (Just "Z-tics: ") zTicsEntry dialog
    zTicsEntry `entrySetText` (gnuZTics gnuParms)

    cbTicsEntry <- entryNew
    addWidget (Just "CB-tics: ") cbTicsEntry dialog
    cbTicsEntry `entrySetText` (gnuCBTics gnuParms)

    -------------------------
    logScaleFrame <- frameNew
    frameSetLabel logScaleFrame (stringToGlib "Log scale" )
    addWidget Nothing logScaleFrame dialog

    logScaleBox <- hBoxNew True 0
    containerAdd logScaleFrame logScaleBox

    logScaleXCheck <- checkButtonNew
    toggleButtonSetActive logScaleXCheck (gnuLogScaleX gnuParms)
    addWidgetToBox (Just  "X:") logScaleXCheck PackNatural logScaleBox
    
    logScaleYCheck <- checkButtonNew
    toggleButtonSetActive logScaleYCheck (gnuLogScaleY gnuParms)
    addWidgetToBox (Just  "Y:") logScaleYCheck PackNatural logScaleBox

    logScaleZCheck <- checkButtonNew
    toggleButtonSetActive logScaleZCheck (gnuLogScaleZ gnuParms)
    addWidgetToBox (Just "Z:") logScaleZCheck PackNatural logScaleBox
    -------------------------

    colorAndMapBox <- hBoxNew True 0
    addWidget Nothing colorAndMapBox dialog

    paletteNegativeCheck <- checkButtonNew
    addWidgetToBox (Just "Negative color: ") paletteNegativeCheck PackNatural colorAndMapBox
    toggleButtonSetActive paletteNegativeCheck (gnuNegativePalette gnuParms)

    mapCheck <- checkButtonNew
    addWidgetToBox (Just "Map: ") mapCheck PackNatural colorAndMapBox
    toggleButtonSetActive mapCheck (gnuMap gnuParms)
    -------------------------

    gridCheck <- checkButtonNew
    addWidget (Just "Grid: ") gridCheck dialog
    toggleButtonSetActive gridCheck (gnuGrid gnuParms)

    borderEntry <- entryNew
    addWidget (Just "Border: ") borderEntry dialog
    borderEntry `entrySetText` (gnuBorder gnuParms)


    widgetShowAll dialog
    response <- dialogRun dialog

    title <- entryGetString titleEntry
    titleOpts <- entryGetString titleOptsEntry
    xLabel <- entryGetString xLabelEntry
    xLabelOpts <- entryGetString xLabelOptsEntry
    yLabel <- entryGetString yLabelEntry
    yLabelOpts <- entryGetString yLabelOptsEntry
    zLabel <- entryGetString zLabelEntry
    zLabelOpts <- entryGetString zLabelOptsEntry
    cbLabel <- entryGetString cbLabelEntry
    cbLabelOpts <- entryGetString cbLabelOptsEntry
    xTics <- entryGetString xTicsEntry
    yTics <- entryGetString yTicsEntry
    zTics <- entryGetString zTicsEntry
    cbTics <- entryGetString cbTicsEntry
    border <- entryGetString borderEntry
    negativePalette <- toggleButtonGetActive paletteNegativeCheck
    viewAsMap <- toggleButtonGetActive mapCheck
    logScaleX <- toggleButtonGetActive logScaleXCheck
    logScaleY <- toggleButtonGetActive logScaleYCheck
    logScaleZ <- toggleButtonGetActive logScaleZCheck
    grid <- toggleButtonGetActive gridCheck
     
    if response == ResponseAccept || response == ResponseOk 
        then
            do
                modifyMVar_ stateRef $ \state ->
                    return $ 
                        state {
                            graphTabs = updateAt currentTabIndex (
                                graphTabParms {
                                    graphTabGraphs = updateAt selectedGraph (graphParms {
                                        graphGnuParams = gnuParms {
                                            gnuTitle = title,
                                            gnuTitleOpts = titleOpts,
                                            gnuXLabel = xLabel,
                                            gnuXLabelOpts = xLabelOpts,
                                            gnuYLabel = yLabel,
                                            gnuYLabelOpts = yLabelOpts,
                                            gnuZLabel = zLabel,
                                            gnuZLabelOpts = zLabelOpts,
                                            gnuCBLabel = cbLabel,
                                            gnuCBLabelOpts = cbLabelOpts,
                                            gnuXTics = xTics,
                                            gnuYTics = yTics,
                                            gnuZTics = zTics,
                                            gnuCBTics = cbTics,
                                            gnuBorder = border,
                                            gnuMap = viewAsMap,
                                            gnuNegativePalette = negativePalette,
                                            gnuLogScaleX = logScaleX,
                                            gnuLogScaleY = logScaleY,
                                            gnuLogScaleZ = logScaleZ,
                                            gnuGrid = grid
                                        }
                                    }) (graphTabGraphs graphTabParms)} ) (graphTabs state)}                    
                widgetDestroy dialog
                return ()
        else 
            widgetDestroy dialog
 
doPlot :: State -> GraphTabParams -> (MultiPlot.T -> IO ()) -> IO ()
doPlot state grphTabParams plotFunc =
    do
        g <- getStdGen 
        let
            mapOp (graphIndex, grphParams) =
                let
                    gnuParms = graphGnuParams grphParams
                    grphArea = graphArea $ grphParams 
                    graphDataParms = graphData grphParams
                    width = graphWidth grphParams
                    height = graphHeight grphParams
                    (left, top, right, bottom) = getNormalizedScreenArea grphTabParams graphIndex

                    title = gnuTitle gnuParms
                    titleOpts = gnuTitleOpts gnuParms
                    xLabel = gnuXLabel gnuParms
                    xLabelOpts = gnuXLabelOpts gnuParms
                    yLabel = gnuYLabel gnuParms
                    yLabelOpts = gnuYLabelOpts gnuParms
                    zLabel = gnuZLabel gnuParms
                    zLabelOpts = gnuZLabelOpts gnuParms
                    cbLabel = gnuCBLabel gnuParms
                    cbLabelOpts = gnuCBLabelOpts gnuParms
                    xTics = gnuXTics gnuParms
                    yTics = gnuYTics gnuParms
                    zTics = gnuZTics gnuParms
                    cbTics = gnuCBTics gnuParms
                    border = gnuBorder gnuParms
                    negativePalette = gnuNegativePalette gnuParms
                    viewAsMap = gnuMap gnuParms
                    logScaleX = gnuLogScaleX gnuParms
                    logScaleY = gnuLogScaleY gnuParms
                    logScaleZ = gnuLogScaleZ gnuParms
                    grid = gnuGrid gnuParms
                    offset = graphOffset grphParams


                    getSettings gdp =
                        let
                            (r, g, b) = graphDataParamsColor gdp
                            --color = rgb8 (fromIntegral (shiftR r 8)) (fromIntegral (shiftR g 8)) (fromIntegral (shiftR b 8))
                            -- following fix is needed because gnuplot library omits quotes from around color value
                            color = Graphics.Gnuplot.ColorSpecification.name $ "#" ++
                                (concatMap (padLeft '0' 2 . flip showHex "") [(fromIntegral (shiftR r 8)), (fromIntegral (shiftR g 8)), (fromIntegral (shiftR b 8))])
                            
                            title = graphDataParamsDesc gdp 
                            pointType = graphDataParamsPointType gdp 
                            pointSize = graphDataParamsPointSize gdp
                            dash1:dash2:_ = graphDataParamsLineDash gdp
                            lineWidth = graphDataParamsLineWidth gdp
                            errorBars = graphDataParamsErrorBars gdp
                        in
                            (title, color, pointType, pointSize, (round dash1, round dash2), lineWidth, errorBars)

                    dataSets2d = filter (\(dataSet, _) -> 
                            case unboxSubData dataSet of
                                Left d -> D.is2d d
                                Right ad -> ADW.is2d ad
                        ) $ concat $ map (\gdp -> 
                                let 
                                    dp = getDataByName (graphDataParamsName gdp) state
                                in 
                                    map (\sdp -> (subData sdp, gdp)) (dataSet dp)
                            ) graphDataParms
        
                    adMap ad (title, color, _, _, (dash1, dash2), lineWidth, _) =
                        let 
                            xMin = plotAreaLeft grphArea
                            xMax = plotAreaRight grphArea
                            xs = [xMin, xMin + (xMax - xMin) / 1000 .. xMax]
                            xsWithOffset = [xMin + offset, xMin + offset + (xMax - xMin) / 1000 .. xMax + offset]
                        in
                            (fmap (Graph2D.lineSpec (((LineSpec.lineWidth lineWidth) . (LineSpec.lineColor color) . (LineSpec.lineType dash1) . (LineSpec.title title)) LineSpec.deflt)) 
                                (Plot2D.list (Graph2D.lines) (zip xsWithOffset (ADW.getValues (map (\x ->  [x]) xs) g ad))))
                    dat2d = map (\(dataSet, gdp) ->
                        let
                            -- note that we are using dash1 as line type  
                            settings@(title, color, pointType, pointSize, (dash1, dash2), lineWidth, errorBars) = getSettings gdp 
                        in
                            case unboxSubData dataSet of
                                Left d ->
                                    if isData d
                                        then
                                            case pointType of
                                                Impulse -> (fmap (Graph2D.lineSpec (((LineSpec.lineWidth lineWidth) . 
                                                    (LineSpec.lineColor color) . (LineSpec.lineType 1) . (LineSpec.title title)) LineSpec.deflt)) (Plot2D.list Graph2D.impulses (V.toList (V.map (\(x, y) -> (x + offset, y)) (D.xys1 d)))))
                                                otherwise ->
                                                    let
                                                        pt = case elemIndex pointType gnuPointTypes of 
                                                            Just i -> i
                                                            otherwise -> 1
                                                        valuesWithOffset = V.toList (V.map (\(x, y) -> (x + offset, y)) (D.xys1 d))
                                                    in 
                                                        if lineWidth == 0 
                                                            then
                                                                if errorBars then
                                                                    (fmap (Graph2D.lineSpec (((LineSpec.pointType pt) . (LineSpec.pointSize pointSize) . (LineSpec.lineColor color) . (LineSpec.title title)) LineSpec.deflt)) 
                                                                        (Plot2D.list (Graph2D.yErrorBarsRelative) (V.toList (V.map (\(x, y, w) -> ((x + offset, y), if w > 0 then sqrt (1 / w) else 0)) (D.values1 d)))))
                                                                else
                                                                    (fmap (Graph2D.lineSpec (((LineSpec.pointType pt) . (LineSpec.pointSize pointSize) . (LineSpec.lineColor color) . (LineSpec.title title)) LineSpec.deflt)) 
                                                                        (Plot2D.list (Graph2D.points) valuesWithOffset))
                                                            else
                                                                case dash1 of 
                                                                    0 -> (fmap (Graph2D.lineSpec (((LineSpec.pointType pt) . (LineSpec.pointSize pointSize) . (LineSpec.lineWidth lineWidth) . 
                                                                        (LineSpec.lineColor color) . (LineSpec.lineType 2) . (LineSpec.title title)) LineSpec.deflt)) (Plot2D.list (Graph2D.linesPoints) valuesWithOffset))
                                                                    otherwise -> (fmap (Graph2D.lineSpec (((LineSpec.pointType pt) . (LineSpec.pointSize pointSize) . (LineSpec.lineWidth lineWidth) . 
                                                                        (LineSpec.lineColor color) . (LineSpec.lineType dash1) . (LineSpec.title title)) LineSpec.deflt)) (Plot2D.list (Graph2D.linesPoints) valuesWithOffset))
                                        else
                                            (fmap (Graph2D.lineSpec (((LineSpec.lineWidth lineWidth) . (LineSpec.lineColor color) . (LineSpec.lineType dash1) . (LineSpec.title title)) LineSpec.deflt)) 
                                                (Plot2D.list (Graph2D.lines) (V.toList (D.xys1 d))))
                                Right ad -> adMap ad settings
                            ) dataSets2d
                    dataSets3d = filter (\(_, _, dataSet) -> 
                        case unboxSubData dataSet of
                            Left d -> D.is3d d
                            Right ad -> ADW.is3d ad
                        ) (concat $ map (\gdp ->
                                let 
                                    name = graphDataParamsName gdp
                                    title = graphDataParamsDesc gdp
                                    dp = getDataByName name state
                                in
                                    map (\sdp -> (name, title, subData sdp)) (dataSet dp)
                            ) graphDataParms)
        
                    dat3d = map (\(name, title, dataSet) ->
                        let 
                            d = 
                                case unboxSubData dataSet of
                                    Left d -> d
                                    Right ad -> sampleAnalyticData ad [plotAreaLeft grphArea, plotAreaBottom grphArea] [plotAreaRight grphArea, plotAreaTop grphArea] [200, 100] g
                            group [] = [[]]
                            group xys =
                                let
                                    (x1', _, _) = head xys
                                    (xys1, xys2) = break (\(x1, _, _) -> x1 > x1') xys
                                in
                                    [xys1] ++ (group xys2)
                            valuesWithOffset = V.toList (V.map (\(x1, x2, y) -> (x1 + offset, x2, y)) (D.xys2 d))
                        in
                            (fmap (Graph3D.lineSpec ((LineSpec.title title) LineSpec.deflt)) 
                                (Plot3D.mesh (group (List.sort valuesWithOffset))))
                        ) dataSets3d
                        
                    numPlots = if length dataSets2d > 0 && length dataSets3d > 0 then 2 else 1
                    
                    opts = 
                        if (graphAreaAutomatic grphParams) 
                            then
                                Opts.add (Opt.custom "autoscale" "") ["xfixmin"] .
                                Opts.add (Opt.custom "autoscale" "") ["xfixmax"]
                            else
                                Opts.add (Opt.xRange "") ["[" ++ show (plotAreaLeft grphArea + graphOffset grphParams) ++ ":" ++ show (plotAreaRight grphArea + graphOffset grphParams) ++ "]"]
                                . Opts.add (Opt.yRange "") ["[" ++ show (plotAreaBottom grphArea) ++ ":" ++ show (plotAreaTop grphArea) ++ "]"]
                                -- . Opts.add (Opt.zRange "") ["[" ++ show (plotAreaBack grphArea) ++ ":" ++ show (plotAreaFront grphArea) ++ "]"]
                    
                    part2d = 
                        MultiPlot.partFromFrame $
                        Frame.cons (
                            (if length title > 0 then Opts.add Opt.title ["\"" ++ title ++ "\" " ++ titleOpts] else Opts.remove Opt.title) $
                            (if length xLabel > 0 then Opts.add (Opt.xLabel "") ["\"" ++ xLabel ++ "\" " ++ xLabelOpts] else Opts.remove (Opt.xLabel "")) $
                            (if length yLabel > 0 then Opts.add (Opt.yLabel "") ["\"" ++ yLabel ++ "\" " ++ yLabelOpts] else Opts.remove (Opt.yLabel "")) $
                            (if length xTics > 0 then Opts.add (Opt.xTicks "") [xTics] else Opts.remove (Opt.xTicks "")) $
                            (if length yTics > 0 then Opts.add (Opt.yTicks "") [yTics] else Opts.remove (Opt.yTicks "")) $
                            (if length border > 0 then Opts.add (Opt.border "") [border] else Opts.remove (Opt.border "")) $
                            Opts.add (Opt.custom "origin" "") [show left ++ ", " ++ show (1 - bottom)] $ 
                            (if logScaleX then Opts.xLogScale else Opts.remove Opt.xLogScale) $ 
                            (if logScaleY then Opts.yLogScale else Opts.remove Opt.yLogScale) $ 
                            Opts.size width height $ 
                            Opts.grid grid $ 
                            Opts.remove (Opt.key "") $ 
                            opts Opts.deflt) (mconcat dat2d)


                    part3d = 
                        MultiPlot.partFromFrame $
                        Frame.cons (
                            (if viewAsMap then Opts.viewMap else Opts.remove Opt.view) $ 
                            (if viewAsMap then Opts.remove (Opt.custom "surface" "") else Opts.add (Opt.custom "surface" "") []) $ 
                            (if viewAsMap then Opts.remove (Opt.custom "contour" "") else Opts.add (Opt.custom "contour" "") []) $ 
                            (if viewAsMap then Opts.remove (Opt.custom "dgrid3d" "") else Opts.add (Opt.custom "dgrid3d" "") []) $ 
                            (if length title > 0 then Opts.add Opt.title ["\"" ++ title ++ "\" " ++ titleOpts] else Opts.remove (Opt.title)) $
                            (if length xLabel > 0 then Opts.add (Opt.xLabel "") ["\"" ++ xLabel ++ "\" " ++ xLabelOpts] else Opts.remove (Opt.xLabel "")) $
                            (if length yLabel > 0 then Opts.add (Opt.yLabel "") ["\"" ++ yLabel ++ "\" " ++ yLabelOpts] else Opts.remove (Opt.yLabel "")) $
                            (if length zLabel > 0 then Opts.add (Opt.zLabel "") ["\"" ++ zLabel ++ "\" " ++ zLabelOpts] else Opts.remove (Opt.zLabel "")) $
                            (if length cbLabel > 0 then Opts.add (Opt.custom "cblabel" "") ["\"" ++ cbLabel ++ "\" " ++ cbLabelOpts] else Opts.remove (Opt.custom "cblabel" "")) $
                            (if length xTics > 0 then Opts.add (Opt.xTicks "") [xTics] else Opts.remove (Opt.xTicks "")) $
                            (if length yTics > 0 then Opts.add (Opt.yTicks "") [yTics] else Opts.remove (Opt.yTicks "")) $
                            (if length zTics > 0 then Opts.add (Opt.zTicks "") [zTics] else Opts.remove (Opt.zTicks "")) $
                            (if length cbTics > 0 then Opts.add (Opt.custom "cbtics" "") [cbTics] else Opts.remove (Opt.custom "cbtics" "")) $
                            Opts.add (Opt.custom "palette" "") (if negativePalette then ["negative"] else ["positive"]) $
                            Opts.add (Opt.custom "origin" "") [show left ++ ", " ++ show (1 - bottom)] $ 
                            (if logScaleX then Opts.xLogScale else Opts.remove Opt.xLogScale) $ 
                            (if logScaleY then Opts.yLogScale else Opts.remove Opt.yLogScale) $ 
                            (if logScaleZ then Opts.zLogScale else Opts.remove Opt.zLogScale) $
                            Opts.size width height $ 
                            Opts.remove (Opt.key "") $  
                            opts Opts.deflt) (mconcat dat3d)

                in
                    if length dataSets3d > 0 then part3d else part2d
                                
            rows = graphTabRows grphTabParams
            cols = graphTabCols grphTabParams
            dataSets = map mapOp $ map (\i -> (i, graphTabGraphs grphTabParams !! i))[0 .. length (graphTabGraphs grphTabParams) - 1]

            multiPlot = MultiPlot.simpleFromPartArray $ 
                listArray ((0, 0), (rows - 1, cols - 1)) dataSets 
        
        plotFunc multiPlot

