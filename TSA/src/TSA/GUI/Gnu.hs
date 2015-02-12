module TSA.GUI.Gnu (plotDialog, previewDialog, paramsDialog, gnuPointTypes, gnuPointTypeNames) where

import Graphics.UI.Gtk hiding (addWidget, Plus, Cross, Circle)
import Data.IORef
import Data.Word
import Data.Bits
import qualified Data.Map as Map
import Control.Concurrent
import Debug.Trace

import Regression.Data as D
import Regression.Spline as S
import qualified Regression.AnalyticData as AD
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
import Graphics.Gnuplot.ColorSpecification (rgb8)
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
            stringToFile "preview.gp" script
            mapM_ (\dat -> do
                    stringToFile (File.name dat) (File.content dat)
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

    titleEntry <- entryNew
    addWidget (Just "Title: ") titleEntry dialog
    titleEntry `entrySetText` (gnuTitle gnuParms)

    xLabelEntry <- entryNew
    addWidget (Just "X Label: ") xLabelEntry dialog
    xLabelEntry `entrySetText` (gnuXLabel gnuParms)

    yLabelEntry <- entryNew
    addWidget (Just "Y Label: ") yLabelEntry dialog
    yLabelEntry `entrySetText` (gnuYLabel gnuParms)

    cbLabelEntry <- entryNew
    addWidget (Just "CB Label: ") cbLabelEntry dialog
    cbLabelEntry `entrySetText` (gnuCBLabel gnuParms)

    fontEntry <- entryNew
    addWidget (Just "Font: ") fontEntry dialog
    fontEntry `entrySetText` (gnuFont gnuParms)
    
    fontSizeAdjustment <- adjustmentNew (fromIntegral (gnuFontSize gnuParms)) 1 100 1 1 1
    fontSizeSpin <- spinButtonNew fontSizeAdjustment 1 0
    addWidget (Just "Font size: ") fontSizeSpin dialog

    xTicksEntry <- entryNew
    addWidget (Just "xTicks: ") xTicksEntry dialog
    xTicksEntry `entrySetText` (gnuXTicks gnuParms)
    
    yTicksEntry <- entryNew
    addWidget (Just "yTicks: ") yTicksEntry dialog
    yTicksEntry `entrySetText` (gnuYTicks gnuParms)

    borderEntry <- entryNew
    addWidget (Just "Border: ") borderEntry dialog
    borderEntry `entrySetText` (gnuBorder gnuParms)

    paletteNegativeCheck <- checkButtonNew
    addWidget (Just "Negative color: ") paletteNegativeCheck dialog
    toggleButtonSetActive paletteNegativeCheck (gnuNegativePalette gnuParms)

    widgetShowAll dialog
    response <- dialogRun dialog

    title <- entryGetString titleEntry
    xLabel <- entryGetString xLabelEntry
    yLabel <- entryGetString yLabelEntry
    cbLabel <- entryGetString cbLabelEntry
    font <- entryGetString fontEntry
    xTicks <- entryGetString xTicksEntry
    yTicks <- entryGetString yTicksEntry
    border <- entryGetString borderEntry
    fontSize <- spinButtonGetValue fontSizeSpin
    negativePalette <- toggleButtonGetActive paletteNegativeCheck
     
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
                                            gnuXLabel = xLabel,
                                            gnuYLabel = yLabel,
                                            gnuCBLabel = cbLabel,
                                            gnuFont = font,
                                            gnuXTicks = xTicks,
                                            gnuYTicks = yTicks,
                                            gnuBorder = border,
                                            gnuFontSize = round fontSize,
                                            gnuNegativePalette = negativePalette
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
                    xLabel = gnuXLabel gnuParms
                    yLabel = gnuYLabel gnuParms
                    cbLabel = gnuCBLabel gnuParms
                    font = gnuFont gnuParms
                    fontSize = gnuFontSize gnuParms
                    xTicks = gnuXTicks gnuParms
                    yTicks = gnuYTicks gnuParms
                    border = gnuBorder gnuParms
                    negativePalette = gnuNegativePalette gnuParms
                    offset = graphOffset grphParams


                    getSettings gdp =
                        let
                            (r, g, b) = graphDataParamsColor gdp
                            color = rgb8 (fromIntegral (shiftR r 8)) (fromIntegral (shiftR g 8)) (fromIntegral (shiftR b 8))
                            pointType = graphDataParamsPointType gdp 
                            pointSize = graphDataParamsPointSize gdp
                            dash1:dash2:_ = graphDataParamsLineDash gdp
                            lineWidth = graphDataParamsLineWidth gdp
                            errorBars = graphDataParamsErrorBars gdp
                        in
                            (color, pointType, pointSize, (round dash1, round dash2), lineWidth, errorBars)

                    dataSets2d = filter (\(dataSet, _) -> 
                        case dataSet  of
                            Left d -> is2d d
                            Right (Left s) -> True
                            Right (Right f) -> AD.is2d f
                        ) $ concat $ map (\gdp -> map (\sdp -> (subData sdp, gdp)) (dataSet (getDataByName (graphDataParamsName gdp) state))) graphDataParms
        
                    adMap ad (color, _, _, (dash1, dash2), lineWidth, _) =
                        let 
                            xMin = plotAreaLeft grphArea
                            xMax = plotAreaRight grphArea
                            xs = [xMin, xMin + (xMax - xMin) / 1000 .. xMax]
                            xsWithOffset = [xMin + offset, xMin + offset + (xMax - xMin) / 1000 .. xMax + offset]
                        in
                            (fmap (Graph2D.lineSpec (((LineSpec.lineWidth lineWidth) . (LineSpec.lineColor color) . (LineSpec.lineType dash1)) LineSpec.deflt)) 
                                (Plot2D.list (Graph2D.lines) (zip xsWithOffset (AD.getValues (map (\x ->  [x]) xs) g ad))))
                    dat2d = map (\(dataSet, gdp) ->
                        let
                            -- note that we are using dash1 as line type  
                            settings@(color, pointType, pointSize, (dash1, dash2), lineWidth, errorBars) = getSettings gdp 
                        in
                            case dataSet of
                                Left d ->
                                    if isData d
                                        then
                                            case pointType of
                                                Impulse -> (Plot2D.list Graph2D.impulses (V.toList (V.map (\(x, y) -> (x + offset, y)) (D.xys1 d))))
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
                                                                    (fmap (Graph2D.lineSpec (((LineSpec.pointType pt) . (LineSpec.pointSize pointSize) . (LineSpec.lineColor color)) LineSpec.deflt)) 
                                                                        (Plot2D.list (Graph2D.yErrorBarsRelative) (V.toList (V.map (\(x, y, w) -> ((x + offset, y), if w > 0 then sqrt (1 / w) else 0)) (D.values1 d)))))
                                                                else
                                                                    (fmap (Graph2D.lineSpec (((LineSpec.pointType pt) . (LineSpec.pointSize pointSize) . (LineSpec.lineColor color)) LineSpec.deflt)) 
                                                                        (Plot2D.list (Graph2D.points) valuesWithOffset))
                                                            else
                                                                case dash1 of 
                                                                    0 -> (fmap (Graph2D.lineSpec (((LineSpec.pointType pt) . (LineSpec.pointSize pointSize) . (LineSpec.lineWidth lineWidth) . 
                                                                        (LineSpec.lineColor color) . (LineSpec.lineType 2)) LineSpec.deflt)) (Plot2D.list (Graph2D.linesPoints) valuesWithOffset))
                                                                    otherwise -> (fmap (Graph2D.lineSpec (((LineSpec.pointType pt) . (LineSpec.pointSize pointSize) . (LineSpec.lineWidth lineWidth) . 
                                                                        (LineSpec.lineColor color) . (LineSpec.lineType dash1)) LineSpec.deflt)) (Plot2D.list (Graph2D.linesPoints) valuesWithOffset))
                                        else
                                            (fmap (Graph2D.lineSpec (((LineSpec.lineWidth lineWidth) . (LineSpec.lineColor color) . (LineSpec.lineType dash1)) LineSpec.deflt)) 
                                                (Plot2D.list (Graph2D.lines) (V.toList (D.xys1 d))))
                                Right (Left s) -> adMap s settings
                                Right (Right f) -> adMap f settings
                            ) dataSets2d
                    dataSets3d = filter (\(_, dataSet) -> 
                        case dataSet of
                            Left d -> is3d d
                            Right (Left s) -> False
                            Right (Right f) -> AD.is3d f
                        ) (concat $ map (\name -> map (\sdp -> (name, subData sdp)) (dataSet (getDataByName name state))) $ map graphDataParamsName graphDataParms)
        
                    dat3d = map (\(name, dataSet) ->
                        let 
                            d = 
                                case dataSet of
                                    Right (Right f) -> sampleAnalyticData f [plotAreaLeft grphArea, plotAreaBottom grphArea] [plotAreaRight grphArea, plotAreaTop grphArea] [200, 100] g
                                    Left dat -> dat
                            group [] = [[]]
                            group xys =
                                let
                                    (x1', _, _) = head xys
                                    (xys1, xys2) = break (\(x1, _, _) -> x1 > x1') xys
                                in
                                    [xys1] ++ (group xys2)
                            valuesWithOffset = V.toList (V.map (\(x1, x2, y) -> (x1 + offset, x2, y)) (D.xys2 d))
                        in
                            (Plot3D.mesh (group (List.sort valuesWithOffset)))
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
                            (if length title > 0 then Opts.add Opt.title ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ title ++ "\""] else Opts.remove Opt.title) $
                            (if length xLabel > 0 then Opts.add (Opt.xLabel "") ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ xLabel ++ "\""] else Opts.remove (Opt.xLabel "")) $
                            (if length yLabel > 0 then Opts.add (Opt.yLabel "") ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ yLabel ++ "\""] else Opts.remove (Opt.yLabel "")) $
                            (if length xTicks > 0 then Opts.add (Opt.xTicks "") [xTicks] else Opts.remove (Opt.xTicks "")) $
                            (if length yTicks > 0 then Opts.add (Opt.yTicks "") [yTicks] else Opts.remove (Opt.yTicks "")) $
                            (if length border > 0 then Opts.add (Opt.border "") [border] else Opts.remove (Opt.border "")) $
                            Opts.add (Opt.custom "origin" "") [show left ++ ", " ++ show (1 - bottom)] $ 
                            Opts.size width height $ 
                            Opts.remove (Opt.key "") $ 
                            opts Opts.deflt) (mconcat dat2d)


                    part3d = 
                        MultiPlot.partFromFrame $
                        Frame.cons (
                            Opts.add (Opt.custom "view" "") ["map"] $
                            Opts.remove (Opt.custom "surface" "") $ 
                            Opts.remove (Opt.custom "contour" "") $ 
                            --Opts.add (Opt.custom "cntrparam" "") ["levels", "30"] $
                            Opts.remove (Opt.custom "dgrid3d" "") $ 
                            Opts.add (Opt.pm3d "") [] $
                            Opts.add (Opt.xTicks "") ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\""] $
                            Opts.add (Opt.yTicks "") ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\""] $
                            Opts.add (Opt.custom "cbtics" "") ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\""] $
                            Opts.add Opt.title ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ title ++ "\""] $
                            Opts.add (Opt.xLabel "") ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ xLabel ++ "\""] $
                            Opts.add (Opt.yLabel "") ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ yLabel ++ "\""] $
                            Opts.add (Opt.custom "cblabel" "") ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ cbLabel ++ "\""] $
                            Opts.add (Opt.custom "palette" "") (if negativePalette then ["negative"] else ["positive"]) $
                            Opts.add (Opt.custom "origin" "") [show left ++ ", " ++ show (1 - bottom)] $ 
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

{-        
preview :: (DialogClass d) => State -> d -> GraphParams -> String -> String -> String -> String -> String -> Int -> Bool -> IO ()
preview state dialog graphParams title xLabel yLabel cbLabel font fontSize negativePalette =
    do
        widgetDestroy dialog
        g <- getStdGen 
        let
            graphDataParms = graphData graphParams
            dataSets2d = filter (\dataSet -> 
                case dataSet of
                    Left d -> is2d d
                    Right s -> True
                ) $ map (\name -> dataSet (getDataByName name state)) $ map graphDataParamsName graphDataParms
            
            adMap ad =
                let 
                    xMin = AD.xMin ad
                    xMax = AD.xMax ad
                    xs = [xMin, xMin + (xMax - xMin) / 1000 .. xMax]
                in
                   (PlotStyle {plotType = Lines, lineSpec = DefaultStyle 1}, zip xs (AD.getValues xs g ad))
            
            dat2d = map (\dataSet ->
                case dataSet of
                    Left (d@(Data _)) -> (PlotStyle {plotType = Points, lineSpec = DefaultStyle 1}, D.xys1 d)
                    Left (s@(Spectrum _)) -> (PlotStyle {plotType = Lines, lineSpec = DefaultStyle 1}, D.xys1 s)
                    Right (Left s) -> adMap s
                    Right (Right f) -> adMap f
                ) dataSets2d
            dataSets3d = filter (\(_, dataSet) -> 
                case dataSet of
                    Left d -> is3d d
                    Right s -> False
                ) $ map (\name -> (name, dataSet (getDataByName name state))) $ map graphDataParamsName graphDataParms
            
            dat3d = map (\(name, dataSet) ->
                case dataSet of
                    Left d ->
                        let 
                            group [] = [[]]
                            group xys =
                                let
                                    (xys1, xys2) = break (\([x1, _], _) -> x1 > head (fst (head xys))) xys
                                in
                                    [map (\([x1, x2], y) -> (x1, x2, y)) xys1] ++ (group xys2)
                        in
                            group (sort (D.xys d))
                ) dataSets3d
                
                
        mapM_
            (\abc -> plotMesh3d ([
                    Custom "terminal" ["wxt", "persist"],
                    Custom "mouse" [],
                    Custom "title" ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ title ++ "\""],
                    Custom "xlabel" ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ xLabel ++ "\""],
                    Custom "ylabel" ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ yLabel ++ "\""],
                    Custom "cblabel" ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ cbLabel ++ "\""],
                    Custom "cntrparam" ["levels", "30"],
                    Custom "autoscale" ["xfixmin"],
                    Custom "autoscale" ["xfixmax"],
                    Custom "nokey" [],
                    XTicks (Just ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\""]),
                    YTicks (Just ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\""]),
                    Custom "cbtics" ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\""]
                ] 
                 ++ (
                    if negativePalette
                        then [Custom "palette" ["negative"]]
                        else []
                    ))
            [Plot3dType ColorMap] abc) dat3d

        plotPathsStyle [
                Custom "terminal wxt persist" [],
                Custom "mouse" [],
                Custom "xlabel" ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ xLabel ++ "\""],
                Custom "ylabel" ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\"", "\"" ++ yLabel ++ "\""],
                Custom "autoscale" ["xfixmin"],
                Custom "autoscale" ["xfixmax"],
                Custom "nokey" [],
                XTicks (Just ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\""]),
                YTicks (Just ["font \"" ++ font ++ "," ++ (show fontSize) ++ "\""])
            ] dat2d
-}            
