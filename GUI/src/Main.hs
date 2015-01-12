
module Main (Main.main) where

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Gdk.EventM

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Control.Concurrent.MVar
import Control.Concurrent
import Debug.Trace
import System.IO
import Data.Complex
import qualified Data.Vector.Unboxed as V

import Control.Monad.IO.Class
import GUI.Plot as Plot
import qualified Math.Function as F

import Utils.Misc
import Utils.List

data State = State {
    window :: Window,
    canvas :: DrawingArea,
    function :: (F.Function (Complex Double)),
    density :: Int,
    scale :: Double,
    functionEntry :: Entry,
    densitySpin :: SpinButton,
    scaleSpin :: SpinButton,
    plotSettings :: PlotSettings
}

type StateRef = MVar State

main = do
    --initGUI
    unsafeInitGUIForThreadedRTS
    
    win <- windowNew
    set win [windowTitle := "Complex Function Analysis"]
    win `on` objectDestroy $ mainQuit
    windowSetDefaultSize win 320 240
    --win `on` sizeRequest $ return (Requisition 320 240)
    
    ----------------------------------------------------------------------------

    drawingArea <- drawingAreaNew
    widgetModifyBg drawingArea StateNormal (Color 65535 65535 65535)
    
    functionDialog <- vBoxNew False 0

    ----------------------------------------------------------------------------
    fnEntry <- entryNew
    fnEntry `entrySetText` "log(z)"
    addWidgetToVBox (Just "Function: ") fnEntry functionDialog

    densAdjustment <- adjustmentNew 20 5 10000 1 1 10
    densSpin <- spinButtonNew densAdjustment 1 0
    addWidgetToVBox (Just "Density: ") densSpin functionDialog

    scAdjustment <- adjustmentNew 0.1 1e-17 (2**52) 1 1 10
    scSpin <- spinButtonNew scAdjustment 1 10
    addWidgetToVBox (Just "Scale: ") scSpin functionDialog

    fn <- entryGetText fnEntry
    dens <- spinButtonGetValue densSpin
    sc <- spinButtonGetValue scSpin

    ----------------------------------------------------------------------------
    let
        state = State {
            window = win,
            canvas = drawingArea,
            function = F.toComplexFunction (F.function fn),
            density = round dens,
            scale = sc,
            functionEntry = fnEntry,
            densitySpin = densSpin,
            scaleSpin = scSpin,
            plotSettings = PlotSettings {
                plotArea = PlotArea {
                    plotAreaLeft = -1,
                    plotAreaRight = 1,
                    plotAreaBottom = -1,
                    plotAreaTop =1, 
                    plotAreaBack = 0,
                    plotAreaFront = 0 
                },
                screenArea = ScreenArea {
                    screenAreaLeft = 0,
                    screenAreaRight = 0,
                    screenAreaBottom = 0,
                    screenAreaTop = 0,
                    screenAreaBack = 0,
                    screenAreaFront = 0 
                },
                plotMinorXUnit = Left True,
                plotMinorYUnit = Left True,
                plotMajorXUnit = Left True,
                plotMajorYUnit = Left True,
                plotBackground =(1, 1, 1),
                mousePos = Nothing,
                plotSelection = Nothing,
                plotSegments = Just PlotSegments {
                    plotSegmentsData = [], 
                    plotSegmentsLineAttributes = PlotLineAttributes {
                        plotLineDash = [5, 5],
                        plotLineWidth = 1, 
                        plotLineColor = (0.5, 0.5, 0.5, 0.75)
                    }
                },
                plotTool = PlotToolSegment
            }
        }
    stateRef <- newMVar state
    ----------------------------------------------------------------------------


    vBox <- vBoxNew False 0
    set vBox [boxHomogeneous := False]
    boxPackStart vBox functionDialog PackNatural 0
    boxPackStart vBox drawingArea PackGrow 0
    
    containerAdd win vBox
    --windowResize win 640 480

    widgetAddEvents drawingArea [Button1MotionMask, ButtonPressMask, ButtonReleaseMask]

    on drawingArea buttonPressEvent (Main.onMouseButton stateRef)
    on drawingArea buttonReleaseEvent (Main.onMouseButton stateRef)
    on drawingArea motionNotifyEvent (Main.onMouseMove stateRef)
    on drawingArea scrollEvent (Main.onMouseScroll stateRef)
    on drawingArea draw (liftIO $ (drawGraph stateRef))

    widgetShowAll win
    w <- widgetGetAllocatedWidth drawingArea
    h <- widgetGetAllocatedHeight drawingArea
    
    modifyMVar_ stateRef $ \state -> return $ 
        state {
            plotSettings =
                (plotSettings state) {
                    screenArea = ScreenArea {
                        screenAreaLeft = 0,
                        screenAreaRight = fromIntegral w,
                        screenAreaBottom = fromIntegral h,
                        screenAreaTop = 0,
                        screenAreaBack = 0,
                        screenAreaFront = 0
                    }
                }
        }
    
    timeoutAdd (yield >> return True) 50
    widgetQueueDraw drawingArea    
    mainGUI

onMouseScroll :: StateRef -> EventM EScroll Bool
onMouseScroll stateRef = do
    (x, y) <- eventCoordinates
    direction <- eventScrollDirection
    state <- liftIO $ readMVar stateRef 
    liftIO $ Plot.onMouseScroll (x, y) direction (plotSettings state) (\newPlotSettings ->
        do 
            modifyMVar_ stateRef $ \state -> return $ state {plotSettings = newPlotSettings}
        )
    liftIO $ widgetQueueDraw $ canvas state
    return True

onMouseButton :: StateRef -> (EventM EButton Bool)
onMouseButton stateRef = do
    button <- eventButton
    modifiers <- eventModifier
    click <- eventClick
    (x, y) <- eventCoordinates
    state <- liftIO $ readMVar stateRef
    liftIO $ Plot.onMouseButton button modifiers click (x, y) (plotSettings state) (\newPlotSettings ->
        do 
            modifyMVar_ stateRef $ \state -> return $ state {plotSettings = newPlotSettings}
        )
    liftIO $ widgetQueueDraw $ canvas state    
    return True

onMouseMove :: StateRef -> EventM EMotion Bool
onMouseMove stateRef = do
    (x, y) <- eventCoordinates
    modifiers <- eventModifier
    state <- liftIO $ readMVar stateRef
    liftIO $ Plot.onMouseMove (x, y) modifiers (plotSettings state) (\newPlotSettings ->
        do 
            modifyMVar_ stateRef $ \state -> return $ state {plotSettings = newPlotSettings}
        )
    --state <- liftIO $ readMVar stateRef
    --liftIO $ putStrLn $ "plotArea" ++ (show $ plotArea (plotSettings state))
    liftIO $ widgetQueueDraw $ canvas state
    return True

drawGraph :: StateRef -> IO ()
drawGraph stateRef = 
    do
        state <- readMVar stateRef
        w <- widgetGetAllocatedWidth (canvas state)
        h <- widgetGetAllocatedHeight (canvas state)
        fn <- entryGetText $ functionEntry state
        dens <- spinButtonGetValue $ densitySpin state
        sc <- spinButtonGetValue $ scaleSpin state

        if fn /= (F.initialExpression (function state)) 
            then
                modifyMVar_ stateRef $ \state -> return $ state {function = F.toComplexFunction (F.function fn)}
            else
                return ()
        if round dens /= density state 
            then
                modifyMVar_ stateRef $ \state -> return $ state {density = round dens}
            else
                return ()
        if sc /= scale state 
            then
                modifyMVar_ stateRef $ \state -> return $ state {scale = sc}
            else
                return ()
                
        state <- readMVar stateRef

        let
            area = plotArea $ plotSettings state
            zs = [x :+ y | x <- [plotAreaLeft area, plotAreaLeft area + xStep .. plotAreaRight area], y <- [plotAreaBottom area, plotAreaBottom area + yStep .. plotAreaTop area]] where
                xStep = (plotAreaRight area - plotAreaLeft area) / (fromIntegral (density state))
                yStep = (plotAreaTop area - plotAreaBottom area) / (fromIntegral h / fromIntegral w * fromIntegral (density state))
            fzs = map (\z -> F.getComplexValue [z] (function state)) zs
            vectros = 
                PlotVectors {
                    plotVectors = V.map (\(z, fz) -> ((realPart z, imagPart z, 0), ((realPart fz) * (scale state) + realPart z, (imagPart fz) * (scale state) + imagPart z, 0))) (V.fromList (zip zs fzs)),
                    plotVectorLineAttributes = PlotLineAttributes {
                        plotLineDash = [1, 0],
                        plotLineWidth = 1, 
                        plotLineColor = (0, 0, 1, 1) 
                    },
                    plotVectorStartStyle = 0,
                    plotVectorEndStyle = 1
                }
            scrArea = ScreenArea 0 (fromIntegral w) (fromIntegral h) 0 0 0
        
            newState = 
                state {
                    plotSettings =
                        (plotSettings state) {
                            screenArea = scrArea
                        }
                }
            
            coef = (fromIntegral h) / (fromIntegral w)
            plotText = PlotText {
                plotText = "Hello World!",
                plotTextFont = Font {
                    fontFace = "Arial",
                    fontSlant = FontSlantNormal,
                    fontWeight = FontWeightNormal,
                    fontSize = 10
                },
                plotTextAngle = 0,
                plotTextPos = (-0.5, -0.5),
                plotTextSize = (Just (ScreenCoord ((fromIntegral w) / 2)), Just (ScreenCoord ((fromIntegral h) / 2))),
                plotTextLineAttributes = Just PlotLineAttributes {
                    plotLineDash = [],
                    plotLineWidth = 1,
                    plotLineColor = (0, 0, 1, 0.5)
                },
                plotTextFillColor = (1, 1, 0, 0.5)
                
            }
        --plot (canvas state) [(plotSettings newState, {-[vectros] -} [plotText])] Nothing
        plot (canvas state) [(plotSettings newState, [vectros])] Nothing
        modifyMVar_ stateRef $ \state -> return newState
        
addWidgetToVBox :: (WidgetClass w) => Maybe String -> w -> VBox -> IO ()
addWidgetToVBox maybeName w vBox = do
    
    hBox <- hBoxNew True 0
    case maybeName of 
        Just name ->
            do
                label <- labelNew maybeName
                boxPackStart hBox label PackNatural 2
        Nothing -> return ()
    boxPackEnd hBox w PackGrow 2
    boxPackStart vBox hBox PackNatural 2

