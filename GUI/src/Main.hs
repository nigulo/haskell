{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (Main.main) where

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Cairo (Context)
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Connector (renderWithContext)

import Data.GI.Base
import Data.GI.Base.GValue

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Control.Concurrent.MVar
import Control.Concurrent
import Debug.Trace
import System.IO
import Data.Complex
import Control.Exception (catch, SomeException)
import qualified Data.Vector.Unboxed as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Data.Int (Int32)

import Control.Monad.IO.Class
import GUI.Plot as Plot
import qualified Math.Function as F

import Utils.Misc
import Utils.List

data State = State {
    window :: Gtk.ApplicationWindow,
    canvas :: Gtk.DrawingArea,
    function :: (F.Function (Complex Double)),
    density :: Int,
    scale :: Double,
    functionEntry :: Gtk.Entry,
    densitySpin :: Gtk.SpinButton,
    scaleSpin :: Gtk.SpinButton,
    plotSettings :: PlotSettings
}

type StateRef = MVar State

main :: IO ()
main = do
    -- Set up UTF-8 encoding for Windows console
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    app <- Gtk.applicationNew (Just "org.example.gui") []

    _ <- Gio.onApplicationActivate app $ activate app

    _ <- Gio.applicationRun app Nothing
    return ()

activate :: Gtk.Application -> IO ()
activate app = do
    win <- Gtk.applicationWindowNew app
    Gtk.windowSetTitle win (Just "Complex Function Analysis")
    Gtk.windowSetDefaultSize win 640 480

    ----------------------------------------------------------------------------

    drawingArea <- Gtk.drawingAreaNew
    Gtk.widgetSetVexpand drawingArea True
    Gtk.widgetSetHexpand drawingArea True

    functionDialog <- Gtk.boxNew Gtk.OrientationVertical 0

    ----------------------------------------------------------------------------
    fnEntry <- Gtk.entryNew
    buffer <- Gtk.entryGetBuffer fnEntry
    Gtk.entryBufferSetText buffer "log(z)" (-1)
    addWidgetToBox (Just "Function: ") fnEntry functionDialog

    densAdjustment <- Gtk.adjustmentNew 20 5 10000 1 1 10
    densSpin <- Gtk.spinButtonNew (Just densAdjustment) 1 0
    addWidgetToBox (Just "Density: ") densSpin functionDialog

    scAdjustment <- Gtk.adjustmentNew 0.1 1e-17 (2**52) 1 1 10
    scSpin <- Gtk.spinButtonNew (Just scAdjustment) 1 10
    addWidgetToBox (Just "Scale: ") scSpin functionDialog

    fnBuffer <- Gtk.entryGetBuffer fnEntry
    fn <- Gtk.entryBufferGetText fnBuffer
    dens <- Gtk.spinButtonGetValue densSpin
    sc <- Gtk.spinButtonGetValue scSpin

    ----------------------------------------------------------------------------
    let
        state = State {
            window = win,
            canvas = drawingArea,
            function = F.toComplexFunction (F.function (T.unpack fn)),
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

    vBox <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.boxAppend vBox functionDialog
    Gtk.boxAppend vBox drawingArea

    Gtk.windowSetChild win (Just vBox)

    -- Set up drawing function
    Gtk.drawingAreaSetDrawFunc drawingArea (Just (drawGraph stateRef))

    -- Set up gesture controllers for mouse events

    -- Drag gesture for mouse movement
    dragGesture <- Gtk.gestureDragNew
    Gtk.widgetAddController drawingArea dragGesture

    _ <- Gtk.onGestureDragDragBegin dragGesture $ \x y -> do
        state <- readMVar stateRef
        modifiers <- getCurrentModifiers win
        liftIO $ Plot.onMouseButton LeftButton modifiers SingleClick (x, y) (plotSettings state) (\newPlotSettings -> do
            modifyMVar_ stateRef $ \s -> return $ s {plotSettings = newPlotSettings}
            )
        Gtk.widgetQueueDraw $ canvas state

    _ <- Gtk.onGestureDragDragUpdate dragGesture $ \offsetX offsetY -> do
        state <- readMVar stateRef
        startX <- Gtk.gestureDragGetStartPoint dragGesture
        case startX of
            (True, sx, sy) -> do
                let x = sx + offsetX
                    y = sy + offsetY
                modifiers <- getCurrentModifiers win
                liftIO $ Plot.onMouseMove (x, y) modifiers (plotSettings state) (\newPlotSettings -> do
                    modifyMVar_ stateRef $ \s -> return $ s {plotSettings = newPlotSettings}
                    )
                Gtk.widgetQueueDraw $ canvas state
            _ -> return ()

    _ <- Gtk.onGestureDragDragEnd dragGesture $ \offsetX offsetY -> do
        state <- readMVar stateRef
        startX <- Gtk.gestureDragGetStartPoint dragGesture
        case startX of
            (True, sx, sy) -> do
                let x = sx + offsetX
                    y = sy + offsetY
                modifiers <- getCurrentModifiers win
                liftIO $ Plot.onMouseButton LeftButton modifiers ReleaseClick (x, y) (plotSettings state) (\newPlotSettings -> do
                    modifyMVar_ stateRef $ \s -> return $ s {plotSettings = newPlotSettings}
                    )
                Gtk.widgetQueueDraw $ canvas state
            _ -> return ()

    -- Scroll controller for zoom
    scrollController <- Gtk.eventControllerScrollNew [Gtk.EventControllerScrollFlagsVertical]
    Gtk.widgetAddController drawingArea scrollController

    _ <- Gtk.onEventControllerScrollScroll scrollController $ \dx dy -> do
        state <- readMVar stateRef
        -- Get current pointer position
        -- For scroll events, we use the widget center as approximation
        w <- Gtk.widgetGetWidth drawingArea
        h <- Gtk.widgetGetHeight drawingArea
        let x = fromIntegral w / 2
            y = fromIntegral h / 2
            direction = if dy < 0 then ScrollUp else ScrollDown
        liftIO $ Plot.onMouseScroll (x, y) direction (plotSettings state) (\newPlotSettings -> do
            modifyMVar_ stateRef $ \s -> return $ s {plotSettings = newPlotSettings}
            )
        Gtk.widgetQueueDraw $ canvas state
        return True

    -- Key controller
    keyController <- Gtk.eventControllerKeyNew
    Gtk.widgetAddController win keyController

    _ <- Gtk.onEventControllerKeyKeyPressed keyController $ \keyval keycode modifiers -> do
        state <- readMVar stateRef
        keyName <- Gdk.keyvalName keyval
        case keyName of
            Just name -> do
                liftIO $ Plot.onKeyDown (T.unpack name) drawingArea (plotSettings state) (\newPlotSettings -> do
                    modifyMVar_ stateRef $ \s -> return $ s {plotSettings = newPlotSettings}
                    )
                Gtk.widgetQueueDraw $ canvas state
            Nothing -> return ()
        return False

    Gtk.widgetSetVisible win True

    w <- Gtk.widgetGetWidth drawingArea
    h <- Gtk.widgetGetHeight drawingArea

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

    Gtk.widgetQueueDraw drawingArea

-- Helper to get current modifier state
getCurrentModifiers :: Gtk.ApplicationWindow -> IO [Plot.Modifier]
getCurrentModifiers win = do
    -- In GTK4, getting modifiers from gesture is more complex
    -- For simplicity, we'll return empty list for now
    -- A proper implementation would use Gdk.SeatGrab and check key state
    return []

drawGraph :: StateRef -> Gtk.DrawingArea -> GI.Cairo.Context -> Int32 -> Int32 -> IO ()
drawGraph stateRef _drawingArea context width height = do
    -- Read state and widget values
    state <- readMVar stateRef

    fnBuffer <- Gtk.entryGetBuffer $ functionEntry state
    fn <- Gtk.entryBufferGetText fnBuffer
    dens <- Gtk.spinButtonGetValue $ densitySpin state
    sc <- Gtk.spinButtonGetValue $ scaleSpin state

    let fnStr = T.unpack fn
        currentDens = round dens
        currentFunc = if fnStr /= F.initialExpression (function state)
                      then F.toComplexFunction (F.function fnStr)
                      else function state
        currentScale = sc

    let
        w = fromIntegral width
        h = fromIntegral height
        area = plotArea $ plotSettings state
        zs = [x :+ y | x <- [plotAreaLeft area, plotAreaLeft area + xStep .. plotAreaRight area], y <- [plotAreaBottom area, plotAreaBottom area + yStep .. plotAreaTop area]] where
            xStep = (plotAreaRight area - plotAreaLeft area) / (fromIntegral currentDens)
            yStep = (plotAreaTop area - plotAreaBottom area) / (h / w * fromIntegral currentDens)
        fzs = map (\z -> F.getComplexValue [z] currentFunc) zs
        vectors =
            PlotVectors {
                plotVectors = V.map (\(z, fz) -> ((realPart z, imagPart z, 0), ((realPart fz) * currentScale + realPart z, (imagPart fz) * currentScale + imagPart z, 0))) (V.fromList (zip zs fzs)),
                plotVectorLineAttributes = PlotLineAttributes {
                    plotLineDash = [1, 0],
                    plotLineWidth = 1,
                    plotLineColor = (0, 0, 1, 1)
                },
                plotVectorStartStyle = 0,
                plotVectorEndStyle = 1
            }
        scrArea = ScreenArea 0 w h 0 0 0
        newPlotSettings = (plotSettings state) { screenArea = scrArea }

    -- Render using Cairo - wrap in exception handler to avoid crashes
    catch (renderWithContext (renderPlot [(newPlotSettings, [vectors])]) context)
          (\e -> hPutStrLn stderr $ "Render error: " ++ show (e :: SomeException))

    -- Update state after rendering
    modifyMVar_ stateRef $ \s -> return $ s {
        function = currentFunc,
        density = currentDens,
        scale = currentScale,
        plotSettings = newPlotSettings
    }

addWidgetToBox :: (Gtk.IsWidget w) => Maybe Text -> w -> Gtk.Box -> IO ()
addWidgetToBox maybeName w vBox = do

    hBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    case maybeName of
        Just name -> do
            label <- Gtk.labelNew (Just name)
            Gtk.boxAppend hBox label
        Nothing -> return ()
    widget <- Gtk.toWidget w
    Gtk.widgetSetHexpand widget True
    Gtk.boxAppend hBox widget
    Gtk.boxAppend vBox hBox
