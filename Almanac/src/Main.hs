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

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.IO.Class
import Debug.Trace
import System.IO
import Data.Complex
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int32)
import Control.Exception (catch, SomeException)

import GUI.Plot as Plot
import GUI.Widget

import Utils.Misc
import Utils.List
import Ephem.Types
import Ephem.Time
import Ephem.Coords
import Ephem.Utils
import Ephem.OrbitalElements
import Ephem.Sun hiding (main)
import Ephem.CelestialBody
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import qualified Data.Map as M

import qualified Data.Vector.Unboxed as V

data State = State {
    window :: Gtk.ApplicationWindow,
    canvas :: Gtk.DrawingArea,
    year :: Int,
    latitude :: Lat,
    longitude :: Long,
    timeZone :: Int,
    yearSpin :: Gtk.SpinButton,
    northSouthDropDown :: Gtk.DropDown,
    latitudeDegSpin :: Gtk.SpinButton,
    latitudeMinSpin :: Gtk.SpinButton,
    latitudeSecSpin :: Gtk.SpinButton,
    eastWestDropDown :: Gtk.DropDown,
    longitudeDegSpin :: Gtk.SpinButton,
    longitudeMinSpin :: Gtk.SpinButton,
    longitudeSecSpin :: Gtk.SpinButton,
    timeZoneSpin :: Gtk.SpinButton,
    plotSettings :: PlotSettings
}

type StateRef = MVar State

main :: IO ()
main = do
    -- Set up UTF-8 encoding for Windows console
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    app <- Gtk.applicationNew (Just "org.example.almanac") []

    _ <- Gio.onApplicationActivate app $ activate app

    _ <- Gio.applicationRun app Nothing
    return ()

activate :: Gtk.Application -> IO ()
activate app = do
    win <- Gtk.applicationWindowNew app
    Gtk.windowSetTitle win (Just "Almanac")
    Gtk.windowSetDefaultSize win 500 800

    ----------------------------------------------------------------------------

    drawingArea <- Gtk.drawingAreaNew
    Gtk.widgetSetVexpand drawingArea True
    Gtk.widgetSetHexpand drawingArea True

    dialog <- Gtk.boxNew Gtk.OrientationVertical 0

    -- Year
    yrAdjustment <- Gtk.adjustmentNew 2013 1800 2050 1 1 10
    yrSpin <- Gtk.spinButtonNew (Just yrAdjustment) 1 0
    addWidgetToBox (Just "Year: ") yrSpin dialog

    -- Latitude
    latBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    latLabel <- Gtk.labelNew (Just "Latitude: ")
    Gtk.boxAppend latBox latLabel

    nsDropDown <- createDropDown ["N", "S"]
    Gtk.dropDownSetSelected nsDropDown 0
    Gtk.boxAppend latBox nsDropDown

    latDegAdjustment <- Gtk.adjustmentNew 58 0 90 1 1 10
    latDegSpin <- Gtk.spinButtonNew (Just latDegAdjustment) 1 0
    Gtk.boxAppend latBox latDegSpin

    latMinAdjustment <- Gtk.adjustmentNew 22 0 60 1 1 10
    latMinSpin <- Gtk.spinButtonNew (Just latMinAdjustment) 1 0
    Gtk.boxAppend latBox latMinSpin

    latSecAdjustment <- Gtk.adjustmentNew 47 0 60 1 1 10
    latSecSpin <- Gtk.spinButtonNew (Just latSecAdjustment) 1 0
    Gtk.boxAppend latBox latSecSpin

    Gtk.boxAppend dialog latBox

    -- Longitude
    longBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    longLabel <- Gtk.labelNew (Just "Longitude: ")
    Gtk.boxAppend longBox longLabel

    ewDropDown <- createDropDown ["E", "W"]
    Gtk.dropDownSetSelected ewDropDown 0
    Gtk.boxAppend longBox ewDropDown

    longDegAdjustment <- Gtk.adjustmentNew 26 0 180 1 1 10
    longDegSpin <- Gtk.spinButtonNew (Just longDegAdjustment) 1 0
    Gtk.boxAppend longBox longDegSpin

    longMinAdjustment <- Gtk.adjustmentNew 43 0 60 1 1 10
    longMinSpin <- Gtk.spinButtonNew (Just longMinAdjustment) 1 0
    Gtk.boxAppend longBox longMinSpin

    longSecAdjustment <- Gtk.adjustmentNew 18 0 60 1 1 10
    longSecSpin <- Gtk.spinButtonNew (Just longSecAdjustment) 1 0
    Gtk.boxAppend longBox longSecSpin

    Gtk.boxAppend dialog longBox

    -- Time zone
    tzAdjustment <- Gtk.adjustmentNew 2 (-12) 12 1 1 10
    tzSpin <- Gtk.spinButtonNew (Just tzAdjustment) 1 0
    addWidgetToBox (Just "Time zone: ") tzSpin dialog

    ----------------------------------------------------------------------------
    yr <- Gtk.spinButtonGetValue yrSpin
    ns <- Gtk.dropDownGetSelected nsDropDown
    latDeg <- Gtk.spinButtonGetValue latDegSpin
    latMin <- Gtk.spinButtonGetValue latMinSpin
    latSec <- Gtk.spinButtonGetValue latSecSpin
    ew <- Gtk.dropDownGetSelected ewDropDown
    longDeg <- Gtk.spinButtonGetValue longDegSpin
    longMin <- Gtk.spinButtonGetValue longMinSpin
    longSec <- Gtk.spinButtonGetValue longSecSpin
    tz <- Gtk.spinButtonGetValue tzSpin

    ----------------------------------------------------------------------------
    let
        lat = DMS (round latDeg) (round latMin) latSec
        long = DMS (round longDeg) (round longMin) longSec
        state = State {
            window = win,
            canvas = drawingArea,
            year = round yr,
            latitude = if ns == 0 then Lat lat N else Lat lat S,
            longitude = if ew == 0 then Long long E else Long long W,
            timeZone = round tz,
            yearSpin = yrSpin,
            northSouthDropDown = nsDropDown,
            latitudeDegSpin = latDegSpin,
            latitudeMinSpin = latMinSpin,
            latitudeSecSpin = latSecSpin,
            eastWestDropDown = ewDropDown,
            longitudeDegSpin = longDegSpin,
            longitudeMinSpin = longMinSpin,
            longitudeSecSpin = longSecSpin,
            timeZoneSpin = tzSpin,
            plotSettings = PlotSettings {
                plotArea = PlotArea {
                    plotAreaLeft = 0,
                    plotAreaRight = 24,
                    plotAreaBottom = -30,
                    plotAreaTop = 395,
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
                plotMinorXUnit = Left False,
                plotMinorYUnit = Left False,
                plotMajorXUnit = Left False,
                plotMajorYUnit = Left False,
                plotBackground = (1, 1, 1),
                mousePos = Nothing,
                plotSelection = Nothing,
                plotSegments = Nothing,
                plotTool = PlotToolSelect
            }
        }
    stateRef <- newMVar state
    ----------------------------------------------------------------------------

    vBox <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.boxAppend vBox dialog
    Gtk.boxAppend vBox drawingArea

    Gtk.windowSetChild win (Just vBox)

    -- Set up drawing function
    Gtk.drawingAreaSetDrawFunc drawingArea (Just (drawAlmanac stateRef))

    -- Set up gesture controllers for mouse events

    -- Drag gesture for mouse movement
    dragGesture <- Gtk.gestureDragNew
    Gtk.widgetAddController drawingArea dragGesture

    _ <- Gtk.onGestureDragDragBegin dragGesture $ \x y -> do
        state <- readMVar stateRef
        liftIO $ Plot.onMouseButton LeftButton [] SingleClick (x, y) (plotSettings state) (\newPlotSettings -> do
            modifyMVar_ stateRef $ \s -> return $ s {plotSettings = newPlotSettings}
            )
        Gtk.widgetQueueDraw drawingArea

    _ <- Gtk.onGestureDragDragUpdate dragGesture $ \offsetX offsetY -> do
        state <- readMVar stateRef
        startPoint <- Gtk.gestureDragGetStartPoint dragGesture
        case startPoint of
            (True, sx, sy) -> do
                let x = sx + offsetX
                    y = sy + offsetY
                liftIO $ Plot.onMouseMove (x, y) [] (plotSettings state) (\newPlotSettings -> do
                    modifyMVar_ stateRef $ \s -> return $ s {plotSettings = newPlotSettings}
                    )
                Gtk.widgetQueueDraw drawingArea
            _ -> return ()

    _ <- Gtk.onGestureDragDragEnd dragGesture $ \offsetX offsetY -> do
        state <- readMVar stateRef
        startPoint <- Gtk.gestureDragGetStartPoint dragGesture
        case startPoint of
            (True, sx, sy) -> do
                let x = sx + offsetX
                    y = sy + offsetY
                liftIO $ Plot.onMouseButton LeftButton [] ReleaseClick (x, y) (plotSettings state) (\newPlotSettings -> do
                    modifyMVar_ stateRef $ \s -> return $ s {plotSettings = newPlotSettings}
                    )
                Gtk.widgetQueueDraw drawingArea
            _ -> return ()

    -- Scroll controller for zoom
    scrollController <- Gtk.eventControllerScrollNew [Gtk.EventControllerScrollFlagsVertical]
    Gtk.widgetAddController drawingArea scrollController

    _ <- Gtk.onEventControllerScrollScroll scrollController $ \dx dy -> do
        state <- readMVar stateRef
        w <- Gtk.widgetGetWidth drawingArea
        h <- Gtk.widgetGetHeight drawingArea
        let x = fromIntegral w / 2
            y = fromIntegral h / 2
            direction = if dy < 0 then ScrollUp else ScrollDown
        liftIO $ Plot.onMouseScroll (x, y) direction (plotSettings state) (\newPlotSettings -> do
            modifyMVar_ stateRef $ \s -> return $ s {plotSettings = newPlotSettings}
            )
        Gtk.widgetQueueDraw drawingArea
        return True

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

drawAlmanac :: StateRef -> Gtk.DrawingArea -> GI.Cairo.Context -> Int32 -> Int32 -> IO ()
drawAlmanac stateRef _drawingArea context width height = do
    state <- readMVar stateRef
    yr <- Gtk.spinButtonGetValue $ yearSpin state
    ns <- Gtk.dropDownGetSelected $ northSouthDropDown state
    latDeg <- Gtk.spinButtonGetValue $ latitudeDegSpin state
    latMin <- Gtk.spinButtonGetValue $ latitudeMinSpin state
    latSec <- Gtk.spinButtonGetValue $ latitudeSecSpin state
    ew <- Gtk.dropDownGetSelected $ eastWestDropDown state
    longDeg <- Gtk.spinButtonGetValue $ longitudeDegSpin state
    longMin <- Gtk.spinButtonGetValue $ longitudeMinSpin state
    longSec <- Gtk.spinButtonGetValue $ longitudeSecSpin state
    tz <- Gtk.spinButtonGetValue $ timeZoneSpin state

    let
        w = fromIntegral width
        h = fromIntegral height
        lat = DMS (round latDeg) (round latMin) latSec
        long = DMS (round longDeg) (round longMin) longSec

    modifyMVar_ stateRef $ \state -> return $ state {
            year = round yr,
            latitude = if ns == 0 then Lat lat N else Lat lat S,
            longitude = if ew == 0 then Long long E else Long long W,
            timeZone = round tz
        }

    state <- readMVar stateRef

    let
        yrInt = fromIntegral (year state)
        leapYear = isLeapYear yrInt
        days = if leapYear then [1 .. 366] else [1 .. 365]
        dates = map (\(month, day) -> (YMD yrInt month (fromIntegral day))) $ map (dayOfYearToMonthAndDay leapYear) days

        (sunRiseSetMap, sunRisesAndSets) = getSunRisesAndSets state dates leapYear

        sunRiseSetPlotData = getSunRiseSetPlotData sunRisesAndSets

        (rises, sets) = unzip $ concat sunRisesAndSets
        minHour = 12 + (floor (minimum (map (\(set, _) -> set) sets)))
        maxHour = (ceiling (maximum (map (\(rise, _) -> rise) rises))) - 12

        planetsRiseSetPlotData = getPlanetsRiseSetPlotData state dates leapYear sunRiseSetMap
        planetsTransitPlotData = getPlanetsTransitPlotData state dates leapYear sunRiseSetMap

        area = plotArea $ plotSettings state
        scrArea = ScreenArea 0 w h 0 0 0

        hoursPlotData = concat $ map (getHourPlotData area scrArea ) ([0 .. maxHour] ++ [minHour .. 23])
        monthsPlotData = concat $ map (getMonthPlotData area scrArea leapYear) [1 .. 12]

        newPlotSettings = (plotSettings state) { screenArea = scrArea }

    -- Render using Cairo
    catch (renderWithContext (renderPlot [(newPlotSettings,
            sunRiseSetPlotData
            ++ planetsRiseSetPlotData
            ++ planetsTransitPlotData
            ++ hoursPlotData
            ++ monthsPlotData
            )]) context)
          (\e -> hPutStrLn stderr $ "Render error: " ++ show (e :: SomeException))

    modifyMVar_ stateRef $ \_ -> return $ state { plotSettings = newPlotSettings }

getSunRiseSetPlotData :: [[((Double, Double), (Double, Double))]] -> [PlotData]
getSunRiseSetPlotData sunRisesAndSets =
    let
        sunRiseSetPlotData = concat $
            map (\risesAndSets ->
                let
                    (rises, sets) = unzip risesAndSets
                    rises1 = splitRiseOrSetData rises [[]]
                    sets1 = splitRiseOrSetData sets [[]]
                in
                    map (\rises ->
                        PlotData {
                            plotDataValues = V.map (\(x, y) -> ((x, 0), (y, 0))) $ V.fromList rises,
                            plotDataPointAttributes = Nothing,
                            plotDataLineAttributes = Just PlotLineAttributes {
                                plotLineDash = [1, 0],
                                plotLineWidth = 2,
                                plotLineColor = (0, 0, 1, 1)
                            }
                        }
                    ) rises1 ++
                    map (\sets ->
                        PlotData {
                            plotDataValues = V.map (\(x, y) -> ((x, 0), (y, 0))) $ V.fromList sets,
                            plotDataPointAttributes = Nothing,
                            plotDataLineAttributes = Just PlotLineAttributes {
                                plotLineDash = [1, 0],
                                plotLineWidth = 2,
                                plotLineColor = (0, 0, 1, 1)
                            }
                        }
                    ) sets1
            ) sunRisesAndSets
    in
        sunRiseSetPlotData

getSunRisesAndSets :: State -> [Date] -> Bool -> (
    M.Map Date ((Hours, Angle) {- sun rise -}, (Hours, Angle) {- sun set -}),
    [[((Double, Double), (Double, Double))]] -- Rises and sets in plot coords
    )
getSunRisesAndSets state dates leapYear =
    let
        sunRiseSets = groupRiseSets $ convertToLT state $ map (\date -> (date, calcSunRiseSet date earth2000 (latitude state) (longitude state))) dates

        sunRisesAndSets =
            map (\riseSets ->
                map (\(date, ((rise, _), (set, _))) ->
                    let
                        YMD y m d = toYMD date
                        day = monthAndDayToDayOfYear leapYear m (floor d)
                        Hrs r = toHrs rise
                        Hrs s = toHrs set
                    in
                        ((r + 12, fromIntegral day), (s - 12, fromIntegral day))
                ) riseSets
            ) sunRiseSets
    in
        (M.fromList (concat sunRiseSets), sunRisesAndSets)

getHourPlotData :: PlotArea -> ScreenArea -> Int -> [PlotData]
getHourPlotData plotArea scrArea hour =
    let
        scrWidth = screenAreaRight scrArea - screenAreaLeft scrArea
        scrHeight = screenAreaBottom scrArea - screenAreaTop scrArea

        boxHeight = (min scrWidth scrHeight) / 50
        boxWidth = boxHeight
        x = clipHour (fromIntegral hour - 12)
        (xScr, _, _) = toScreenCoords scrArea plotArea (x, 0, 0)
        xScr' = xScr - boxWidth
        (xShifted, _) = toGraphCoords scrArea plotArea (xScr', 0)

        hourLabel = PlotText {
            plotText = show hour,
            plotTextFont = Font {
                fontFace = "Arial",
                fontSlant = FontSlantNormal,
                fontWeight = FontWeightNormal,
                fontSize = 10
            },
            plotTextAngle = 0,
            plotTextPos = (xShifted, -10),
            plotTextSize = (Just (ScreenCoord boxWidth), (Just (ScreenCoord boxHeight))),
            plotTextLineAttributes = Just PlotLineAttributes {
                plotLineDash = [],
                plotLineWidth = 1,
                plotLineColor = (0, 0, 0, 1)
            },
            plotTextFillColor = (0, 0, 0, 1)

        }
    in
        [hourLabel, hourLabel {plotTextPos = (xShifted, 366)}]

monthName :: Int -> String
monthName 1 = "January"
monthName 2 = "February"
monthName 3 = "March"
monthName 4 = "April"
monthName 5 = "May"
monthName 6 = "June"
monthName 7 = "July"
monthName 8 = "August"
monthName 9 = "September"
monthName 10 = "October"
monthName 11 = "November"
monthName 12 = "December"
monthName _ = ""

getMonthPlotData :: PlotArea -> ScreenArea -> Bool -> Int -> [PlotData]
getMonthPlotData plotArea scrArea leapYear month =
    let
        numDays = monthLength leapYear month
        plotWidth = plotAreaRight plotArea - plotAreaLeft plotArea
        plotHeight = plotAreaTop plotArea - plotAreaBottom plotArea
        scrWidth = screenAreaRight scrArea - screenAreaLeft scrArea
        scrHeight = screenAreaBottom scrArea - screenAreaTop scrArea
        rectBottom = fromIntegral (if month == 1 then 0 else sum (map (monthLength leapYear) [1 .. month - 1]))
        rectWidth = plotWidth / 20
        rectHeight = fromIntegral numDays
        rectangle = PlotRectangle {
            plotRectangleLeft = 0 - rectWidth,
            plotRectangleRight = 0,
            plotRectangleBottom = rectBottom,
            plotRectangleTop = rectBottom + rectHeight,
            plotRectangleLineAttributes = PlotLineAttributes {
                plotLineDash = [],
                plotLineWidth = 2,
                plotLineColor = (0, 0, 0, 1)
            },
            plotRectangleFillColor = (1, 1, 1, 1)
        }
        monthLabel = PlotText {
            plotText = monthName month,
            plotTextFont = Font {
                fontFace = "Arial",
                fontSlant = FontSlantNormal,
                fontWeight = FontWeightNormal,
                fontSize = 10
            },
            plotTextAngle = -90,
            plotTextPos = (0 - rectWidth * 0.1, rectBottom + rectHeight * 0.2),
            plotTextSize = (Just (ScreenCoord (rectHeight * 0.6 * scrHeight / plotHeight)), Just (ScreenCoord (rectWidth * 0.8 * scrWidth / plotWidth))),
            plotTextLineAttributes = Just PlotLineAttributes {
                plotLineDash = [],
                plotLineWidth = 1,
                plotLineColor = (0, 0, 0, 1)
            },
            plotTextFillColor = (0, 0, 0, 1)

        }
    in
        [rectangle, monthLabel]

getPlanetColor :: Planet -> (Double, Double, Double, Double)
getPlanetColor planet =
    let
        (r, g, b) = getPlanetColor' planet where
            getPlanetColor' Mercury = (125, 125, 125) -- light grey
            getPlanetColor' Venus = (255, 255, 0) -- yellow
            getPlanetColor' Mars = (255, 0, 0) -- red
            getPlanetColor' Jupiter = (255, 125, 0) -- orange
            getPlanetColor' Saturn = (125, 0, 255) -- violet
            getPlanetColor' Uranus = (0, 255, 255) -- cyan
            getPlanetColor' Neptune = (0, 125, 255) -- ocean
            getPlanetColor' Pluto = (65, 65, 65) -- dark grey
    in
        (r / 255, g / 255, b / 255, 1)

getPlanetsRiseSet :: State -> [Date] -> Bool -> [(Planet, OrbitalElements)] -> [(Planet, [[(Date, ((Hours, Angle) {- rise -}, (Hours, Angle) {- set -}))]])]
getPlanetsRiseSet state dates _ =
    map (\(planetName, planet) ->
            (planetName, groupRiseSets $ convertToLT state $ map (\date -> (date, calcPlanetRiseSet date planet earth2000 (latitude state) (longitude state))) dates)
        )

filterPlanetData :: State
    -> M.Map Date ((Hours, Angle) {- sun rise -}, (Hours, Angle) {- sun set -})
    -> (Date, Hours {- rise, set or transit -})
    -> Bool
filterPlanetData state sunRisesAndSets (date, time) =
    let
        lat = latitude state
        maybeSunRiseAndSet = M.lookup date sunRisesAndSets
    in
        case maybeSunRiseAndSet of
            Just ((sunRise, _), (sunSet, _)) ->
                let
                    Hrs sunRiseHrs = toHrs sunRise
                    Hrs sunSetHrs = toHrs sunSet
                    Hrs timeHrs = toHrs time
                in
                    timeHrs < sunRiseHrs || timeHrs > sunSetHrs
            Nothing ->
                let
                    (sunLong, _) = calcSun earth2000 date
                    tilt = calcObliquityOfEcliptic date
                    (ra, dec) = eclToEqu sunLong (Deg 0) tilt
                    lha = raToLHA ra (Hrs 0)
                    (alt, _) = equToHor lha dec lat
                    Rad altRad = toRad alt
                    polarNight = altRad < 0
                in
                    trace ("polarNight=" ++ show polarNight) polarNight

convertToPlotCoords :: Bool -> (Date, Hours) -> (Double, Double)
convertToPlotCoords leapYear (date, riseOrSet) =
    let
        YMD _ m d = toYMD date
        day = monthAndDayToDayOfYear leapYear m (floor d)
        Hrs rs = toHrs riseOrSet
    in
        ((clipHour (rs + 12), fromIntegral day))

getPlanetsRiseSetPlotData :: State -> [Date] -> Bool -> M.Map Date ((Hours, Angle) {- sun rise -}, (Hours, Angle) {- sun set -}) -> [PlotData]
getPlanetsRiseSetPlotData state dates leapYear sunRisesAndSets =
    let
        planetRiseSets = getPlanetsRiseSet state dates leapYear (take 2 planets2000)
        planetRisesAndSets =
            map (\(planetName, pRiseSets) ->
                (planetName, map (\riseSets ->
                    let
                        (rises, sets) = unzip $ map (\(date, ((rise, _), (set, _))) -> ((date, rise), (date, set))) riseSets
                    in
                        (map (convertToPlotCoords leapYear) (filter (filterPlanetData state sunRisesAndSets) rises),
                            map (convertToPlotCoords leapYear) (filter (filterPlanetData state sunRisesAndSets) sets))

                ) pRiseSets)
            ) planetRiseSets
    in
        concat $
            map (\(planetName, risesAndSets) -> concat $
                map (\(rises, sets) ->
                    let
                        rises1 = splitRiseOrSetData rises [[]]
                        sets1 = splitRiseOrSetData sets [[]]
                    in
                        concat [map (getPlanetPlotData planetName) rises1, map (getPlanetPlotData planetName) sets1]
                    ) risesAndSets
            ) planetRisesAndSets

getPlanetsTransitPlotData :: State -> [Date] -> Bool -> M.Map Date ((Hours, Angle) {- sun rise -}, (Hours, Angle) {- sun set -}) -> [PlotData]
getPlanetsTransitPlotData state dates leapYear sunRisesAndSets =
    let
        planetRiseSets = getPlanetsRiseSet state dates leapYear (drop 2 planets2000)
        planetCulminations =
            map (\(planetName, pRiseSets) ->
                (planetName, map (\riseSets ->
                    let
                        transits = map (\(date, ((rise, _), (set, _))) ->
                            let
                                Hrs r = toHrs rise
                                Hrs s = toHrs set
                            in
                                (date, if r < s then Hrs (clipHour ((r + s) / 2)) else Hrs (clipHour ((r - 24 + s) / 2)))
                            ) riseSets
                    in
                        map (convertToPlotCoords leapYear) (filter (filterPlanetData state sunRisesAndSets) transits)
                ) pRiseSets)
            ) planetRiseSets
    in
        concat $
            map (\(planetName, pCulminations) -> concat $
                map (\culminations ->
                    let
                        culminations1 = splitRiseOrSetData culminations [[]]
                    in
                        map (getPlanetPlotData planetName) culminations1
                    ) pCulminations
            ) planetCulminations

getPlanetPlotData :: Planet -> [(Double, Double)] -> PlotData
getPlanetPlotData planetName riseOrSetData =
    PlotData {
        plotDataValues = V.map (\(x, y) -> ((x, 0), (y, 0))) $ V.fromList riseOrSetData,
        plotDataPointAttributes = Nothing,
        plotDataLineAttributes = Just PlotLineAttributes {
            plotLineDash = [1, 0],
            plotLineWidth = 2,
            plotLineColor = getPlanetColor planetName
        }
    }

splitRiseOrSetData :: [(Double, Double)] -> [[(Double, Double)]] -> [[(Double, Double)]]
splitRiseOrSetData [] arr = arr
splitRiseOrSetData (riseOrSet:riseOrSetData) [[]] = splitRiseOrSetData riseOrSetData [[riseOrSet]]
splitRiseOrSetData ((riseOrSet, day):riseOrSetData) arr =
    let
        (lastRiseOrSet, lastDay) = last $ last arr
    in
        if abs (riseOrSet - lastRiseOrSet) > 12 || abs (day - lastDay) > 1
            then
                splitRiseOrSetData riseOrSetData (arr ++ [[(riseOrSet, day)]])
            else
                splitRiseOrSetData riseOrSetData (init arr ++ [last arr ++ [(riseOrSet, day)]])

groupRiseSets :: [(Date, Maybe (((Hours, Angle), (Hours, Angle))))] -> [[(Date, ((Hours, Angle), (Hours, Angle)))]]
groupRiseSets riseSets =
    let
        groupFn [] arr = arr
        groupFn (riseSet:rest) arr =
            case riseSet of
                (date, Just rs) -> groupFn rest (init arr ++ [last arr ++ [(date, rs)]])
                (_, Nothing) -> groupFn rest (arr ++ [[]])
    in
        groupFn riseSets [[]]

convertToLT :: State -> [(Date, Maybe ((GMT, Angle), (GMT, Angle)))] -> [(Date, Maybe ((Hours, Angle), (Hours, Angle)))]
convertToLT state datesRiseSets =
    let
        gmtToLTMapOp (date, riseSet) =
            case riseSet of
                Just ((rise, riseAzi), (set, setAzi)) -> (date, Just ((gmtToLT rise (timeZone state), riseAzi), (gmtToLT set (timeZone state), setAzi)))
                Nothing -> (date, Nothing)
    in
        map gmtToLTMapOp datesRiseSets

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
