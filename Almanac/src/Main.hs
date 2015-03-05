
module Main (Main.main) where

import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.Gdk.EventM

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.IO.Class
import Debug.Trace
import System.IO
import Data.Complex

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
    window :: Window,
    canvas :: DrawingArea,
    year :: Int,
    latitude :: Lat,
    longitude :: Long,
    timeZone :: Int,
    yearSpin :: SpinButton,
    northSouthCombo :: ComboBox,
    latitudeDegSpin :: SpinButton,
    latitudeMinSpin :: SpinButton,
    latitudeSecSpin :: SpinButton,
    eastWestCombo :: ComboBox,
    longitudeDegSpin :: SpinButton,
    longitudeMinSpin :: SpinButton,
    longitudeSecSpin :: SpinButton,
    timeZoneSpin :: SpinButton,
    plotSettings :: PlotSettings
}

type StateRef = MVar State

main = do
    --initGUI
    unsafeInitGUIForThreadedRTS
    
    win <- windowNew
    set win [windowTitle := "Almanac"]
    win `on` objectDestroy $ mainQuit
    windowSetDefaultSize win 320 240
    
    ----------------------------------------------------------------------------

    drawingArea <- drawingAreaNew
    widgetModifyBg drawingArea StateNormal (Color 65535 65535 65535)
    
    dialog <- vBoxNew False 0
    
    -- Year
    yrAdjustment <- adjustmentNew 2013 1800 2050 1 1 10
    yrSpin <- spinButtonNew yrAdjustment 1 0
    addWidgetToVBox (Just "Year: ") yrSpin dialog

    -- Latitude
    latBox <- hBoxNew False 0
    latLabel <- labelNew (Just "Latitude: ")
    boxPackStart latBox latLabel PackNatural 2
    
    nsCombo <- createComboBox ["N", "S"]
    comboBoxSetActive nsCombo 0
    boxPackStart latBox nsCombo PackNatural 2
    
    latDegAdjustment <- adjustmentNew 58 0 90 1 1 10
    latDegSpin <- spinButtonNew latDegAdjustment 1 0
    boxPackStart latBox latDegSpin PackNatural 2

    latMinAdjustment <- adjustmentNew 22 0 60 1 1 10
    latMinSpin <- spinButtonNew latMinAdjustment 1 0
    boxPackStart latBox latMinSpin PackNatural 2

    latSecAdjustment <- adjustmentNew 47 0 60 1 1 10
    latSecSpin <- spinButtonNew latSecAdjustment 1 0
    boxPackStart latBox latSecSpin PackNatural 2

    addWidgetToVBox Nothing latBox dialog

    -- Longitude
    longBox <- hBoxNew False 0
    longLabel <- labelNew (Just "Longitude: ")
    boxPackStart longBox longLabel PackNatural 2
    
    ewCombo <- createComboBox ["E", "W"]
    comboBoxSetActive ewCombo 0
    boxPackStart longBox ewCombo PackNatural 2
    
    longDegAdjustment <- adjustmentNew 26 0 90 1 1 10
    longDegSpin <- spinButtonNew longDegAdjustment 1 0
    boxPackStart longBox longDegSpin PackNatural 2

    longMinAdjustment <- adjustmentNew 43 0 60 1 1 10
    longMinSpin <- spinButtonNew longMinAdjustment 1 0
    boxPackStart longBox longMinSpin PackNatural 2

    longSecAdjustment <- adjustmentNew 18 0 60 1 1 10
    longSecSpin <- spinButtonNew longSecAdjustment 1 0
    boxPackStart longBox longSecSpin PackNatural 2

    addWidgetToVBox Nothing longBox dialog
    
    -- Time zone
    tzAdjustment <- adjustmentNew 2 (-12) 12 1 1 10
    tzSpin <- spinButtonNew tzAdjustment 1 0
    addWidgetToVBox (Just "Time zone: ") tzSpin dialog
    
    ----------------------------------------------------------------------------
    yr <- spinButtonGetValue yrSpin
    ns <- comboBoxGetActive nsCombo
    latDeg <- spinButtonGetValue latDegSpin
    latMin <- spinButtonGetValue latSecSpin
    latSec <- spinButtonGetValue latSecSpin
    ew <- comboBoxGetActive ewCombo
    longDeg <- spinButtonGetValue longDegSpin
    longMin <- spinButtonGetValue longMinSpin
    longSec <- spinButtonGetValue longSecSpin
    tz <- spinButtonGetValue tzSpin

    ----------------------------------------------------------------------------
    let
        lat = DMS (round latDeg) (round latMin) latSec
        long = DMS (round longDeg) (round longMin) longSec
        leapYear = isLeapYear (round yr)
        state = State {
            window = win,
            canvas = drawingArea,
            year = round yr,
            latitude = if ns == 0 then Lat lat N else Lat lat S,
            longitude = if ew == 0 then Long long E else Long long W,
            timeZone = round tz,
            yearSpin = yrSpin,
            northSouthCombo = nsCombo,
            latitudeDegSpin = latDegSpin,
            latitudeMinSpin = latMinSpin,
            latitudeSecSpin = latSecSpin,
            eastWestCombo = ewCombo,
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


    vBox <- vBoxNew False 0
    set vBox [boxHomogeneous := False]
    boxPackStart vBox dialog PackNatural 0
    boxPackStart vBox drawingArea PackGrow 0
    
    containerAdd win vBox
    windowResize win 500 800

    on drawingArea buttonPressEvent (Main.onMouseButton stateRef)
    on drawingArea buttonReleaseEvent (Main.onMouseButton stateRef)
    on drawingArea motionNotifyEvent (Main.onMouseMove stateRef)
    on drawingArea scrollEvent (Main.onMouseScroll stateRef)
    on drawingArea draw (liftIO $ (drawAlmanac stateRef))

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

drawAlmanac :: StateRef -> IO ()
drawAlmanac stateRef = 
    do
        state <- readMVar stateRef
        w <- widgetGetAllocatedWidth (canvas state)
        h <- widgetGetAllocatedHeight (canvas state)
        yr <- spinButtonGetValue $ yearSpin state
        ns <- comboBoxGetActive $ northSouthCombo state
        latDeg <- spinButtonGetValue $ latitudeDegSpin state
        latMin <- spinButtonGetValue $ latitudeSecSpin state
        latSec <- spinButtonGetValue $ latitudeSecSpin state
        ew <- comboBoxGetActive $ eastWestCombo state
        longDeg <- spinButtonGetValue $ longitudeDegSpin state
        longMin <- spinButtonGetValue $ longitudeMinSpin state
        longSec <- spinButtonGetValue $ longitudeSecSpin state
        tz <- spinButtonGetValue $ timeZoneSpin state

        let
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
            yr = fromIntegral (year state)
            leapYear = isLeapYear yr
            days = if leapYear then [1 .. 366] else [1 .. 365]
            dates = map (\(month, day) -> (YMD yr month (fromIntegral day))) $ map (dayOfYearToMonthAndDay leapYear) days

            (sunRiseSetMap, sunRisesAndSets) = getSunRisesAndSets state dates leapYear 
                
            nightGradientPlotData = getNightGradientPlotData sunRisesAndSets

            sunRiseSetPlotData = getSunRiseSetPlotData sunRisesAndSets
            
            (rises, sets) = unzip $ concat sunRisesAndSets
            minHour = 12 + (floor (minimum (map (\(set, _) -> set) sets)))
            maxHour = (ceiling (maximum (map (\(rise, _) -> rise) rises))) - 12
            
            planetsRiseSetPlotData = getPlanetsRiseSetPlotData state dates leapYear sunRiseSetMap
            planetsTransitPlotData = getPlanetsTransitPlotData state dates leapYear sunRiseSetMap
            
            area = plotArea $ plotSettings state
            scrArea = ScreenArea 0 (fromIntegral w) (fromIntegral h) 0 0 0

            hoursPlotData = concat $ map (getHourPlotData area scrArea ) ([0 .. maxHour] ++ [minHour .. 23])
            monthsPlotData = concat $ map (getMonthPlotData area scrArea leapYear) [1 .. 12]
        
            newState = 
                state {
                    plotSettings =
                        (plotSettings state) {
                            screenArea = scrArea
                        }
                }

        plot (canvas state) [(plotSettings newState, 
            --nightGradientPlotData 
            sunRiseSetPlotData 
            ++ planetsRiseSetPlotData 
            ++ planetsTransitPlotData 
            ++ hoursPlotData 
            ++ monthsPlotData
            )] Nothing 
        modifyMVar_ stateRef $ \state -> return newState

getNightGradientPlotData :: [[((Double, Double), (Double, Double))]] -> [PlotData] 
getNightGradientPlotData sunRisesAndSets = 
    let
        risesAndSetsForGradient = map (\riseSets -> zip (init riseSets) (tail riseSets)) sunRisesAndSets

        nightGradientPlotData = concat $
            map (\risesAndSets -> concat $
                map (\(((rise1, day1), (set1, _)), ((rise2, day2), (set2, _))) ->
                        [PlotPolygon {
                            plotPolygon = Polygon {
                                polygonVertices = [(rise1, day1), ((set1 + rise1) / 2, day1), ((set1 + rise1) / 2, day2), (rise2, day2)]
                            },
                            plotPolygonPattern = LinearPattern {
                                linearPatternStart = (rise1, (day1 + day2) / 2),
                                linearPatternEnd = ((set1 + rise1) / 2, (day1 + day2) / 2),
                                patternColorStops = [ColorStop 0 (0.75, 0.75, 1, 1), ColorStop 1 (0.25, 0.25, 1, 1)]
                            }
                        },
                        PlotPolygon {
                            plotPolygon = Polygon {
                                polygonVertices = [(set1, day1), ((set1 + rise1) / 2, day1), ((set1 + rise1) / 2, day2), (set2, day2)]
                            },
                            plotPolygonPattern = LinearPattern {
                                linearPatternStart = (set1, (day1 + day2) / 2),
                                linearPatternEnd = ((set1 + rise1) / 2, (day1 + day2) / 2),
                                patternColorStops = [ColorStop 0 (0.75, 0.75, 1, 1), ColorStop 1 (0.25, 0.25, 1, 1)]
                            }
                        }]
                    ) risesAndSets
                    
            ) risesAndSetsForGradient
    
    in
        nightGradientPlotData
        
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
        boxWidth = boxHeight --if hour >= 10 then 2 * boxHeight else boxHeight
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

getMonthPlotData :: PlotArea -> ScreenArea -> Bool -> Int -> [PlotData]
getMonthPlotData plotArea scrArea leapYear month =
    let
        numDays = monthLength leapYear month
        plotWidth = plotAreaRight plotArea - plotAreaLeft plotArea 
        plotHeight = plotAreaTop plotArea - plotAreaBottom plotArea 
        plotCoef = plotHeight / plotWidth
        scrWidth = screenAreaRight scrArea - screenAreaLeft scrArea 
        scrHeight = screenAreaBottom scrArea - screenAreaTop scrArea 
        scrCoef = scrHeight / scrWidth
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
getPlanetsRiseSet state dates leapYear planets =
    map (\(planetName, planet) ->
            (planetName, groupRiseSets $ convertToLT state $ map (\date -> (date, calcPlanetRiseSet date planet earth2000 (latitude state) (longitude state))) dates)
        ) planets

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
                    (alt, azi) = equToHor lha dec lat
                    Rad altRad = toRad alt
                    polarNight = altRad < 0
                in
                    trace ("polarNight=" ++ show polarNight) polarNight
            
convertToPlotCoords :: Bool -> (Date, Hours) -> (Double, Double)      
convertToPlotCoords leapYear (date, riseOrSet) = 
    let
        YMD y m d = toYMD date 
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
--        plotDataPointAttributes = Just PlotPointAttributes {
--            plotPointType = Point, 
--            plotPointSize = 1,
--            plotPointColor = getPlanetColor planetName
--        },
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
        groupFn (riseSet:riseSets) arr =
            case riseSet of
                (date, Just riseSet) -> groupFn riseSets (init arr ++ [last arr ++ [(date, riseSet)]])
                (date, Nothing) -> groupFn riseSets (arr ++ [])
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

addWidgetToVBox :: (WidgetClass w) => Maybe String -> w -> VBox -> IO ()
addWidgetToVBox maybeName w vBox = do
    
    hBox <- hBoxNew True 0
    case maybeName of 
        Just name ->
            do
                label <- labelNew maybeName
                boxPackStart hBox label PackNatural 2
        Nothing -> return ()
    boxPackEnd hBox w PackNatural 2
    boxPackStart vBox hBox PackNatural 2

