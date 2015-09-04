
module GUI.Plot (
    PlotArea (..),
    ScreenArea (..),
    PlotData (..),
    PlotSettings (..),
    PlotPointAttributes (..),
    PlotLineAttributes (..),
    PlotPointType(..),
    PlotSelection(..),
    PlotSegments(..),
    GUI.Plot.Rectangle(..),
    Coord (..),
    Polygon (..),
    Pattern (..),
    ColorStop (..),
    Font (..),
    FontSlant (..),
    FontWeight (..),
    PlotTool (..),
    toScreenCoords,
    toGraphCoords,
    onKeyDown,
    onMouseScroll,
    onMouseButton,
    onMouseMove,
    plot
    ) where

import Graphics.UI.Gtk as Gtk hiding (addWidget, Plus, Cross, Circle, Font, rectangle)
import Graphics.Rendering.Cairo hiding (Pattern, FontSlant, FontWeight)
import Graphics.Rendering.Cairo.Matrix as M hiding (rotate)
import Graphics.UI.Gtk.Gdk.Events hiding (Rectangle)

import Debug.Trace

import Utils.Math
import Utils.Misc

import Data.List
import Data.Char
import Data.Maybe
import Data.Tuple
import qualified Data.Vector.Unboxed as V
import Control.Monad

data PlotPointType = Point | Plus | Cross | PlusCross | Square | FilledSquare | Circle | FilledCircle | 
    Triangle | FilledTriangle | DownTriangle  | FilledDownTriangle | Diamond | FilledDiamond |
    Pentagon | FilledPentagon |
    Impulse deriving (Show, Read, Eq)

data PlotArea = PlotArea {
    plotAreaLeft :: Double,
    plotAreaRight :: Double,
    plotAreaBottom :: Double,
    plotAreaTop :: Double,
    plotAreaBack :: Double,
    plotAreaFront :: Double 
} deriving (Show, Read)

data ScreenArea = ScreenArea {
    screenAreaLeft :: Double,
    screenAreaRight :: Double,
    screenAreaBottom :: Double,
    screenAreaTop :: Double,
    screenAreaBack :: Double,
    screenAreaFront :: Double 
} deriving (Show, Read)

data PlotPointAttributes = 
    PlotPointAttributes {
        plotPointType :: PlotPointType, 
        plotPointSize :: Double,
        plotPointColor :: (Double, Double, Double, Double) -- RGBA
    } deriving (Show, Read)
    
data PlotLineAttributes = 
    PlotLineAttributes {
        plotLineDash :: [Double], -- ^ a list specifying alternate lengths of on and off portions of the stroke
        plotLineWidth :: Double, 
        plotLineColor :: (Double, Double, Double, Double) -- RGBA
    } deriving (Show, Read)

data PlotData = 
    PlotData {
        plotDataValues :: V.Vector ((Double {-x-}, Double {-error-}), (Double {-y-}, Double {-error-})),
        plotDataPointAttributes :: Maybe PlotPointAttributes,
        plotDataLineAttributes :: Maybe PlotLineAttributes
    } |
    PlotData3d {
        plotDataValues3d :: V.Vector (Double {-x-}, Double {-y-}, Double {-z-})
    } |
    PlotVectors {
        plotVectors :: V.Vector ((Double {-x1-}, Double {-y1-}, Double {-z1-}), (Double {-x2-}, Double {-y2-}, Double {-z2-})),
        plotVectorLineAttributes :: PlotLineAttributes,
        plotVectorStartStyle :: Int, -- ^ 0: none, 1: arrow
        plotVectorEndStyle :: Int -- ^ 0: none, 1: arrow
    } |
    PlotRectangle {
        plotRectangleLeft :: Double,
        plotRectangleRight :: Double,
        plotRectangleBottom :: Double,
        plotRectangleTop :: Double,
        plotRectangleLineAttributes :: PlotLineAttributes,
        plotRectangleFillColor :: (Double, Double, Double, Double)
    } |
    PlotPolygon {
        plotPolygon :: Polygon,
        plotPolygonPattern :: Pattern
    } |
    PlotText {
        plotText :: String,
        plotTextFont :: Font,
        plotTextPos :: (Double, Double),
        plotTextAngle :: Double, -- angle in degrees
        plotTextSize :: (Maybe Coord, Maybe Coord), -- width and height
        plotTextLineAttributes :: Maybe PlotLineAttributes,
        plotTextFillColor :: (Double, Double, Double, Double)
    } deriving (Show, Read)

data Coord = PlotCoord Double | ScreenCoord Double deriving (Show, Read)

data Polygon = Polygon {
    polygonVertices :: [(Double, Double)]
} deriving (Show, Read)

data Rectangle = Rectangle {
    rectangleLeft :: Double,
    rectangleRight :: Double,
    rectangleBottom :: Double,
    rectangleTop :: Double
} deriving (Show, Read)

data Pattern = 
    LinearPattern {
        linearPatternStart :: (Double, Double),
        linearPatternEnd :: (Double, Double),
        patternColorStops :: [ColorStop]
    } |
    RadialPattern {
        radialPpatternStart :: (Double, Double, Double), -- x, y and r
        radialPatternEnd :: (Double, Double, Double), -- x, y and r
        patternColorStops :: [ColorStop]
    } deriving (Show, Read)

data ColorStop = ColorStop {
    colorStopOffset :: Double,
    colorStopColor :: (Double, Double, Double, Double)
} deriving (Show, Read)

data Font = Font {
    fontFace :: String,
    fontSlant :: FontSlant,
    fontWeight :: FontWeight,
    fontSize :: Double
} deriving (Show, Read)

data FontSlant = FontSlantNormal | FontSlantItalic | FontSlantOblique deriving (Show, Read)

data FontWeight = FontWeightNormal | FontWeightBold deriving (Show, Read)

data PlotSelection = PlotSelection {
    plotSelectionRectangle :: Maybe GUI.Plot.Rectangle,
    plotSelectionLineAttributes :: PlotLineAttributes
        
} deriving (Show, Read)

data PlotSegments = PlotSegments {
    plotSegmentsData :: [Double],
    plotSegmentsLineAttributes :: PlotLineAttributes
        
} deriving (Show, Read)

data PlotTool = PlotToolSelect | PlotToolSegment deriving (Show, Read)

data PlotSettings = PlotSettings {
    plotArea :: PlotArea,
    screenArea :: ScreenArea,
    plotMinorXUnit :: Either Bool Double,
    plotMinorYUnit :: Either Bool Double,
    plotMajorXUnit :: Either Bool Double,
    plotMajorYUnit :: Either Bool Double,
    plotBackground :: (Double, Double, Double),
    mousePos :: Maybe (Double, Double),
    plotSelection :: Maybe PlotSelection,
    plotSegments :: Maybe PlotSegments,
    plotTool :: PlotTool
    
} deriving (Show, Read)

plot :: DrawingArea -> [(PlotSettings, [PlotData])] -> Maybe String-> IO ()
plot canvas settings maybeFileName = 
    do
        Just win <- widgetGetWindow canvas
        w <- widgetGetAllocatedWidth canvas
        h <- widgetGetAllocatedHeight canvas
        
        case maybeFileName of
            Nothing ->
                do
                    surface <- createImageSurface FormatRGB24 w h
                    renderWith surface (mapM_ (\(plotSettings, plotData) -> plot' plotSettings plotData) settings)
                    surfaceFlush surface
                    surfaceFinish surface
                                        
                    drawWindowBeginPaintRect win (Gtk.Rectangle 0 0 w h)
--                    renderWithDrawWindow win $ do
--                        setSourceSurface surface 0 0
--                        setOperator OperatorSource
--                        paint
                    renderWithDrawWindow win $
                        mapM_ (\(plotSettings, plotData) -> plot' plotSettings plotData) settings
                    drawWindowEndPaint win
            Just fileName -> 
                do
                    withPDFSurface (if ".pdf" `isSuffixOf` (map toLower fileName) then fileName else fileName ++ ".pdf") (fromIntegral w) (fromIntegral h)
                        (flip renderWith (mapM_ (\(plotSettings, plotData) -> plot' plotSettings plotData) settings)) 
                        
plot' :: PlotSettings -> [PlotData] -> Render ()
plot' plotSettings plotData =
    do
        let
            scrArea = screenArea plotSettings
            left = screenAreaLeft scrArea
            top = screenAreaTop scrArea
            right = screenAreaRight scrArea
            bottom = screenAreaBottom scrArea
            (r, g, b) = plotBackground plotSettings
        
        setSourceRGB r g b    
        rectangle left top right bottom
        fill
        mapM_ (drawData plotSettings) plotData
        drawUnits plotSettings (getUnits plotSettings)
        drawSelection plotSettings
        drawSegments plotSettings
    
drawSelection :: PlotSettings -> Render ()
drawSelection plotSettings =
    case plotSelection plotSettings of
        Nothing -> return ()
        Just sel ->
            case plotSelectionRectangle sel of
                Nothing -> return ()
                Just rect ->
                    do
                        let
                            scrArea = screenArea plotSettings
                            lineAttributes = plotSelectionLineAttributes sel
                            (r, g, b, a) = plotLineColor lineAttributes
                            (x1, y1, _) = toScreenCoords scrArea (plotArea plotSettings) (rectangleLeft rect, rectangleBottom rect, 0)
                            (x2, y2, _) = toScreenCoords scrArea (plotArea plotSettings) (rectangleRight rect, rectangleTop rect, 0)
                            x = min x1 x2
                            dx = abs (x2 - x1)
                            y = min y1 y2
                            dy = abs (y2 - y1)
                        setSourceRGBA r g b a
                        setLineWidth $ plotLineWidth lineAttributes
                        setLineJoin LineJoinRound
                        setDash (plotLineDash lineAttributes) 0
                        rectangle x y dx dy
                        stroke
drawSegments :: PlotSettings -> Render ()
drawSegments plotSettings = 
    case plotSegments plotSettings of
        Nothing -> return ()
        Just segments ->
            do
                let
                    scrArea = screenArea plotSettings
                    lineAttributes = plotSegmentsLineAttributes segments
                    (r, g, b, a) = plotLineColor lineAttributes
                setSourceRGBA r g b a
                setLineWidth $ plotLineWidth lineAttributes
                setLineJoin LineJoinRound
                setDash (plotLineDash lineAttributes) 0

                mapM_ (\x ->
                    do
                        let
                            (x1, _, _) = toScreenCoords scrArea (plotArea plotSettings) (x, 0, 0)
                        moveTo x1 (screenAreaBottom scrArea)
                        lineTo x1 (screenAreaTop scrArea)

                    ) (plotSegmentsData segments)
                stroke
drawData :: PlotSettings -> PlotData -> Render ()
drawData plotSettings plotData = 
    do

        let
            scrArea = screenArea plotSettings
        
            formattedArea = formatScreenArea scrArea
            left = screenAreaLeft formattedArea
            right = screenAreaRight formattedArea
            top = screenAreaTop formattedArea
            bottom = screenAreaBottom formattedArea
            back = screenAreaBack formattedArea
            front = screenAreaFront formattedArea
            width = right - left
            height = bottom - top
            depth = front - back
            xSpace = (right - left) / 20
            ySpace = (top - bottom) / 20 -- Note that ySpace is negative
            zSpace = (front - back) / 20
            
            leftPlusSpace = left + xSpace
            rightMinusSpace = right - xSpace
            topMinusSpace = top - ySpace
            bottomPlusSpace = bottom + ySpace
            backPlusSpace = back + zSpace
            frontMinusSpace = front - zSpace
            
            calcIntersections i points = 
                if (i == 0) then points V.! i `V.cons` calcIntersections (i + 1) points
                else if (i >= V.length points) 
                    then V.empty
                    else
                        let
                            p1@((x, _), (y, _)) = points V.! i 
                            ((x1, wx1), (y1, wy1)) = points V.! (i - 1)
                            calcIntersection (coords1@(_, _), coords2@(_, _), boundaryCoord) =
                                let
                                    (coord11, coord12) = maximumBy (\(coord1, _) (coord2, _) -> compare coord1 coord2) [coords1, coords2]
                                    (coord21, coord22) = minimumBy (\(coord1, _) (coord2, _) -> compare coord1 coord2) [coords1, coords2]
                                in
                                    if coord11 > boundaryCoord && coord21 < boundaryCoord
                                        then Just (boundaryCoord, coord22 + (coord12 - coord22) * (boundaryCoord - coord21) / (coord11 - coord21))
                                        else Nothing
                            xIntersections = catMaybes $ map (calcIntersection) [
                                ((x, y), (x1, y1), leftPlusSpace), 
                                ((x, y), (x1, y1), rightMinusSpace)]
                            yIntersections = map swap $ catMaybes $ map (calcIntersection) [
                                ((y, x), (y1, x1), topMinusSpace), 
                                ((y, x), (y1, x1), bottomPlusSpace)]
                            -- TODO: correct interpolation of errors
                            intersections = map (\(x, y) -> ((x, wx1), (y, wy1))) $ sortBy (\(x1, _) (x2, _) -> compare x1 x2) $ xIntersections ++ yIntersections
                        in
                            if x < leftPlusSpace && x1 < leftPlusSpace || x > rightMinusSpace && x1 > rightMinusSpace 
                                then 
                                    calcIntersections (i + 1) points
                                else if intersections /= [] then 
                                    ((V.fromList intersections) `V.snoc` p1) V.++ calcIntersections (i + 1) points 
                                else
                                    p1 `V.cons` calcIntersections (i + 1) points
            
            dataToScreen d@(PlotData _ _ _) =
                V.filter (\((x, _), (y, _)) -> x >= leftPlusSpace && x < rightMinusSpace && y >= topMinusSpace && y < bottomPlusSpace) $
                    toScreenCoordss_ formattedArea (plotArea plotSettings) (plotDataValues d)
            dataLinesToScreen d@(PlotData _ _ _) =
                -- Additional terms xSpace / 1000 and ySpace / 1000 are workaround for rounding errors
                fst $ Utils.Misc.segmentVector (\((x, _), (y, _)) -> x >= leftPlusSpace - xSpace / 1000 && x < rightMinusSpace + xSpace / 1000 && y >= topMinusSpace + ySpace / 1000 && y < bottomPlusSpace - ySpace / 1000) $
                    calcIntersections 0 $
                    toScreenCoordss_ formattedArea (plotArea plotSettings) (plotDataValues d)
            dataToScreen3d d@(PlotData3d _) =
                V.filter (\(x1, x2, z) -> x1 >= leftPlusSpace && x1 < rightMinusSpace && x2 >= topMinusSpace && x2 < bottomPlusSpace  && z >= backPlusSpace && z < frontMinusSpace) $
                    V.map (\(x1, x2, z) -> 
                        let
                            ((screenX1, _) , (screenX2, _), screenZ) = toScreenCoords1 formattedArea (plotArea plotSettings) ((x1, 0), (x2, 0), z)
                        in (screenX1, screenX2, screenZ)
                        ) $ plotDataValues3d d
            
        case plotData of
            PlotData _ _ _ ->
                do
                    let
                        maybePointAttributes = plotDataPointAttributes plotData
                        maybeLineAttributes = plotDataLineAttributes plotData
                        dataPoints :: V.Vector ((Double, Double), (Double, Double)) = dataToScreen plotData
                        dataLines :: [V.Vector ((Double, Double), (Double, Double))] = dataLinesToScreen plotData
                    case maybePointAttributes of
                        Just pointAttributes ->
                            do
                                let
                                    ptSize = (plotPointSize pointAttributes) * (min width height) / 200
                                    (r, g, b, a) = plotPointColor pointAttributes
                                setSourceRGBA r g b a
                                case maybeLineAttributes of
                                    Just lineAttributes ->
                                        if (plotLineWidth lineAttributes) > 0
                                        then setLineWidth (plotLineWidth lineAttributes)
                                        else setLineWidth 1
                                    otherwise -> setLineWidth 1 
                                setLineJoin LineJoinMiter
                                
                                V.mapM_ (\(x, y) -> drawDataSymbol (left, top, right, bottom) (x, y) ptSize (plotPointType pointAttributes)) dataPoints
                                stroke
                        otherwise ->
                            return ()
                    case maybeLineAttributes of
                        Just lineAttributes ->
                            if all (== 0) (plotLineDash lineAttributes) || plotLineWidth lineAttributes == 0
                            then return ()
                            else
                                do
                                    let
                                        (r, g, b, a) = plotLineColor lineAttributes
                                    setLineWidth $ plotLineWidth lineAttributes
                                    setLineJoin LineJoinRound
                                    setDash (plotLineDash lineAttributes) 0
                                    --drawDataLines' (trace ("dataLines: " ++ show dataLines) dataLines) (r, g, b, a)
                                    drawDataLines' dataLines (r, g, b, a)
                        otherwise ->
                            return ()
            PlotData3d _ ->
                draw3dData (dataToScreen3d plotData)
            PlotVectors _ _ _ _ ->
                do
                    let 
                        lineAttributes = plotVectorLineAttributes plotData
                        (r, g, b, a) = plotLineColor lineAttributes
                        screenVectors = 
                            V.filter (\((x1, y1, z1), (x2, y2, z2)) -> x1 >= left + xSpace && x1 < right - xSpace && y1 >= top - ySpace && y1 < bottom + ySpace &&
                                x2 >= left + xSpace && x2 < right - xSpace && y2 >= top - ySpace && y2 < bottom + ySpace) $
                            V.map (\((x1, y1, z1), (x2, y2, z2)) -> 
                            (toScreenCoords scrArea (plotArea plotSettings) (x1, y1, z1), toScreenCoords scrArea (plotArea plotSettings) (x2, y2, z2))) $ 
                            plotVectors plotData 
                    setSourceRGBA r g b a
                    setLineWidth $ plotLineWidth lineAttributes
                    setLineJoin LineJoinRound
                    setDash (plotLineDash lineAttributes) 0
                    drawVectors screenVectors (plotVectorStartStyle plotData) (plotVectorEndStyle plotData)
            PlotRectangle _ _ _ _ _ (rf, gf, bf, af) ->
                do
                    let
                        lineAttributes = plotRectangleLineAttributes plotData
                        (r, g, b, a) = plotLineColor lineAttributes
                        (x1, y1, _) = toScreenCoords scrArea (plotArea plotSettings) (plotRectangleLeft plotData, plotRectangleBottom plotData, 0)
                        (x2, y2, _) = toScreenCoords scrArea (plotArea plotSettings) (plotRectangleRight plotData, plotRectangleTop plotData, 0)
                        x = min x1 x2
                        dx = abs (x2 - x1)
                        y = min y1 y2
                        dy = abs (y2 - y1)
                    rectangle x y dx dy
                    setSourceRGBA rf gf bf af
                    fillPreserve
                    setSourceRGBA r g b a
                    setLineWidth $ plotLineWidth lineAttributes
                    setLineJoin LineJoinRound
                    setDash (plotLineDash lineAttributes) 0
                    stroke
            PlotPolygon polygon pattern ->
                do
                    let
                        colorStops = patternColorStops pattern
                        patternFunc pat = do
                            mapM_ (\(ColorStop offset (r, g, b, a)) ->
                                    patternAddColorStopRGBA pat offset r g b a
                                ) colorStops 
                            setSource pat
                            newPath
                            let
                                (x, y) = head $ polygonVertices polygon
                                (scrX, scrY, _) = toScreenCoords scrArea (plotArea plotSettings) (x, y, 0)
                            moveTo scrX scrY
                            mapM_ (\(x, y) -> do
                                let
                                    (scrX, scrY, _) = toScreenCoords scrArea (plotArea plotSettings) (x, y, 0)
                                lineTo scrX scrY
                                ) (tail (polygonVertices polygon))
                            closePath
                            fill
                            stroke
                            
                    case pattern of 
                        LinearPattern (x0, y0) (x1, y1) _ ->
                            do
                                let
                                    (scrX0, scrY0, _) = toScreenCoords scrArea (plotArea plotSettings) (x0, y0, 0)
                                    (scrX1, scrY1, _) = toScreenCoords scrArea (plotArea plotSettings) (x1, y1, 0)
                                withLinearPattern scrX0 scrY0 scrX1 scrY1 patternFunc
                        RadialPattern (x0, y0, r0) (x1, y1, r1) _ ->
                            do
                                let
                                        (scrX0, scrY0, _) = toScreenCoords scrArea (plotArea plotSettings) (x0, y0, 0)
                                        (scrX1, scrY1, _) = toScreenCoords scrArea (plotArea plotSettings) (x1, y1, 0)
                                        -- here we assume x and y axis have the equal units
                                        (xZero, _, _) = toScreenCoords scrArea (plotArea plotSettings) (0, 0, 0)
                                        (xR0, _, _) = toScreenCoords scrArea (plotArea plotSettings) (r0, 0, 0)
                                        (xR1, _, _) = toScreenCoords scrArea (plotArea plotSettings) (r1, 0, 0)
                                        scrR0 = abs $ xZero - xR0
                                        scrR1 = abs $ xZero - xR1
                                withRadialPattern scrX0 scrY0 scrR0 scrX1 scrY1 scrR1 patternFunc
            PlotText text (Font fontFace fontSlant fontWeight fontSize) (x, y) textAngle textSize maybeLineAttributes (r, g, b, a) -> do
                let
                    (scrX0, scrY0, _) = toScreenCoords scrArea (plotArea plotSettings) (x, y, 0)
                    toCairoFontSlant GUI.Plot.FontSlantNormal = Graphics.Rendering.Cairo.FontSlantNormal
                    toCairoFontSlant GUI.Plot.FontSlantItalic = Graphics.Rendering.Cairo.FontSlantItalic
                    toCairoFontSlant GUI.Plot.FontSlantOblique = Graphics.Rendering.Cairo.FontSlantOblique
                    toCairoFontWeight GUI.Plot.FontWeightNormal = Graphics.Rendering.Cairo.FontWeightNormal
                    toCairoFontWeight GUI.Plot.FontWeightBold = Graphics.Rendering.Cairo.FontWeightBold
                moveTo scrX0 scrY0
                save
                let
                    textAngleRad = textAngle * pi / 180.0
                selectFontFace fontFace (toCairoFontSlant fontSlant) (toCairoFontWeight fontWeight)
                determineFontSize plotSettings text fontSize textSize textAngleRad
                rotate textAngleRad
                textPath text
                setSourceRGBA r g b a
                fillPreserve
                case maybeLineAttributes of 
                    Just (PlotLineAttributes lineDash lineWidth (r, g, b, a)) -> do
                        setSourceRGBA r g b a
                        setLineWidth lineWidth
                        setDash lineDash 0
                    Nothing -> return ()
                stroke
                restore

determineFontSize plotSettings text fontSize (tw, th) angle = do
    let
        --area = plotArea plotSettings
        --plotWidth = plotAreaRight area - plotAreaLeft area
        --plotHeight = plotAreaTop area - plotAreaBottom area
        --coef = plotHeight / plotWidth
        scrArea = screenArea plotSettings
        scrWidth = screenAreaRight scrArea - screenAreaLeft scrArea 
        scrHeight = screenAreaBottom scrArea - screenAreaTop scrArea 
        precision = min (scrWidth / 10000) (scrHeight / 10000)
        (checkWidth, textWidth) = case tw of
            Just tw -> (True, tw)
            Nothing -> (False, ScreenCoord 0)
        (checkHeight, textHeight) = case th of
            Just th-> (True, th)
            Nothing -> (False, ScreenCoord 0)
        (xZero, yZero, _) = toScreenCoords (screenArea plotSettings) (plotArea plotSettings) (0, 0, 0)
        scrTextWidth = case textWidth of
            PlotCoord textWidth ->
                let
                    (width, _, _) = toScreenCoords (screenArea plotSettings) (plotArea plotSettings) (textWidth, 0, 0)
                in
                    abs $ xZero - width
            ScreenCoord textWidth -> textWidth
        scrTextHeight = case textHeight of
            PlotCoord textHeight ->
                let
                    (_, height, _) = toScreenCoords (screenArea plotSettings) (plotArea plotSettings) (0, textHeight, 0)
                in
                    abs $ yZero - height
            ScreenCoord textHeight -> textHeight
        determineFontSize' eitherLastWidthAndSizeOrNewSize eitherLastHeightAndSizeOrNewSize = do
            let
                (widthOk, newSizeX, lastWidth1, lastWidth2, lastSizeX1, lastSizeX2) = case eitherLastWidthAndSizeOrNewSize of
                    Left (lastWidth1, lastWidth2, lastSizeX1, lastSizeX2) ->
                        let
                            newSize = if not checkWidth then fontSize
                                else if max lastWidth1 lastWidth2 >= scrTextWidth && min lastWidth1 lastWidth2 <= scrTextWidth
                                    then (lastSizeX1 + lastSizeX2) / 2
                                else if min lastWidth1 lastWidth2 > scrTextWidth 
                                    then (min lastSizeX1 lastSizeX2) / 2
                                else (max lastSizeX1 lastSizeX2) * 2
                        in
                            (abs (floor newSize - floor lastSizeX1) <= 1, newSize, lastWidth1, lastWidth2, lastSizeX1, lastSizeX2)
                    Right (newSize) -> (True, newSize, 0, 0, 0, 0)
                (heightOk, newSizeY, lastHeight1, lastHeight2, lastSizeY1, lastSizeY2) = case eitherLastHeightAndSizeOrNewSize of
                    Left (lastHeight1, lastHeight2, lastSizeY1, lastSizeY2) ->
                        let
                            newSize = if not checkHeight then fontSize 
                                else if max lastHeight1 lastHeight2 >= scrTextHeight && min lastHeight1 lastHeight2 <= scrTextHeight
                                    then (lastSizeY1 + lastSizeY2) / 2
                                else if min lastHeight1 lastHeight2 > scrTextHeight 
                                    then (min lastSizeY1 lastSizeY2) / 2
                                else (max lastSizeY1 lastSizeY2) * 2
                        in
                            (abs (floor newSize - floor lastSizeY1) <= 1, newSize, lastHeight1, lastHeight2, lastSizeY1, lastSizeY2)
                    Right (newSize) -> (True, newSize, 0, 0, 0, 0)
            --if floor (trace ("lastWidth1, lastWidth2, scrTextWidth=" ++ show lastWidth1 ++ ", " ++ show lastWidth2 ++ ", " ++ show scrTextWidth) lastWidth1) == floor scrTextWidth && floor (trace ("lastHeight1=" ++ show lastHeight1) lastHeight1) == floor scrTextHeight
            --if (trace ("newSizeX, lastSizeX1, lastSizeX2=" ++ show newSizeX ++ ", " ++ show lastSizeX1 ++ ", " ++ show lastSizeX2 ++ "\nnewSizeY, lastSizeY1, lastSizey2=" ++ show newSizeY ++ ", " ++ show lastSizeY1 ++ ", " ++ show lastSizeY2) (widthOk && heightOk)) 
            if widthOk && heightOk 
                then return ()
                else do
                    setFontMatrix $ M.scale newSizeX newSizeY M.identity
                    (TextExtents xb yb w h _ _) <- textExtents text
                    let
                        (lastWidth, lastSizeX) = if w < scrTextWidth then (max lastWidth1 lastWidth2, max lastSizeX1 lastSizeX2) else (min lastWidth1 lastWidth2, min lastSizeX1 lastSizeX2)
                        (lastHeight, lastSizeY) = if h < scrTextHeight then (max lastHeight1 lastHeight2, max lastSizeY1 lastSizeY2) else (min lastHeight1 lastHeight2, min lastSizeY1 lastSizeY2)
                    --determineFontSize' (if widthOk then (Right newSizeX) else Left ((trace ("w,h=" ++ show w ++ ", " ++ show h) w), lastWidth, newSizeX, lastSizeX)) (if heightOk then (Right newSizeY) else Left ((trace ("w,h=" ++ show w ++ ", " ++ show h) h), lastHeight, newSizeY, lastSizeY))
                    determineFontSize' (if widthOk then (Right newSizeX) else Left (w, lastWidth, newSizeX, lastSizeX)) (if heightOk then (Right newSizeY) else Left (h, lastHeight, newSizeY, lastSizeY))
    setFontMatrix $ M.scale fontSize fontSize M.identity
    (TextExtents xb yb w h _ _) <- textExtents text
    determineFontSize' (Left (w, w, fontSize, fontSize)) (Left (h, h, fontSize, fontSize))
                                 
drawVectors :: V.Vector ((Double, Double, Double), (Double, Double, Double)) -> Int -> Int -> Render ()
drawVectors vectors startStyle ensStyle = 
    do
        let
            drawVector ((x1, y1, z1), (x2, y2, z2)) = 
                do
                    moveTo x1 y1
                    lineTo x2 y2
                    case ensStyle of
                        1 ->
                            do
                                let
                                    l = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
                                return ()
                        otherwise ->
                            return ()
        V.mapM_ drawVector vectors
        stroke

draw3dData :: V.Vector (Double, Double, Double) -> Render ()
draw3dData xyz =
    do
        let 
            (x1s, x2s, ys) = V.unzip3 xyz

            (xMin1, xMin2) = (V.minimum x1s, V.minimum x2s)
            (xMax1, xMax2) = (V.maximum x1s, V.maximum x2s)
            numx1 = V.length $ nubVector x1s
            numx2 = V.length $ nubVector x2s
            yMin = V.minimum ys
            yRange = V.maximum ys - yMin

            stepx1 = (xMax1 - xMin1) / fromIntegral numx1
            stepx2 = (xMax2 - xMin2) / fromIntegral numx2
        V.mapM_ (\(x1, x2, y) ->
                do
                    let 
                        (r, g, b) = getColor y yMin yRange
                        
                    setSourceRGB r g b
                    (prevX1, prevX2) <- getCurrentPoint
                    if (abs (prevX1 - x1) >= 1 || abs (prevX2 - x2) >= 1)
                        then
                            do
                                moveTo x1 x2
                                rectangle (x1 - stepx1 / 2 - 1) (x2 - stepx2 / 2 - 1) (stepx1 + 1) (stepx2 + 1)
                                if (stepx1 > 0 && stepx2 > 0)
                                    then
                                        do
                                            Graphics.Rendering.Cairo.fill
                                    else
                                        return ()  
                                stroke
                        else
                            return ()
            ) xyz 

getColor :: Double -> Double -> Double -> (Double, Double, Double)
getColor y yMin yRange =
    let
        color = (y - yMin) / yRange
        r = 
            if color <= 0.5 
                then 1.5 * color 
                else 0.75 
        g = 2 * (max 0 (color - 0.5))
        b = 2 * (max 0 (0.5 - color))
    in (r, g, b)



drawDataSymbol :: (Double, Double, Double, Double) -> ((Double, Double), (Double, Double)) -> Double -> PlotPointType -> Render ()
drawDataSymbol (left, top, right, bottom) ((x, xErr), (y, yErr)) size symbol =
    do
        let
            drawPlus =
                do
                    moveTo (x - size) y
                    lineTo (x + size) y
                    moveTo x (y - size)
                    lineTo x (y + size)
            drawCross =
                do
                    moveTo (x - size) (y - size)
                    lineTo (x + size) (y + size)
                    moveTo (x - size) (y + size)
                    lineTo (x + size) (y - size)
            drawTriangle =
                do
                    moveTo (x - size) (y + (size / sqrt(3)))
                    lineTo x (y - (size / sqrt(3)))
                    lineTo (x + size) (y + (size / sqrt(3)))
                    lineTo (x - size) (y + (size / sqrt(3)))
            drawDownTriangle =
                do
                    moveTo (x - size) (y - (size / sqrt(3)))
                    lineTo x (y + (size / sqrt(3)))
                    lineTo (x + size) (y - (size / sqrt(3)))
                    lineTo (x - size) (y - (size / sqrt(3)))
            drawDiamond =
                do
                    moveTo (x - size) y
                    lineTo x (y + size)
                    lineTo (x + size) y
                    lineTo x (y - size)
                    lineTo (x - size) y
            drawPentagon =
                do
                    let
                        h = size * (sqrt(5) - 1) / 4
                        a = size * sqrt((5 + sqrt(5)) / 2) / 2
                        s = size * sqrt((5 - (sqrt(5))) / 2) / 2
                        h2 = sqrt (size * size - s * s)
                    moveTo (x - a) (y - h) 
                    lineTo x (y - size)
                    lineTo (x + a) (y - h)
                    lineTo (x + s) (y + h2)
                    lineTo (x - s) (y + h2)
                    lineTo (x - a) (y - h) 
            
        case symbol of
            Point ->
                do
                    rectangle x y 1 1
            Plus -> drawPlus
            Cross -> drawCross
            PlusCross ->
                do
                    drawPlus
                    drawCross
            Square ->
                do
                    rectangle (x - size) (y - size) (size * 2) (size * 2)
            FilledSquare ->
                do
                    rectangle (x - size) (y - size) (size * 2) (size * 2)
                    fill
            Circle ->
                do
                    moveTo (x + size) y
                    arc x y size 0 (2 * pi)
            FilledCircle ->
                do
                    moveTo (x + size) y
                    arc x y size 0 (2 * pi)
                    fill
            Triangle -> drawTriangle
            FilledTriangle ->
                do 
                    drawTriangle
                    fill
            DownTriangle -> drawDownTriangle
            FilledDownTriangle ->
                do 
                    drawDownTriangle
                    fill
            Diamond -> drawDiamond
            FilledDiamond ->
                do  
                    drawDiamond
                    fill
            Pentagon -> drawPentagon
            FilledPentagon ->
                do 
                    drawPentagon
                    fill
            Impulse -> 
                do
                    moveTo x (bottom)
                    lineTo x y
        if xErr > 0
            then do
                moveTo (x - xErr) y
                lineTo (x + xErr) y
                moveTo (x - xErr) (y - size)
                lineTo (x - xErr) (y + size)
                moveTo (x + xErr) (y - size)
                lineTo (x + xErr) (y + size)
            else 
                return ()
        if yErr > 0
            then do
                moveTo x (y - yErr)
                lineTo x (y + yErr)
                moveTo (x - size) (y - yErr)
                lineTo (x + size) (y - yErr)
                moveTo (x - size) (y + yErr)
                lineTo (x + size) (y + yErr)
            else 
                return ()
        --stroke

-- | Data points are grouped by gaps to avoid horizontal connection lines
drawDataLines' :: [V.Vector ((Double, Double), (Double, Double))] -> (Double, Double, Double, Double) -> Render ()
drawDataLines' points (r, g, b, a) =
    drawDataLines points $ map (\points -> V.replicate (V.length points) (r, g, b, a)) points

drawDataLines :: [V.Vector ((Double, Double), (Double, Double))] -> [V.Vector (Double, Double, Double, Double)] -> Render ()
drawDataLines points colors = 
    do
        zipWithM_ (\points colors -> do
                let
                    drawDataLine (((x, _), (y, _)), (r, g, b, a)) =
                        do 
                            (prevX, prevY) <- getCurrentPoint
                            if (abs (prevX - x) >= 0.5 || abs (prevY - y) >= 0.5)
                                then
                                    do
                                        let 
                                            setColors = if (V.length colors > 1)
                                                then
                                                    setSourceRGBA r g b a
                                                else
                                                    return ()
                                        setColors
                                        lineTo x y
                                else
                                    return ()
                if (V.length points > 0) 
                    then 
                        do
                            let ((x, _), (y, _)) = V.head points
                            moveTo x y
                    else return ()
                if V.length colors == 1
                    then
                        do
                            let
                                (r, g, b, a) = colors V.! 0
                            setSourceRGBA r g b a
                    else
                        return ()
                V.mapM_ drawDataLine $ V.zip points colors
                if not (V.null ((V.filter (\((_, _), (_, yErr)) -> yErr > 0) points)))
                    then
                        do
                            let ((x, _), (y, yErr)) = V.head points
                            moveTo x (y - yErr)
                            V.mapM_ drawDataLine $ V.zip (V.map (\((x, _), (y, yErr)) -> ((x, 0 :: Double), (y - yErr, 0 :: Double))) points) colors
                            moveTo x (y + yErr)
                            V.mapM_ drawDataLine $ V.zip (V.map (\((x, _), (y, yErr)) -> ((x, 0 :: Double), (y + yErr, 0 :: Double))) points) colors
                    else
                        return ()
            ) points colors
        stroke

setLineSettings :: (Double, Double, Double) -> Double -> Render ()
setLineSettings (r, g, b) lineWidth =
    do
        setSourceRGB r g b
        setDash [1, 1] 0
        setLineWidth lineWidth
        setLineCap LineCapRound
        setLineJoin LineJoinRound

unitLabel (width, height) xy beforeOrAfter ((x, y, _), (screenX, screenY, _))  =
    do
        let 
            val = if xy then x else y
            label = (show $ epsilonRound val)
        TextExtents _ _ w h _ _ <- textExtents label
        if xy 
            then
                if beforeOrAfter
                    then
                        return (screenX - (w / 2), screenY - (height / 100 + h), label)
                    else
                        return (screenX - (w / 2), screenY + (height / 100 + h), label)
            else
                if beforeOrAfter
                    then
                        return (screenX - (w + width / 167), screenY + h / 2, label)
                    else
                        return (screenX + (width / 167), screenY + h / 2, label)

drawUnits :: 
             PlotSettings
             -> (
                 (Bool, Bool), -- ^ draw x and y minor units  
                 (Bool, Bool), -- ^ draw x and y major units
                 ([Double], [Double], [Double]), -- ^ x, y and z minor units
                 ([Double], [Double], [Double])  -- ^ x, y and z major units
             ) -> Render ()
drawUnits plotSettings ((drawXMinorUnits, drawYMinorUnits), (drawXMajorUnits, drawYMajorUnits), (xMinorUnits, yMinorUnits, zMinorUnits), (xMajorUnits, yMajorUnits, zMajorUnits)) = 
    do
        let
            scrArea = screenArea plotSettings
            scrLeft = screenAreaLeft (screenArea plotSettings)
            scrRight = screenAreaRight (screenArea plotSettings)
            scrTop = screenAreaTop (screenArea plotSettings)
            scrBottom = screenAreaBottom (screenArea plotSettings)
            width = scrRight - scrLeft
            height = scrBottom - scrTop
            lineWidth = ((min width height) / 320)

        setLineSettings (0, 0, 0) lineWidth
        setFontSize $ ((min (width * 3) (height * 4)) / 160)
        let 
            clip = False -- whether to clip graph boundaries to ticks
            minXUnit = if clip then minimum [(minimum xMinorUnits), (minimum xMajorUnits)] else plotAreaLeft (plotArea plotSettings)
            minYUnit = if clip then minimum [(minimum yMinorUnits), (minimum yMajorUnits)] else plotAreaBottom (plotArea plotSettings)
            maxXUnit = if clip then maximum [(maximum xMinorUnits), (maximum xMajorUnits)] else plotAreaRight (plotArea plotSettings)
            maxYUnit = if clip then maximum [(maximum yMinorUnits), (maximum yMajorUnits)] else plotAreaTop (plotArea plotSettings)
            drawUnits2 :: Bool -> [Double] -> Double -> Bool -> Render ()
            drawUnits2 xy units lineLength drawLabel =
                do
                    let 
                        -- min and max prefixes denote either lower or upper instance of x-axis and left or right instance of y-axis 
                        (minCoords, maxCoords) = 
                            if xy then
                                unzip $ map (\unit -> ((unit, minYUnit, 0), (unit, maxYUnit, 0))) units
                            else
                                unzip $ map (\unit -> ((minXUnit, unit, 0), (maxXUnit, unit, 0))) units
                        -- | Screen coords of unit line start points
                        screenMinCoords = map (toScreenCoords scrArea (plotArea plotSettings)) minCoords
                        screenMaxCoords = map (toScreenCoords scrArea (plotArea plotSettings)) maxCoords
                        -- | Screen coords of unit line end points
                        (lineEndMinCoords, lineEndMaxCoords) = 
                            if xy then
                                (map (\(x, y, _) -> (x, y - (lineLength))) screenMinCoords, map (\(x, y, _) -> (x, y + (lineLength))) screenMaxCoords) 
                            else
                                (map (\(x, y, _) -> (x + (lineLength), y)) screenMinCoords, map (\(x, y, _) -> (x - (lineLength), y)) screenMaxCoords)
                    zipWithM_ (\(x1, y1, _) (x2, y2) -> do moveTo x1 y1; lineTo x2 y2) screenMinCoords lineEndMinCoords
                    zipWithM_ (\(x1, y1, _) (x2, y2) -> do moveTo x1 y1; lineTo x2 y2) screenMaxCoords lineEndMaxCoords
                    stroke
                    if drawLabel
                        then
                            do
                                labels <- mapM (unitLabel (width, height) xy (not xy)) (zip minCoords screenMinCoords)
                                setSourceRGB 0 0 0
                                mapM_ (\(x, y, label) -> do moveTo x y; showText label) labels
                        else return ()
            -- draw ticks and units
            majorTickSize = (min (width * 3) (height * 4)) / 200
        if drawXMinorUnits then drawUnits2 True xMinorUnits (majorTickSize / 2) False else return ()
        if drawXMajorUnits then drawUnits2 True xMajorUnits majorTickSize True else return ()
        if drawYMinorUnits then drawUnits2 False yMinorUnits (majorTickSize / 2) False else return ()
        if drawYMajorUnits then drawUnits2 False yMajorUnits majorTickSize True else return ()
        let
            minZUnit = if clip then minimum [(minimum zMinorUnits), (minimum zMajorUnits)] else plotAreaBack (plotArea plotSettings)
            maxZUnit = if clip then maximum [(maximum zMinorUnits), (maximum zMajorUnits)] else plotAreaFront (plotArea plotSettings)
            (screenXMin, screenYMin, screenZMin) = toScreenCoords scrArea (plotArea plotSettings) (minXUnit, minYUnit, minZUnit)
            (screenXMax, screenYMax, screenZMax) = toScreenCoords scrArea (plotArea plotSettings) (maxXUnit, maxYUnit, maxZUnit)
        setSourceRGB 0 0 0
        -- draw axis
        if drawXMinorUnits || drawXMajorUnits 
            then
                do
                    moveTo screenXMin screenYMin
                    lineTo screenXMax screenYMin
                    moveTo screenXMin screenYMax
                    lineTo screenXMax screenYMax
            else
                return ()
        if drawYMinorUnits || drawYMajorUnits
            then
                do 
                    moveTo screenXMin screenYMin
                    lineTo screenXMin screenYMax
                    moveTo screenXMax screenYMin
                    lineTo screenXMax screenYMax
            else
                return ()
            
        stroke
        drawColorBox (scrRight - width / 10, scrTop + height / 5) (scrRight - width / 15, scrBottom - height / 5) zMinorUnits zMajorUnits lineWidth (width, height)
        stroke

drawColorBox :: (Double, Double) -> (Double, Double) -> [Double] -> [Double] -> Double -> (Double, Double) -> Render ()
drawColorBox (left, top) (right, bottom) minUnits maxUnits lineWidth (width, height) =
    if length minUnits > 0 || length maxUnits > 0 then
        do
            let
                yMin = minimum maxUnits
                yRange = maximum maxUnits - yMin
                yCoef = (top - bottom) / yRange

            mapM_ (\(y1, y2) ->
                do
                    mapM_ (\(y1', y2') ->
                        do
                            let 
                                (r, g, b) = getColor ((y1' + y2') / 2) yMin yRange
                                yScreen2 = bottom + (y2' - yMin) * yCoef
                                yScreen1 = bottom + (y1' - yMin) * yCoef
                                
                            setSourceRGB r g b
                            rectangle (left + 1) (yScreen2) (right - left - 1) (yScreen1 - yScreen2)
                            Graphics.Rendering.Cairo.fill
                        ) (zip [y1, y1 + (y2 - y1) / 10 .. y2 - (y2 - y1) / 10] [y1 + (y2 - y1) / 10, y1 + (y2 - y1) / 5 .. y2])
                    let
                        yScreen2 = bottom + (y2 - yMin) * yCoef
                    stroke

                ) $ zip (init maxUnits) (tail maxUnits)
            mapM_ (\y ->
                do
                    let 
                        yScreen = bottom + (y - yMin) * yCoef
                    setLineSettings (0, 0, 0) lineWidth
                    moveTo left yScreen; lineTo right yScreen
                    (x, y, label) <- unitLabel (width, height) False False ((0, y, 0), (right, yScreen, 0))
                    moveTo x y; showText label
                    stroke
                ) maxUnits
            rectangle left top (right - left) (bottom - top)
    else
        return ()

getUnits :: PlotSettings ->
            (
                (Bool, Bool), -- ^ draw x and y minor units  
                (Bool, Bool), -- ^ draw x and y major units
                ([Double], [Double], [Double]), -- ^ x, y and z minor units
                ([Double], [Double], [Double])  -- ^ x, y and z major units
            ) 
getUnits plotSettings = 
    let
        area = plotArea plotSettings
        (xLeft, xRight) = (plotAreaLeft area, plotAreaRight area)
        (xMinUnit, yMinUnit, zMinUnit, xMaxUnit, yMaxUnit, zMaxUnit) = 
            let
                xRange = xRight - xLeft
                yRange = plotAreaTop area - plotAreaBottom area
                zRange = plotAreaFront area - plotAreaBack area
                getMaxUnit range = 
                    let 
                        maxUnit = 10 ^^ ceiling (logBase 10 (range / 5))
                    in
                        if range / maxUnit < 2 
                            then maxUnit / 5 
                        else if range / maxUnit < 5 
                            then maxUnit / 2
                        else maxUnit where
                getMinUnit range maxUnit = 
                    if round (range / maxUnit) > 5 then
                        maxUnit / 5
                    else
                        maxUnit / 10
                xMaxUnit = getMaxUnit xRange
                yMaxUnit = getMaxUnit yRange
                zMaxUnit = if zRange > 0 then Just (getMaxUnit zRange) else Nothing
                xMinUnit = getMinUnit xRange xMaxUnit
                yMinUnit = getMinUnit yRange yMaxUnit
                zMinUnit = case zMaxUnit of
                    Just zMaxUnit -> Just (getMinUnit zRange zMaxUnit)
                    otherwise -> Nothing
            in
                (            
                    case plotMinorXUnit plotSettings of 
                        Left _ -> Just xMinUnit
                        Right mu -> Just mu,
                    case plotMinorYUnit plotSettings of 
                        Left _ -> Just yMinUnit
                        Right mu -> Just mu,
                    zMinUnit,
                    case plotMajorXUnit plotSettings of 
                        Left _ -> Just xMaxUnit
                        Right mu -> Just mu,
                    case plotMajorYUnit plotSettings of 
                        Left _ -> Just yMaxUnit
                        Right mu -> Just mu,
                    zMaxUnit
                )                  
        units :: Double -> Double -> Maybe Double -> [Double]
        units minValue maxValue unit = 
            case unit of 
                Nothing -> []
                Just unit -> [x | x <- [startValue, startValue + unit .. endValue]] where
                    startValue = unit * (fromIntegral (ceiling (minValue / unit)))
                    endValue = unit * (fromIntegral (floor (maxValue / unit)))
        drawXMinorUnits = case plotMinorXUnit plotSettings of 
            Left False -> False
            otherwise -> True
        drawYMinorUnits = case plotMinorYUnit plotSettings of 
            Left False -> False
            otherwise -> True
        drawXMajorUnits = case plotMajorXUnit plotSettings of 
            Left False -> False
            otherwise -> True
        drawYMajorUnits = case plotMajorYUnit plotSettings of 
            Left False -> False
            otherwise -> True
    in
        (
            (drawXMinorUnits, drawYMinorUnits),
            (drawXMajorUnits, drawYMajorUnits),
            (units xLeft xRight xMinUnit,
            units (plotAreaBottom area) (plotAreaTop area) yMinUnit,
            units (plotAreaBack area) (plotAreaFront area) zMinUnit),
            (units xLeft xRight xMaxUnit,
            units (plotAreaBottom area) (plotAreaTop area) yMaxUnit,
            units (plotAreaBack area) (plotAreaFront area) zMaxUnit)
        )

formatScreenArea :: ScreenArea -> ScreenArea
formatScreenArea scrArea =
    let
        left = screenAreaLeft scrArea
        top = screenAreaTop scrArea
        w = (screenAreaRight scrArea) - left
        h = (screenAreaBottom scrArea) - top
        hSpace = w / 15
        vSpace = h / 20
        left' = left + 2 * hSpace;
        top' = top + vSpace;
        w1 = w - 3 * hSpace
        h1 = h - 3 * vSpace
    in
        scrArea {
            screenAreaLeft = left', 
            screenAreaTop = top',
            screenAreaRight = left' + w1,
            screenAreaBottom = top' + h1
        }

toScreenCoords :: 
                  ScreenArea -> 
                  PlotArea ->
                  (Double, Double, Double) -> 
                  (Double, Double, Double)
toScreenCoords scrArea plotArea (x, y, z) = 
    let
        ((screenX, _), (screenY, _), screenZ) = toScreenCoords1 (formatScreenArea scrArea) plotArea ((x, 0), (y, 0), z)
    in
        (screenX, screenY, screenZ)

toScreenCoords1 :: ScreenArea ->
                   PlotArea ->
                  ((Double, Double), (Double, Double), Double) -> 
                  ((Double, Double), (Double, Double), Double)
toScreenCoords1 formattedArea plotArea xyz =
    let 
        left = screenAreaLeft formattedArea
        right = screenAreaRight formattedArea
        bottom = screenAreaBottom formattedArea
        top = screenAreaTop formattedArea
        back = screenAreaBack formattedArea
        front = screenAreaFront formattedArea
        xMin = plotAreaLeft plotArea
        xMax = plotAreaRight plotArea
        yMin = plotAreaBottom plotArea
        yMax = plotAreaTop plotArea
        zMin = plotAreaBack plotArea
        zMax = plotAreaFront plotArea
        width = right - left
        height = bottom - top
        depth = front - back
        xCoef = (width) / (xMax - xMin)
        yCoef = (height) / (yMax - yMin)
        zCoef = (depth) / (zMax - zMin)
    in 
        toScreenCoords2 (xMin, yMin, zMin) (left, bottom, back) (xCoef, yCoef, zCoef) xyz

toScreenCoords2 :: (Double, Double, Double) ->
                   (Double, Double, Double) ->
                   (Double, Double, Double) ->
                  ((Double, Double), (Double, Double), Double) -> 
                  ((Double, Double), (Double, Double), Double)
toScreenCoords2 (xMin, yMin, zMin) (left, bottom, back) (xCoef, yCoef, zCoef) ((x, xErr), (y, yErr), z) =
    let 

        screenX = left + ((x - xMin) * xCoef)
        screenY = bottom + ((yMin - y) * yCoef)
        screenZ = back + ((z - zMin) * zCoef)
    in 
        ((screenX, xErr * xCoef), (screenY, yErr * yCoef), screenZ)

toScreenCoords2_ :: (Double, Double) ->
                   (Double, Double) ->
                   (Double, Double) ->
                  ((Double, Double), (Double, Double)) -> 
                  ((Double, Double), (Double, Double))
toScreenCoords2_ (xMin, yMin) (left, bottom) (xCoef, yCoef) ((x, xErr), (y, yErr)) =
    let 

        screenX = left + ((x - xMin) * xCoef)
        screenY = bottom + ((yMin - y) * yCoef)
    in 
        ((screenX, xErr * xCoef), (screenY, yErr * yCoef))

toScreenCoordss :: ScreenArea ->
                   PlotArea ->
                  V.Vector ((Double, Double), (Double, Double), Double) -> 
                  V.Vector ((Double, Double), (Double, Double), Double)
toScreenCoordss formattedArea plotArea xyzs =
    let 
        left = screenAreaLeft formattedArea
        right = screenAreaRight formattedArea
        bottom = screenAreaBottom formattedArea
        top = screenAreaTop formattedArea
        back = screenAreaBack formattedArea
        front = screenAreaFront formattedArea
        xMin = plotAreaLeft plotArea
        xMax = plotAreaRight plotArea
        yMin = plotAreaBottom plotArea
        yMax = plotAreaTop plotArea
        zMin = plotAreaBack plotArea
        zMax = plotAreaFront plotArea
        width = right - left
        height = bottom - top
        depth = front - back
        xCoef = (width) / (xMax - xMin)
        yCoef = (height) / (yMax - yMin)
        zCoef = (depth) / (zMax - zMin)
    in 
        V.map (toScreenCoords2 (xMin, yMin, zMin) (left, bottom, back) (xCoef, yCoef, zCoef)) xyzs

toScreenCoordss_ :: ScreenArea ->
                   PlotArea ->
                  V.Vector ((Double, Double), (Double, Double)) -> 
                  V.Vector ((Double, Double), (Double, Double))
toScreenCoordss_ formattedArea plotArea xys =
    let 
        left = screenAreaLeft formattedArea
        right = screenAreaRight formattedArea
        bottom = screenAreaBottom formattedArea
        top = screenAreaTop formattedArea
        xMin = plotAreaLeft plotArea
        xMax = plotAreaRight plotArea
        yMin = plotAreaBottom plotArea
        yMax = plotAreaTop plotArea
        width = right - left
        height = bottom - top
        xCoef = (width) / (xMax - xMin)
        yCoef = (height) / (yMax - yMin)
    in 
        V.map (toScreenCoords2_ (xMin, yMin) (left, bottom) (xCoef, yCoef)) xys

toGraphCoords :: ScreenArea -> 
                   PlotArea ->
                  (Double, Double) -> 
                  (Double, Double)
toGraphCoords screenArea plotArea (x, y) =
    let 
        formattedArea = formatScreenArea screenArea
        left = screenAreaLeft formattedArea
        top = screenAreaTop formattedArea
        right = screenAreaRight formattedArea
        bottom = screenAreaBottom formattedArea
        width = right - left
        height = top - bottom
        (xMin, xMax) = (plotAreaLeft plotArea, plotAreaRight plotArea)
        yMin = plotAreaBottom plotArea
        yMax = plotAreaTop plotArea
        xCoef = (xMax - xMin) / (width)
        yCoef = (yMax - yMin) / (height)
    in 
        (xMin + ((x - left) * xCoef), 
            yMin + ((y - bottom) * yCoef))

--------------------------------------------------------------------------------
-- Event handling

onKeyDown :: String -> DrawingArea -> PlotSettings -> (PlotSettings -> IO ()) -> IO Bool
onKeyDown keyName canvas plotSettings updateFunc = do
    let 
            
        area = plotArea $ plotSettings
        left = plotAreaLeft area
        right = plotAreaRight area
        bottom = plotAreaBottom area
        top = plotAreaTop area
        xChange = (right - left) / 10
        yChange = (top - bottom) / 10
        newArea =
            case keyName of
                "KP_Add" -> area {plotAreaLeft = left + xChange, plotAreaRight = right - xChange}
                "KP_Subtract" -> area {plotAreaLeft = left - xChange, plotAreaRight = right + xChange}
                "KP_Multiply" -> area {plotAreaBottom = bottom + yChange, plotAreaTop = top - yChange}
                "KP_Divide" -> area {plotAreaBottom = bottom - yChange, plotAreaTop = top + yChange}
                "Right" -> area {plotAreaLeft = left + xChange, plotAreaRight = right + xChange}
                "Left" -> area {plotAreaLeft = left - xChange, plotAreaRight = right - xChange}
                "Up" -> area {plotAreaBottom = bottom + yChange, plotAreaTop = top + yChange}
                "Down" -> area {plotAreaBottom = bottom - yChange, plotAreaTop = top - yChange}
                _ -> area
        areaChanged = 
            plotAreaLeft newArea /= left || 
            plotAreaRight newArea /= right ||
            plotAreaTop newArea /= top ||
            plotAreaBottom newArea /= bottom
        
--    case keyName of 
--        "f" -> windowFullscreen $ window state
--        "u" -> windowUnfullscreen $ window state
--        _ -> return ()
    updateFunc $ plotSettings {plotArea = newArea}
    return True

onMouseScroll :: (Double, Double) -> ScrollDirection -> PlotSettings -> (PlotSettings -> IO ()) -> IO Bool
onMouseScroll (x, y) direction plotSettings updateFunc = do
    let
        area = plotArea $ plotSettings
        left = plotAreaLeft area
        right = plotAreaRight area
        bottom = plotAreaBottom area
        top = plotAreaTop area
        scrArea = screenArea plotSettings
        w = screenAreaRight scrArea - screenAreaLeft scrArea
        h = screenAreaBottom scrArea - screenAreaTop scrArea
        xCenter = (right + left) / 2
        yCenter = (top + bottom) / 2
        xNewCenter = left + x * (right - left) / w
        yNewCenter = bottom + (h - y) * (top - bottom) / h
        xShift = xNewCenter - xCenter
        yShift = yNewCenter - yCenter
        xChangeLeft = (right - left + 2 * xShift) / 10
        xChangeRight = (right - left - 2 * xShift) / 10
        yChangeBottom = (top - bottom + 2 * yShift) / 10
        yChangeTop = (top - bottom - 2 * yShift) / 10
        newArea =
            case direction of
                ScrollUp -> area {plotAreaLeft = left + xChangeLeft, plotAreaRight = right - xChangeRight, plotAreaBottom = bottom + yChangeBottom, plotAreaTop = top - yChangeTop}
                ScrollDown -> area {plotAreaLeft = left - xChangeLeft, plotAreaRight = right + xChangeRight, plotAreaBottom = bottom - yChangeBottom, plotAreaTop = top + yChangeTop}
                otherwise -> area
    updateFunc $ plotSettings {plotArea = newArea}
    return True

onMouseButton :: MouseButton -> [Modifier] -> Click -> (Double, Double) -> PlotSettings -> (PlotSettings -> IO ()) -> IO Bool
onMouseButton button modifiers click (x, y) plotSettings updateFunc = do
    let
        area = plotArea $ plotSettings
        left = plotAreaLeft area
        right = plotAreaRight area
        bottom = plotAreaBottom area
        top = plotAreaTop area
    
    case button of 
        LeftButton -> do
            let
                newPos =
                    case click of
                        ReleaseClick -> Nothing
                        otherwise -> Just (x, y)
                    --case mousePos plotSettings of
                    --    Just (xStart, yStart) -> Nothing
                    --    otherwise -> Just (x, y)

                (x1, y1) = toGraphCoords (screenArea plotSettings) (plotArea plotSettings) (x, y)
                (newSelection, newSegments) =
                    case plotTool plotSettings of
                        PlotToolSelect ->
                            (if Shift `elem` modifiers
                                then
                                    case click of
                                        ReleaseClick ->
                                            fmap (\sel ->
                                                sel {plotSelectionRectangle =
                                                    case plotSelectionRectangle sel of
                                                        Just rect ->
                                                            if rectangleLeft rect == x1 || rectangleTop rect == y1
                                                                then Nothing
                                                                else  Just $ rect {rectangleRight = x1, rectangleBottom = y1}
                                                        otherwise -> Nothing
                                                }
                                            ) (plotSelection plotSettings)
                                        otherwise ->
                                            fmap (\sel ->
                                                sel {
                                                    plotSelectionRectangle = Just $ GUI.Plot.Rectangle {
                                                        rectangleLeft = x1, 
                                                        rectangleRight = x1,
                                                        rectangleTop = y1,
                                                        rectangleBottom = y1
                                                    }
                                                }
                                            ) (plotSelection plotSettings)
                                else plotSelection plotSettings,
                                plotSegments plotSettings)
                        PlotToolSegment -> 
                            (plotSelection plotSettings, 
                            if Shift `elem` modifiers then
                                case click of
                                    SingleClick -> fmap (\segments -> segments {plotSegmentsData = plotSegmentsData segments ++ [x1]}) (plotSegments plotSettings)
                                    otherwise -> plotSegments plotSettings
                            else if Graphics.UI.Gtk.Gdk.Events.Control `elem` modifiers then
                                case click of
                                    SingleClick -> fmap (\segments -> segments {plotSegmentsData = []}) (plotSegments plotSettings)
                                    otherwise -> plotSegments plotSettings
                            else plotSegments plotSettings
                            )
                        
            updateFunc $ plotSettings {mousePos = newPos, plotSelection = newSelection, plotSegments = newSegments}
            return ()
        RightButton -> 
            case click of
                ReleaseClick ->
                    do
                        return ()
                            
                otherwise -> return ()
        otherwise -> return ()
    return True

onMouseMove :: (Double, Double) -> [Modifier] -> PlotSettings -> (PlotSettings -> IO ()) -> IO Bool
onMouseMove (x, y) modifiers plotSettings updateFunc = do
    let
        area = plotArea $ plotSettings
        left = plotAreaLeft area
        right = plotAreaRight area
        bottom = plotAreaBottom area
        top = plotAreaTop area
        scrArea = screenArea plotSettings
        (newArea, newPos, newSelection) =
            case mousePos plotSettings of
                Just (xStart, yStart) ->
                    let
                        xChange = (xStart - x) * (right - left) / (screenAreaRight scrArea - screenAreaLeft scrArea)
                        yChange = (yStart - y) * (top - bottom) / (screenAreaTop scrArea - screenAreaBottom scrArea)
                    in
                        -- modifier not present somehow, ignore the button altogether
                        --if Button1 `elem` (trace ("modifiers: " ++ show modifiers) modifiers)
                        --    then
                                if Graphics.UI.Gtk.Gdk.Events.Control `elem` modifiers
                                    -- Scale
                                    then
                                        (area {plotAreaLeft = left - xChange, plotAreaRight = right + xChange, plotAreaBottom = bottom - yChange, plotAreaTop = top + yChange}, Just (x, y), plotSelection plotSettings)
                                else if Shift `elem` modifiers
                                    -- Change selection
                                    then
                                        let
                                            (x1, y1) = toGraphCoords (screenArea plotSettings) (plotArea plotSettings) (x, y)
                                            newSel =
                                                fmap (\sel ->
                                                        sel {plotSelectionRectangle = fmap (\rect ->
                                                            rect {rectangleRight = x1, rectangleBottom = y1}
                                                        ) (plotSelectionRectangle sel)
                                                    }
                                                ) (plotSelection plotSettings)

                                        in
                                            (area , Just (x, y), newSel)
                                else
                                    -- Move
                                    (area {plotAreaLeft = left + xChange, plotAreaRight = right + xChange, plotAreaBottom = bottom + yChange, plotAreaTop = top + yChange}, Just (x, y), plotSelection plotSettings)
                        --    else 
                        --        (area, Nothing, plotSelection plotSettings)
                otherwise -> (area, Nothing, plotSelection plotSettings)
    updateFunc $ plotSettings {plotArea = newArea, mousePos = newPos, plotSelection = newSelection}
        
    return True

