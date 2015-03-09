
module Regression.Spline (
    Spline,
    getTangent, 
    getDerivative, 
    add,
    subtr,
    mult,
    divide,
    getSubSplines,
    unmodulate,
    isModulated,
    xmlElementName,
    splineProduct,
    splineSum,
    splineDiff
    ) where

import Regression.AnalyticData
import Regression.Polynom (Polynom)
--import Regression.Data (Data (..))
import qualified Regression.Polynom as P
import qualified Math.Function as F
import qualified Math.Expression as E
import Debug.Trace
import Utils.Misc
import Utils.List
import System.Random

import Data.List
import Data.Maybe
import qualified Utils.Xml as Xml

type Spline = AnalyticData Polynom

 -- | Spline consists of multiple polynoms defined for different ranges of x,
 --   so that the value y for given x is calculated as follows:
 --   first the polynom which satisfies xMin <= x < xMax is found and then
 --   polynom's value at x.

instance Xml.XmlElement Spline where
    toElement (AnalyticData s) = Xml.element xmlElementName [] (map mapOp s) where
        mapOp (xMin, xMax, p) = Left $ Xml.element "node" [("left", show (head xMin)), ("right", show (head xMax))] [Left (Xml.toElement p)]

    fromElement e = 
        AnalyticData (map mapOp (Xml.contents e)) where 
            mapOp (Left e) =
                let 
                    xMin = read $ head $ Xml.attrValues e "left" 
                    xMax = read $ head $ Xml.attrValues e "right"
                    p = Xml.fromElement $ head $ Xml.contentElements e P.xmlElementName
                in ([xMin], [xMax], p)

xmlElementName :: String
xmlElementName = "spline"

-- | Calculates the tangent of the spline at the given coordinate
getTangent :: Double -> Spline -> Double
getTangent x s = head $ op (\[x] _ -> P.getTangent x) [[x]] (mkStdGen 1) s

-- | Calculates the derivative of the spline at the given coordinate
getDerivative :: Int -> Double -> Spline -> Double
getDerivative i x s = head $ op (\[x] _ -> P.getDerivative i x) [[x]] (mkStdGen 1) s

-- | Returns the list of subsplines this spline is built up from
getSubSplines :: Spline -> [Spline]
getSubSplines s@(AnalyticData ps) = 
    if isModulated s then
        let (xMins, xMaxs, polynoms) = unzip3 ps
            subPolynoms =  map P.getSubPolynoms polynoms
            fori subSplines i = 
                let
                    pols = map (\p -> (xMins !! i, xMaxs !! i, p)) (subPolynoms !! i)
                in zipWith (\ps pol -> ps ++ [pol]) subSplines pols
        in 
            --trace ("subPols1: " ++ show (length (subPolynoms  !! 0))) $
            --map (\ps -> AnalyticData ps) (forl_ [0 .. (length polynoms - 1)] (replicate (length (subPolynoms !! 0)) []) fori)
            map (\ps -> AnalyticData ps) (foldl' (fori) (replicate (length (subPolynoms !! 0)) []) [0 .. (length polynoms - 1)])
    else []

-- | Returns modulating functions from underlaying polynoms
unmodulate :: Spline -> Spline
unmodulate s@(AnalyticData ps) = 
    let (xMins, xMaxs, polynoms) = unzip3 ps
        unmods = map P.unmodulate polynoms
    in AnalyticData (zip3 xMins xMaxs unmods)

isModulated :: Spline -> Bool
isModulated (AnalyticData s) = any P.isModulated $ pols where
    (_, _, pols) = unzip3 s

add :: Spline -> Spline -> Spline
add = F.binaryOp (F.add)

subtr :: Spline -> Spline -> Spline
subtr = F.binaryOp (F.subtr)

mult :: Spline -> Double -> Spline
mult = F.constantOp (F.mult)

divide :: Spline -> Double -> Spline
divide = F.constantOp (F.divide)

splineProduct :: Spline -> Spline -> Spline
splineProduct (AnalyticData s1) (AnalyticData s2) = 
    let 
        fOp :: ([Double], [Double], Polynom) -> ([Double], [Double], Polynom) -> Maybe ([Double], [Double], Polynom)
        fOp (xMins1, xMaxs1, pol1) (xMins2, xMaxs2, pol2) =
            if all (\(xMin1, xMax1, xMin2, xMax2)  -> (xMin1 < xMax2 && xMax1 > xMin2) || (xMin2 < xMax1 && xMax2 > xMin1)) (zip4 xMins1 xMaxs1 xMins2 xMaxs2) 
                then Just (map (\(x1, x2) -> max x1 x2) (zip xMins1 xMins2), map (\(x1, x2) -> min x1 x2) (zip xMaxs1 xMaxs2), P.polynomProduct pol1 pol2)
                else Nothing
        newFs = [fOp (s1 !! i) (s2 !! j) | i <- [0 .. length s1 - 1], j <- [0 .. length s2 - 1]]
    in AnalyticData $ catMaybes newFs

splineSum :: Spline -> Spline -> Spline
splineSum (AnalyticData s1) (AnalyticData s2) = 
    let 
        fOp :: ([Double], [Double], Polynom) -> ([Double], [Double], Polynom) -> Maybe ([Double], [Double], Polynom)
        fOp (xMins1, xMaxs1, pol1) (xMins2, xMaxs2, pol2) =
            if all (\(xMin1, xMax1, xMin2, xMax2)  -> (xMin1 < xMax2 && xMax1 > xMin2) || (xMin2 < xMax1 && xMax2 > xMin1)) (zip4 xMins1 xMaxs1 xMins2 xMaxs2) 
                then Just (map (\(x1, x2) -> max x1 x2) (zip xMins1 xMins2), map (\(x1, x2) -> min x1 x2) (zip xMaxs1 xMaxs2), P.polynomSum pol1 pol2)
                else Nothing
        newFs = [fOp (s1 !! i) (s2 !! j) | i <- [0 .. length s1 - 1], j <- [0 .. length s2 - 1]]
    in AnalyticData $ catMaybes newFs

splineDiff :: Spline -> Spline -> Spline
splineDiff (AnalyticData s1) (AnalyticData s2) = 
    let 
        fOp :: ([Double], [Double], Polynom) -> ([Double], [Double], Polynom) -> Maybe ([Double], [Double], Polynom)
        fOp (xMins1, xMaxs1, pol1) (xMins2, xMaxs2, pol2) =
            if all (\(xMin1, xMax1, xMin2, xMax2)  -> (xMin1 < xMax2 && xMax1 > xMin2) || (xMin2 < xMax1 && xMax2 > xMin1)) (zip4 xMins1 xMaxs1 xMins2 xMaxs2) 
                then Just (map (\(x1, x2) -> max x1 x2) (zip xMins1 xMins2), map (\(x1, x2) -> min x1 x2) (zip xMaxs1 xMaxs2), P.polynomDiff pol1 pol2)
                else Nothing
        newFs = [fOp (s1 !! i) (s2 !! j) | i <- [0 .. length s1 - 1], j <- [0 .. length s2 - 1]]
    in AnalyticData $ catMaybes newFs
