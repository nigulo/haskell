module Main where

import Debug.Trace
import ISDA.InOut as IO
import Utils.Str

import Regression.Polynom as P
import Regression.Spline as S
import Regression.AnalyticData as AD
import Regression.Regression as R
import Regression.Data as D
import Math.Expression
import Math.Function

import Foreign.Storable
import Foreign.C.Types
import Data.Word
import Data.Bits
import Foreign.C.String
import qualified Data.Vector.Unboxed as V

getSpline :: ISDAState -> IO ISDAState
getSpline (params, Left dat@(dataHeader, dataBlocks)) = 
    do
        let 
            --tMin = head (ts dataBlocks);
            --tMax = last (ts dataBlocks);
            n = 500;
            --step = (tMax - tMin) / (fromIntegral (nData dataHeader) - 1);
            ys = map (realToFrac) (fs dataBlocks)
            weights = if length (IO.ws dataBlocks) == 0 
                      then take (length ys) (repeat 1)
                      else map (realToFrac) (IO.ws dataBlocks)
            rank = case getParam params "Polynom_Rank" of 
                       Just a -> a
            knots = case getParam params "Num_Knots" of
                       Just a -> a
            period =  case getParam params "Mod_Period" of
                       Just a -> a
                       Nothing -> 0
            dat = D.data1 $ V.fromList (zip3 (ts dataBlocks) ys weights)
    
        spline <- case period of 
            0 -> fitWithSpline_ rank knots dat False 2 (\_ -> return ())
            per -> 
                let 
                    freq = 2 * pi / period
                    templates =
                        [PolynomTemplate (rank, Just (fromExpression const1), Just (fromExpression const0)),
                        PolynomTemplate (rank, Just (fromExpression (sine freq)), Just (fromExpression (dsin freq))),
                        PolynomTemplate (rank, Just (fromExpression (cosine freq)), Just (fromExpression (dcos freq)))]                         
                in                    
                    fitWithSpline (modulatedUnitPolynoms templates) knots dat False 2 (\_ -> return ())
        let
            xmin = AD.xMin1 spline
            xmax = AD.xMax1 spline
            timePoints = [xmin, xmin + (xmax - xmin) / n .. xmax]
            splineValues = AD.getValues_ (init (map (\x -> [x]) timePoints)) spline
    
        return (params, 
         Right (SpecHeader {nLim = fromIntegral (length splineValues), 
                           swMin = head timePoints, 
                           swMax = last (init timePoints)}, 
               SpecBlocks {pty = map realToFrac splineValues}))

main ::IO ()
main = 
        let x = -1.2345678e-1
        in
            do
--                print (extendedToDouble (doubleToExtended x))
                state <- readState
--                state2 <- getSpline state
--                writeState state2
                state <- getSpline state
                writeState $ state
                writeLn "Data converted to spectrum successfully"


