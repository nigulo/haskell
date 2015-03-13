module Main where

import Debug.Trace
import ISDA.InOut as IO
import Utils.Str

import Regression.IMF
import Regression.Spline as S
import Regression.Regression as R
import Regression.Utils as U
import Regression.Data as D

import Foreign.Storable
import Foreign.C.Types
import Data.Word
import Data.Bits
import Foreign.C.String
import qualified Data.Vector.Unboxed as V
getIMF :: ISDAState -> IO ISDAState
getIMF (params, Left dat@(dataHeader, dataBlocks)) = 
    do
        let 
            n = 500
            ys = map (realToFrac) (fs dataBlocks)
            weights = if length (IO.ws dataBlocks) == 0 
                      then take (length ys) (repeat 1)
                      else map (realToFrac) (IO.ws dataBlocks)
            rank = case getParam params "Polynom_Rank" of 
                       Just a -> a
            knots = case getParam params "Num_Knots" of
                       Just a -> a
            envType = case getString params "Envelope_Type" of
                       Just "ON" -> True
                       Just "OFF" -> False
            envPrec = case getParam params "Envelope_Precision" of
                       Just a -> a :: Double
            dat = D.data1 $ V.fromList (zip3 (ts dataBlocks) ys weights)
        spline <- fitWithSpline_ rank knots dat 2 (\_ -> return ())
        let
            sdev = U.stdev dat (Right (Left spline))
        env <- imfIO rank knots rank knots (sdev / envPrec) dat
        let
            timePoints = D.xs1 env
            splineValues = D.ys env
        
        return (params, 
                 Left (DataHeader {bands = bands dataHeader,
                                  curSegs = curSegs dataHeader,
                                  nData = fromIntegral (V.length splineValues),
                                  tOff = tOff dataHeader,
                                  fMin = V.minimum splineValues,
                                  fMax = V.maximum splineValues,
                                  isIndex = isIndex dataHeader},
                       DataBlocks {ts = V.toList timePoints,
                                  dataSegs = [],
                                  fs = V.toList $ V.map (realToFrac) splineValues,
                                  IO.ws = [],
                                  is = []}))


main ::IO ()
main = 
        let x = -1.2345678e-1
        in
            do
--              print (extendedToDouble (doubleToExtended x))
                state <- readState
--                state2 <- getSpline state
--                writeState state2
                state <- getIMF state
                writeState $ state
                writeLn "Data converted to IMF successfully"


