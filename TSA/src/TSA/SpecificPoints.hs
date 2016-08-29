module TSA.SpecificPoints (findExtrema, findZeroCrossings) where


import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.AnalyticDataWrapper as ADW
import Regression.Utils

import TSA.CommonParams
import TSA.Params
import TSA.Data

import Data.List
import Data.Maybe
import Utils.Misc

import Control.Concurrent.MVar
import Control.Concurrent
import System.CPUTime
import Math.Expression
import qualified Math.Function as F

import System.Random
import qualified Data.Vector.Unboxed as V
import Statistics.Sample

findExtrema :: DataParams -> Int -> Bool -> String 
    -> TaskEnv
    -> IO (DataParams, DataParams)
findExtrema dataParams precision global name taskEnv = do
    g <- getStdGen 
    let 
        findExtremaFunc i sd puFunc = 
            case unboxSubData sd of
                Left d -> do
                    let
                        (minima, maxima) = D.getExtrema d global
                    return [SD1 $ D.data1' minima, SD1 $ D.data1' maxima]
                Right ad -> do 
                    let
                        (minima, maxima) = ADW.getExtrema precision Nothing g ad
                    return [SD1 $ D.data1' minima, SD1 $ D.data1' maxima] 

    [minima, maxima] <- applyToData findExtremaFunc dataParams [name ++ "_min", name ++ "_max"] taskEnv
    return (minima, maxima)

findZeroCrossings :: DataParams -> Int  -> String -> TaskEnv -> IO DataParams
findZeroCrossings dataParams precision name taskEnv = do
    g <- getStdGen 
    let 
        findZCFunc i sd puFunc = do
            case unboxSubData sd of
                Left d -> do
                    let
                        zc = D.getZeroCrossings d
                    return [SD1 $ D.data1 (V.map (\x -> (x, 0, 1)) zc)] 
                Right ad -> do 
                    let
                        zc = ADW.getZeroCrossings precision g ad
                    return [SD1 $ D.data1 (V.map (\x -> (x, 0, 1)) zc)] 

    [zc] <- applyToData findZCFunc dataParams [name] taskEnv
    return zc
