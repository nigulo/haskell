module TSA.Extrema (findExtrema) where


import Debug.Trace

import Regression.Polynom as P
import Regression.Spline as S
import Regression.Regression as R
import Regression.Data as D
import Regression.AnalyticData as AD
import Regression.Utils

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

findExtrema :: DataParams -> Int  -> String -> (Double -> IO ()) -> IO (DataParams, DataParams)
findExtrema dataParams precision name puFunc = do
    g <- getStdGen 
    let 
        findExtremaFunc i j (Left d) puFunc = do
            let
                (minima, maxima) = D.getExtrema d
            return [Left $ D.data1' minima, Left $ D.data1' maxima] 
        findExtremaFunc i j (Right ad) puFunc = do 
            let
                (minima, maxima) = case ad of
                    Left s -> AD.getExtrema precision Nothing g s
                    Right f -> AD.getExtrema precision Nothing g f
            return [Left $ D.data1' minima, Left $ D.data1' maxima] 

    [minima, maxima] <- applyToData findExtremaFunc dataParams [name ++ "_min", name ++ "_max"] puFunc
    return (minima, maxima)
