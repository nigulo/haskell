module TSA.Interpolate (interpolate) where

import Regression.Regression as R

import TSA.CommonParams
import TSA.Params
import TSA.Data

interpolate :: Int -> String -> DataParams -> TaskEnv -> IO DataParams
interpolate method fitName dat taskEnv = do
    let 
        func i (SD1 dat) _ = interpolateWithSpline dat >>= \spline -> return $ SD2 spline
    applyToData1 func dat fitName taskEnv

