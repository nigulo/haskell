{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Regression.RegressionTest where

import Regression.Regression
import Test.Framework
import System.FilePath
import Utils.Xml

test_fitWithSpline11 = do
    let
        dataPath = "." ++ [pathSeparator] ++ "tests" ++ [pathSeparator] ++ "Regression" ++ [pathSeparator] ++ "data" ++ [pathSeparator]
    modulatedUnitPolynomsStr <- readFile (dataPath ++ "modulatedUnitPolynoms11.xml")
    datStr <- readFile (dataPath ++ "dat1.xml")
    splineStr <- readFile (dataPath ++ "spline11.xml")
    let
        modulatedUnitPolynoms = fromDocument (parse "" modulatedUnitPolynomsStr) 
        dat = fromDocument (parse "" datStr)
        spline = fromDocument (parse "" splineStr) 
    result <- fitWithSpline modulatedUnitPolynoms 3 dat False 2 (\_ -> return ())
    assertEqual result spline
{-
test_fitWithSpline12 = do
    let
        dataPath = "." ++ [pathSeparator] ++ "tests" ++ [pathSeparator] ++ "Regression" ++ [pathSeparator] ++ "data" ++ [pathSeparator]
    modulatedUnitPolynomsStr <- readFile (dataPath ++ "modulatedUnitPolynoms12.xml")
    datStr <- readFile (dataPath ++ "dat1.xml")
    splineStr <- readFile (dataPath ++ "spline12.xml")
    let
        modulatedUnitPolynoms = fromDocument (parse "" modulatedUnitPolynomsStr) 
        dat = fromDocument (parse "" datStr)
        spline = fromDocument (parse "" splineStr) 
    result <- fitWithSpline modulatedUnitPolynoms 1 dat False 0 (\_ -> return ())
    assertEqual result spline
    
test_fitWithSpline21 = do
    let
        dataPath = "." ++ [pathSeparator] ++ "tests" ++ [pathSeparator] ++ "Regression" ++ [pathSeparator] ++ "data" ++ [pathSeparator]
    modulatedUnitPolynomsStr <- readFile (dataPath ++ "modulatedUnitPolynoms21.xml")
    datStr <- readFile (dataPath ++ "dat2.xml")
    splineStr <- readFile (dataPath ++ "spline21.xml")
    let
        modulatedUnitPolynoms = fromDocument (parse "" modulatedUnitPolynomsStr) 
        dat = fromDocument (parse "" datStr)
        spline = fromDocument (parse "" splineStr) 
    result <- fitWithSpline modulatedUnitPolynoms 25 dat False 2 (\_ -> return ())
    assertEqual result spline
    
test_fitWithSpline22 = do
    let
        dataPath = "." ++ [pathSeparator] ++ "tests" ++ [pathSeparator] ++ "Regression" ++ [pathSeparator] ++ "data" ++ [pathSeparator]
    modulatedUnitPolynomsStr <- readFile (dataPath ++ "modulatedUnitPolynoms22.xml")
    datStr <- readFile (dataPath ++ "dat2.xml")
    splineStr <- readFile (dataPath ++ "spline22.xml")
    let
        modulatedUnitPolynoms = fromDocument (parse "" modulatedUnitPolynomsStr) 
        dat = fromDocument (parse "" datStr)
        spline = fromDocument (parse "" splineStr) 
    result <- fitWithSpline modulatedUnitPolynoms 1 dat False 0 (\_ -> return ())
    assertEqual result spline
-}    