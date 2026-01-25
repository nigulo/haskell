module Regression.RegressionTest (tests) where

import Regression.Regression
import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath
import Utils.Xml

tests :: TestTree
tests = testGroup "Regression"
    [ testCase "fitWithSpline11" test_fitWithSpline11
    , testCase "fitWithSpline12" test_fitWithSpline12
    , testCase "fitWithSpline21" test_fitWithSpline21
    , testCase "fitWithSpline22" test_fitWithSpline22
    , testCase "interpolateWithSpline" test_interpolateWithSpline
    ]

test_fitWithSpline11 :: Assertion
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
    result <- fitWithSpline modulatedUnitPolynoms 3 dat 2 (\_ -> return ())
    result @?= spline

test_fitWithSpline12 :: Assertion
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
    result <- fitWithSpline modulatedUnitPolynoms 1 dat 0 (\_ -> return ())
    result @?= spline

test_fitWithSpline21 :: Assertion
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
    result <- fitWithSpline modulatedUnitPolynoms 25 dat 2 (\_ -> return ())
    result @?= spline

test_fitWithSpline22 :: Assertion
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
    result <- fitWithSpline modulatedUnitPolynoms 1 dat 0 (\_ -> return ())
    result @?= spline

test_interpolateWithSpline :: Assertion
test_interpolateWithSpline = do
    let
        dataPath = "." ++ [pathSeparator] ++ "tests" ++ [pathSeparator] ++ "Regression" ++ [pathSeparator] ++ "data" ++ [pathSeparator]
    datStr <- readFile (dataPath ++ "dat3.xml")
    splineStr <- readFile (dataPath ++ "spline3.xml")
    let
        dat = fromDocument (parse "" datStr)
        spline = fromDocument (parse "" splineStr)
    result <- interpolateWithSpline dat
    result @?= spline
