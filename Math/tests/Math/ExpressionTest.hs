{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Math.ExpressionTest where

import Math.Expression
import Utils.Test
import Test.Framework

test_greatherEquals = do
    let
        expr :: Expression Double = expression "x>=0"
    assertEqual ["x"] (varNames expr)
    assertEqual 0 (calcDouble__ expr ("x", -0.1))
    assertEqual 1 (calcDouble__ expr ("x", 0))
    assertEqual 1 (calcDouble__ expr ("x", 0.1))

test_lowerEquals = do
    let
        expr :: Expression Double = expression "x<=2"
    assertEqual ["x"] (varNames expr)
    assertEqual 1 (calcDouble__ expr ("x", 1.9))
    assertEqual 1 (calcDouble__ expr ("x", 2))
    assertEqual 0 (calcDouble__ expr ("x", 2.1))

test_greather = do
    let
        expr :: Expression Double = expression "x>0"
    assertEqual ["x"] (varNames expr)
    assertEqual 0 (calcDouble__ expr ("x", -0.1))
    assertEqual 0 (calcDouble__ expr ("x", 0))
    assertEqual 1 (calcDouble__ expr ("x", 0.1))

test_lower = do
    let
        expr :: Expression Double = expression "x<2"
    assertEqual ["x"] (varNames expr)
    assertEqual 1 (calcDouble__ expr ("x", 1.9))
    assertEqual 0 (calcDouble__ expr ("x", 2))
    assertEqual 0 (calcDouble__ expr ("x", 2.1))

test_equal = do
    let
        expr :: Expression Double = expression "x==0"
    assertEqual ["x"] (varNames expr)
    assertEqual 0 (calcDouble__ expr ("x", -0.1))
    assertEqual 1 (calcDouble__ expr ("x", 0))
    assertEqual 0 (calcDouble__ expr ("x", 0.1))

test_notEqual = do
    let
        expr :: Expression Double = expression "x!=2"
    assertEqual ["x"] (varNames expr)
    assertEqual 1 (calcDouble__ expr ("x", 1.9))
    assertEqual 0 (calcDouble__ expr ("x", 2))
    assertEqual 1 (calcDouble__ expr ("x", 2.1))

test_plus = do
    let
        expr :: Expression Double = expression "x+y"
    assertEqual 3 (calcDouble_ expr [("x", 1), ("y", 2)])
    assertEqual 3 (calcDouble_ expr [("y", 1), ("x", 2)])

test_minus = do
    let
        expr :: Expression Double = expression "x-y"
    assertEqual (-1) (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual 1 (calcDouble_ expr [("y", 2), ("x", 3)])

test_mul = do
    let
        expr :: Expression Double = expression "x*y"
    assertEqual 6 (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual 6 (calcDouble_ expr [("y", 2), ("x", 3)])

test_div = do
    let
        expr :: Expression Double = expression "x/y"
    assertEqual (2 / 3) (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual 1.5 (calcDouble_ expr [("y", 2), ("x", 3)])

test_pow = do
    let
        expr :: Expression Double = expression "x^y"
    assertEqual 8 (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual 9 (calcDouble_ expr [("y", 2), ("x", 3)])

test_logBase = do
    let
        expr :: Expression Double = expression "logbase(2,x)"
    assertEqual (logBase 2 5) (calcDouble__ expr ("x", 5))

test_min = do
    let
        expr :: Expression Double = expression "min(x,y)"
    assertEqual 2 (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual 2 (calcDouble_ expr [("y", 2), ("x", 3)])

test_max = do
    let
        expr :: Expression Double = expression "max(x,y)"
    assertEqual 3 (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual 3 (calcDouble_ expr [("y", 2), ("x", 3)])

test_normal = do
    let
        expr :: Expression Double = expression "normal(x,2,1.5)"
        normal x mean sigma = exp (-0.5 * ((x - mean) / sigma) ^ 2) / (sigma * sqrt(2 * pi))
    assertEqualDouble (normal 2 2 1.5) (calcDouble__ expr ("x", 2))
    assertEqualDouble (normal 2.2 2 1.5) (calcDouble__ expr ("x", 2.2))
    assertEqualDouble (normal 1.8 2 1.5) (calcDouble__ expr ("x", 1.8))
    assertEqualDouble (calcDouble__ expr ("x", 2.2)) (calcDouble__ expr ("x", 1.8))

test_sum = do
    let
        expr :: Expression Double = expression "sum(x,1,10,1,x)"
    assertEqual 55 (calcDouble___ expr)

test_product = do
    let
        expr :: Expression Double = expression "product(x,1,5,1,x)"
    assertEqual 120 (calcDouble___ expr)

test_if = do
    let
        expr :: Expression Double = expression "if(x,2,3)"
    assertEqual 3 (calcDouble__ expr ("x", 0))
    assertEqual 2 (calcDouble__ expr ("x", 1))

test_fact = do
    let
        expr :: Expression Double = expression "x!"
    assertEqual 120 (calcDouble__ expr ("x", 5))
    assertEqual 1 (calcDouble__ expr ("x", 1))
    assertEqual 1 (calcDouble__ expr ("x", 0))

test_sqrt = do
    let
        expr :: Expression Double = expression "sqrt(x)"
    assertEqual 3 (calcDouble__ expr ("x", 9))
    assertEqual 4 (calcDouble__ expr ("x", 16))

test_sin = do
    let
        expr :: Expression Double = expression "sin(x)"
    assertEqualDouble (sin (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (sin 5) (calcDouble__ expr ("x", 5))

test_cos = do
    let
        expr :: Expression Double = expression "cos(x)"
    assertEqualDouble (cos (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (cos 5) (calcDouble__ expr ("x", 5))

test_tan = do
    let
        expr :: Expression Double = expression "tan(x)"
    assertEqualDouble (tan (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (tan 5) (calcDouble__ expr ("x", 5))

test_sinh = do
    let
        expr :: Expression Double = expression "sinh(x)"
    assertEqualDouble (sinh (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (sinh 5) (calcDouble__ expr ("x", 5))

test_cosh = do
    let
        expr :: Expression Double = expression "cosh(x)"
    assertEqualDouble (cosh (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (cosh 5) (calcDouble__ expr ("x", 5))

test_tanh = do
    let
        expr :: Expression Double = expression "tanh(x)"
    assertEqualDouble (tanh (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (tanh 5) (calcDouble__ expr ("x", 5))

test_asin = do
    let
        expr :: Expression Double = expression "asin(x)"
    assertEqualDouble (asin (-0.8)) (calcDouble__ expr ("x", -0.8))
    assertEqualDouble (asin 0.8) (calcDouble__ expr ("x", 0.8))

test_acos = do
    let
        expr :: Expression Double = expression "acos(x)"
    assertEqualDouble (acos (-0.8)) (calcDouble__ expr ("x", -0.8))
    assertEqualDouble (acos 0.8) (calcDouble__ expr ("x", 0.8))

test_atan = do
    let
        expr :: Expression Double = expression "atan(x)"
    assertEqualDouble (atan (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (atan 5) (calcDouble__ expr ("x", 5))

test_asinh = do
    let
        expr :: Expression Double = expression "asinh(x)"
    assertEqualDouble (asinh (-0.8)) (calcDouble__ expr ("x", -0.8))
    assertEqualDouble (asinh 0.8) (calcDouble__ expr ("x", 0.8))

test_acosh = do
    let
        expr :: Expression Double = expression "acosh(x)"
    assertEqualDouble (acosh (-0.8)) (calcDouble__ expr ("x", -0.8))
    assertEqualDouble (acosh 0.8) (calcDouble__ expr ("x", 0.8))

test_atanh = do
    let
        expr :: Expression Double = expression "atanh(x)"
    assertEqualDouble (atanh (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (atanh 5) (calcDouble__ expr ("x", 5))

test_log = do
    let
        expr :: Expression Double = expression "log(x)"
    assertEqualDouble (log 5) (calcDouble__ expr ("x", 5))

test_exp = do
    let
        expr :: Expression Double = expression "exp(x)"
    assertEqualDouble (exp 5) (calcDouble__ expr ("x", 5))

test_abs = do
    let
        expr :: Expression Double = expression "abs(x)"
    assertEqual 5 (calcDouble__ expr ("x", -5))
    assertEqual 5 (calcDouble__ expr ("x", 5))
test_sgn = do
    let
        expr :: Expression Double = expression "sgn(x)"
    assertEqual (-1) (calcDouble__ expr ("x", -5))
    assertEqual (1) (calcDouble__ expr ("x", 5))
    assertEqual 0 (calcDouble__ expr ("x", 0))

test_round = do
    let
        expr :: Expression Double = expression "round(x)"
    assertEqual (fromIntegral ((round (5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.1))
    assertEqual (fromIntegral ((round (-5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.1))
    assertEqual (fromIntegral ((round (5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.6))
    assertEqual (fromIntegral ((round (-5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.6))
    
test_trunc = do
    let
        expr :: Expression Double = expression "trunc(x)"
    assertEqual (fromIntegral ((truncate (5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.1))
    assertEqual (fromIntegral ((truncate (-5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.1))
    assertEqual (fromIntegral ((truncate (5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.6))
    assertEqual (fromIntegral ((truncate (-5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.6))
    
test_floor = do
    let
        expr :: Expression Double = expression "floor(x)"
    assertEqual (fromIntegral ((floor (5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.1))
    assertEqual (fromIntegral ((floor (-5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.1))
    assertEqual (fromIntegral ((floor (5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.6))
    assertEqual (fromIntegral ((floor (-5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.6))
test_ceil = do
    let
        expr :: Expression Double = expression "ceil(x)"
    assertEqual (fromIntegral ((ceiling (5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.1))
    assertEqual (fromIntegral ((ceiling (-5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.1))
    assertEqual (fromIntegral ((ceiling (5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.6))
    assertEqual (fromIntegral ((ceiling (-5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.6))

