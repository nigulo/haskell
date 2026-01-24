module Math.ExpressionTest (tests) where

import Math.Expression
import Utils.Test
import Test.Tasty
import Test.Tasty.HUnit

test_greaterEquals :: Assertion
test_greaterEquals = do
    let
        expr :: Expression Double = expression "x>=0"
    assertEqual "" ["x"] (varNames expr)
    assertEqual "" 0 (calcDouble__ expr ("x", -0.1))
    assertEqual "" 1 (calcDouble__ expr ("x", 0))
    assertEqual "" 1 (calcDouble__ expr ("x", 0.1))

test_lowerEquals :: Assertion
test_lowerEquals = do
    let
        expr :: Expression Double = expression "x<=2"
    assertEqual "" ["x"] (varNames expr)
    assertEqual "" 1 (calcDouble__ expr ("x", 1.9))
    assertEqual "" 1 (calcDouble__ expr ("x", 2))
    assertEqual "" 0 (calcDouble__ expr ("x", 2.1))

test_greater :: Assertion
test_greater = do
    let
        expr :: Expression Double = expression "x>0"
    assertEqual "" ["x"] (varNames expr)
    assertEqual "" 0 (calcDouble__ expr ("x", -0.1))
    assertEqual "" 0 (calcDouble__ expr ("x", 0))
    assertEqual "" 1 (calcDouble__ expr ("x", 0.1))

test_lower :: Assertion
test_lower = do
    let
        expr :: Expression Double = expression "x<2"
    assertEqual "" ["x"] (varNames expr)
    assertEqual "" 1 (calcDouble__ expr ("x", 1.9))
    assertEqual "" 0 (calcDouble__ expr ("x", 2))
    assertEqual "" 0 (calcDouble__ expr ("x", 2.1))

test_equal :: Assertion
test_equal = do
    let
        expr :: Expression Double = expression "x==0"
    assertEqual "" ["x"] (varNames expr)
    assertEqual "" 0 (calcDouble__ expr ("x", -0.1))
    assertEqual "" 1 (calcDouble__ expr ("x", 0))
    assertEqual "" 0 (calcDouble__ expr ("x", 0.1))

test_notEqual :: Assertion
test_notEqual = do
    let
        expr :: Expression Double = expression "x!=2"
    assertEqual "" ["x"] (varNames expr)
    assertEqual "" 1 (calcDouble__ expr ("x", 1.9))
    assertEqual "" 0 (calcDouble__ expr ("x", 2))
    assertEqual "" 1 (calcDouble__ expr ("x", 2.1))

test_plus :: Assertion
test_plus = do
    let
        expr :: Expression Double = expression "x+y"
    assertEqual "" 3 (calcDouble_ expr [("x", 1), ("y", 2)])
    assertEqual "" 3 (calcDouble_ expr [("y", 1), ("x", 2)])

test_minus :: Assertion
test_minus = do
    let
        expr :: Expression Double = expression "x-y"
    assertEqual "" (-1) (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual "" 1 (calcDouble_ expr [("y", 2), ("x", 3)])

test_mul :: Assertion
test_mul = do
    let
        expr :: Expression Double = expression "x*y"
    assertEqual "" 6 (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual "" 6 (calcDouble_ expr [("y", 2), ("x", 3)])

test_div :: Assertion
test_div = do
    let
        expr :: Expression Double = expression "x/y"
    assertEqual "" (2 / 3) (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual "" 1.5 (calcDouble_ expr [("y", 2), ("x", 3)])

test_pow :: Assertion
test_pow = do
    let
        expr :: Expression Double = expression "x^y"
    assertEqual "" 8 (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual "" 9 (calcDouble_ expr [("y", 2), ("x", 3)])

test_logBase :: Assertion
test_logBase = do
    let
        expr :: Expression Double = expression "logbase(2,x)"
    assertEqual "" (logBase 2 5) (calcDouble__ expr ("x", 5))

test_min :: Assertion
test_min = do
    let
        expr :: Expression Double = expression "min(x,y)"
    assertEqual "" 2 (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual "" 2 (calcDouble_ expr [("y", 2), ("x", 3)])

test_max :: Assertion
test_max = do
    let
        expr :: Expression Double = expression "max(x,y)"
    assertEqual "" 3 (calcDouble_ expr [("x", 2), ("y", 3)])
    assertEqual "" 3 (calcDouble_ expr [("y", 2), ("x", 3)])

test_normal :: Assertion
test_normal = do
    let
        expr :: Expression Double = expression "gauss(x,2,1.5)"
        gauss x mean sigma = exp (-0.5 * ((x - mean) / sigma) ^ (2 :: Int)) / (sigma * sqrt(2 * pi))
    assertEqualDouble (gauss 2 2 1.5) (calcDouble__ expr ("x", 2))
    assertEqualDouble (gauss 2.2 2 1.5) (calcDouble__ expr ("x", 2.2))
    assertEqualDouble (gauss 1.8 2 1.5) (calcDouble__ expr ("x", 1.8))
    assertEqualDouble (calcDouble__ expr ("x", 2.2)) (calcDouble__ expr ("x", 1.8))

test_sum :: Assertion
test_sum = do
    let
        expr :: Expression Double = expression "sum(x,1,10,1,x)"
    assertEqual "" 55 (calcDouble___ expr)

test_product :: Assertion
test_product = do
    let
        expr :: Expression Double = expression "product(x,1,5,1,x)"
    assertEqual "" 120 (calcDouble___ expr)

test_if :: Assertion
test_if = do
    let
        expr :: Expression Double = expression "if(x,2,3)"
    assertEqual "" 3 (calcDouble__ expr ("x", 0))
    assertEqual "" 2 (calcDouble__ expr ("x", 1))

test_fact :: Assertion
test_fact = do
    let
        expr :: Expression Double = expression "x!"
    assertEqual "" 120 (calcDouble__ expr ("x", 5))
    assertEqual "" 1 (calcDouble__ expr ("x", 1))
    assertEqual "" 1 (calcDouble__ expr ("x", 0))

test_sqrt :: Assertion
test_sqrt = do
    let
        expr :: Expression Double = expression "sqrt(x)"
    assertEqual "" 3 (calcDouble__ expr ("x", 9))
    assertEqual "" 4 (calcDouble__ expr ("x", 16))

test_sin :: Assertion
test_sin = do
    let
        expr :: Expression Double = expression "sin(x)"
    assertEqualDouble (sin (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (sin 5) (calcDouble__ expr ("x", 5))

test_cos :: Assertion
test_cos = do
    let
        expr :: Expression Double = expression "cos(x)"
    assertEqualDouble (cos (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (cos 5) (calcDouble__ expr ("x", 5))

test_tan :: Assertion
test_tan = do
    let
        expr :: Expression Double = expression "tan(x)"
    assertEqualDouble (tan (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (tan 5) (calcDouble__ expr ("x", 5))

test_sinh :: Assertion
test_sinh = do
    let
        expr :: Expression Double = expression "sinh(x)"
    assertEqualDouble (sinh (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (sinh 5) (calcDouble__ expr ("x", 5))

test_cosh :: Assertion
test_cosh = do
    let
        expr :: Expression Double = expression "cosh(x)"
    assertEqualDouble (cosh (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (cosh 5) (calcDouble__ expr ("x", 5))

test_tanh :: Assertion
test_tanh = do
    let
        expr :: Expression Double = expression "tanh(x)"
    assertEqualDouble (tanh (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (tanh 5) (calcDouble__ expr ("x", 5))

test_asin :: Assertion
test_asin = do
    let
        expr :: Expression Double = expression "asin(x)"
    assertEqualDouble (asin (-0.8)) (calcDouble__ expr ("x", -0.8))
    assertEqualDouble (asin 0.8) (calcDouble__ expr ("x", 0.8))

test_acos :: Assertion
test_acos = do
    let
        expr :: Expression Double = expression "acos(x)"
    assertEqualDouble (acos (-0.8)) (calcDouble__ expr ("x", -0.8))
    assertEqualDouble (acos 0.8) (calcDouble__ expr ("x", 0.8))

test_atan :: Assertion
test_atan = do
    let
        expr :: Expression Double = expression "atan(x)"
    assertEqualDouble (atan (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (atan 5) (calcDouble__ expr ("x", 5))

test_asinh :: Assertion
test_asinh = do
    let
        expr :: Expression Double = expression "asinh(x)"
    assertEqualDouble (asinh (-0.8)) (calcDouble__ expr ("x", -0.8))
    assertEqualDouble (asinh 0.8) (calcDouble__ expr ("x", 0.8))

test_acosh :: Assertion
test_acosh = do
    let
        expr :: Expression Double = expression "acosh(x)"
    assertEqualDouble (acosh (-0.8)) (calcDouble__ expr ("x", -0.8))
    assertEqualDouble (acosh 0.8) (calcDouble__ expr ("x", 0.8))

test_atanh :: Assertion
test_atanh = do
    let
        expr :: Expression Double = expression "atanh(x)"
    assertEqualDouble (atanh (-5)) (calcDouble__ expr ("x", -5))
    assertEqualDouble (atanh 5) (calcDouble__ expr ("x", 5))

test_log :: Assertion
test_log = do
    let
        expr :: Expression Double = expression "log(x)"
    assertEqualDouble (log 5) (calcDouble__ expr ("x", 5))

test_exp :: Assertion
test_exp = do
    let
        expr :: Expression Double = expression "exp(x)"
    assertEqualDouble (exp 5) (calcDouble__ expr ("x", 5))

test_abs :: Assertion
test_abs = do
    let
        expr :: Expression Double = expression "abs(x)"
    assertEqual "" 5 (calcDouble__ expr ("x", -5))
    assertEqual "" 5 (calcDouble__ expr ("x", 5))

test_sgn :: Assertion
test_sgn = do
    let
        expr :: Expression Double = expression "sgn(x)"
    assertEqual "" (-1) (calcDouble__ expr ("x", -5))
    assertEqual "" 1 (calcDouble__ expr ("x", 5))
    assertEqual "" 0 (calcDouble__ expr ("x", 0))

test_round :: Assertion
test_round = do
    let
        expr :: Expression Double = expression "round(x)"
    assertEqual "" (fromIntegral ((round (5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.1))
    assertEqual "" (fromIntegral ((round (-5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.1))
    assertEqual "" (fromIntegral ((round (5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.6))
    assertEqual "" (fromIntegral ((round (-5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.6))

test_trunc :: Assertion
test_trunc = do
    let
        expr :: Expression Double = expression "trunc(x)"
    assertEqual "" (fromIntegral ((truncate (5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.1))
    assertEqual "" (fromIntegral ((truncate (-5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.1))
    assertEqual "" (fromIntegral ((truncate (5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.6))
    assertEqual "" (fromIntegral ((truncate (-5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.6))

test_floor :: Assertion
test_floor = do
    let
        expr :: Expression Double = expression "floor(x)"
    assertEqual "" (fromIntegral ((floor (5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.1))
    assertEqual "" (fromIntegral ((floor (-5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.1))
    assertEqual "" (fromIntegral ((floor (5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.6))
    assertEqual "" (fromIntegral ((floor (-5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.6))

test_ceil :: Assertion
test_ceil = do
    let
        expr :: Expression Double = expression "ceil(x)"
    assertEqual "" (fromIntegral ((ceiling (5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.1))
    assertEqual "" (fromIntegral ((ceiling (-5.1 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.1))
    assertEqual "" (fromIntegral ((ceiling (5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", 5.6))
    assertEqual "" (fromIntegral ((ceiling (-5.6 :: Double)) :: Int)) (calcDouble__ expr ("x", -5.6))

tests :: TestTree
tests = testGroup "Math.ExpressionTest"
    [ testCase "greaterEquals" test_greaterEquals
    , testCase "lowerEquals" test_lowerEquals
    , testCase "greater" test_greater
    , testCase "lower" test_lower
    , testCase "equal" test_equal
    , testCase "notEqual" test_notEqual
    , testCase "plus" test_plus
    , testCase "minus" test_minus
    , testCase "mul" test_mul
    , testCase "div" test_div
    , testCase "pow" test_pow
    , testCase "logBase" test_logBase
    , testCase "min" test_min
    , testCase "max" test_max
    , testCase "normal" test_normal
    , testCase "sum" test_sum
    , testCase "product" test_product
    , testCase "if" test_if
    , testCase "fact" test_fact
    , testCase "sqrt" test_sqrt
    , testCase "sin" test_sin
    , testCase "cos" test_cos
    , testCase "tan" test_tan
    , testCase "sinh" test_sinh
    , testCase "cosh" test_cosh
    , testCase "tanh" test_tanh
    , testCase "asin" test_asin
    , testCase "acos" test_acos
    , testCase "atan" test_atan
    , testCase "asinh" test_asinh
    , testCase "acosh" test_acosh
    , testCase "atanh" test_atanh
    , testCase "log" test_log
    , testCase "exp" test_exp
    , testCase "abs" test_abs
    , testCase "sgn" test_sgn
    , testCase "round" test_round
    , testCase "trunc" test_trunc
    , testCase "floor" test_floor
    , testCase "ceil" test_ceil
    ]
