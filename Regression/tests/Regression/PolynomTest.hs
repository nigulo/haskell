{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Regression.PolynomTest where

import Regression.Polynom
import Math.Function as Fn
import Test.Framework

test_constantOp = do
    let
        f1 = function "sin(x)"
        d1 = function "cos(x)"
        f2 = function "cos(x)"
        d2 = function "-sin(x)"
        f3 = function "e^x"
        d3 = function "e^x"
        pol = Polynom [
            ([(0, 2), (1, 0.7), (2, 1.5)], Just f1, Just d1), 
            ([(1, 4.5)], Just f2, Just d2),
            ([(0, 7), (3, 1)], Just f3, Just d3)
            ]

        polMulConst = Polynom [
            ([(0, -2), (1, -0.7), (2, -1.5)], Just f1, Just d1), 
            ([(1, -4.5)], Just f2, Just d2),
            ([(0, -7), (3, -1)], Just f3, Just d3)
            ]
        result = constantOp Fn.mult pol (-1)
    assertEqual result polMulConst

test_binaryOp = do
    let
        f1 = function "sin(x)"
        d1 = function "cos(x)"
        f2 = function "cos(x)"
        d2 = function "-sin(x)"
        f3 = function "e^x"
        d3 = function "e^x"
        pol1 = Polynom [
            ([(0, 2), (1, 0.7), (2, 1.5)], Just f1, Just d1), 
            ([(1, 4.5)], Just f2, Just d2),
            ([(0, 7), (3, 1)], Just f3, Just d3)
            ]

        pol2 = Polynom [
            ([(4, 3.3)], Just f1, Just d1), 
            ([(1, -3), (2, 1.1)], Just f2, Just d2),
            ([(1, 0.6), (2, -2.8), (3, 4.2)], Just f2, Just d2)
            ]

        polSum = Polynom [
            ([(0, 2), (1, 0.7), (2, 1.5), (4, 3.3)], Just f1, Just d1), 
            ([(1, 4.5 - 3), (2, 1.1)], Just f2, Just d2),
            ([(0, 7), (1, 0.6), (2, -2.8), (3, 1 + 4.2)], Just f3, Just d3)
            ]
        result = binaryOp Fn.add pol1 pol2
    assertEqual result polSum
    
test_polynomProduct = do
    let
        f11 = function "sin(x)"
        d11 = function "cos(x)"
        f12 = function "cos(x)"
        d12 = function "-sin(x)"
        f13 = function "sin(x)*cos(x)"
        d13 = function "cos(x)*cos(x) - sin(x)*sin(x)"
        pol1 = Polynom [
            ([(0, 2), (1, 0.7), (2, 1.5)], Just f11, Just d11), 
            ([(1, 4.5)], Just f12, Just d12),
            ([(0, 7), (3, 1)], Just f13, Just d13)
            ]

        f21 = function "ln(x)"
        d21 = function "1/x"
        f22 = function "sqrt(x)"
        d22 = function "-1/sqrt(x)"
        pol2 = Polynom [
            ([(4, 3.3)], Just f21, Just d21), 
            ([(1, 3), (2, 1.1)], Just f22, Just d22)
            ]
        
        f1121 = Fn.binaryOp Fn.mult f11 f21
        f1122 = binaryOp Fn.mult f11 f22
        f1221 = binaryOp Fn.mult f12 f21
        f1222 = binaryOp Fn.mult f12 f22
        f1321 = binaryOp Fn.mult f13 f21
        f1322 = binaryOp Fn.mult f13 f22

        d1121 = binaryOp Fn.add (binaryOp Fn.mult d11 f21) (binaryOp Fn.mult f11 d21)
        d1122 = binaryOp Fn.add (binaryOp Fn.mult d11 f22) (binaryOp Fn.mult f11 d22)
        d1221 = binaryOp Fn.add (binaryOp Fn.mult d12 f21) (binaryOp Fn.mult f12 d21)
        d1222 = binaryOp Fn.add (binaryOp Fn.mult d12 f22) (binaryOp Fn.mult f12 d22)
        d1321 = binaryOp Fn.add (binaryOp Fn.mult d13 f21) (binaryOp Fn.mult f13 d21)
        d1322 = binaryOp Fn.add (binaryOp Fn.mult d13 f22) (binaryOp Fn.mult f13 d22)
        
        polProduct = Polynom [
            ([(4, 2 * 3.3), (5, 0.7 * 3.3), (6, 1.5 * 3.3)], Just f1121, Just d1121), 
            ([(1, 2 * 3), (2, 2 * 1.1 + 0.7 * 3), (3, 0.7 * 1.1 + 1.5 * 3), (4, 1.5 * 1.1)], Just f1122, Just d1122), 
            ([(5, 4.5 * 3.3)], Just f1221, Just d1221), 
            ([(2, 4.5 * 3), (3, 4.5 * 1.1)], Just f1222, Just d1222), 
            ([(4, 7 * 3.3), (7, 1 * 3.3)], Just f1321, Just d1321),
            ([(1, 7 * 3), (2, 7 * 1.1), (4, 1 * 3), (5, 1 * 1.1)], Just f1322, Just d1322) 
            ]
        result = polynomProduct pol1 pol2
    assertEqual result polProduct
    
test_polynomSum = do
    let
        f11 = function "sin(x)"
        d11 = function "cos(x)"
        f12 = function "cos(x)"
        d12 = function "-sin(x)"
        f13 = function "ln(x)"
        d13 = function "1/x"
        pol1 = Polynom [
            ([(0, 2), (1, 0.7), (2, 1.5)], Just f11, Just d11), 
            ([(1, 4.5)], Just f12, Just d12),
            ([(0, 7), (3, 1)], Just f13, Just d13)
            ]

        f21 = function "ln(x)"
        d21 = function "1/x"
        f22 = function "cos(x)"
        d22 = function "-sin(x)"
        pol2 = Polynom [
            ([(4, 3.3)], Just f21, Just d21), 
            ([(1, 3), (2, 1.1)], Just f22, Just d22)
            ]
        
        polSum = Polynom [
            ([(0, 2), (1, 0.7), (2, 1.5)], Just f11, Just d11), 
            ([(4, 3.3)], Just f21, Just d21), 
            ([(1, 4.5 + 3), (2, 1.1)], Just f12, Just d12),
            ([(0, 7), (3, 1)], Just f13, Just d13)
            ]
        result = polynomSum pol1 pol2
    assertEqual result polSum
    