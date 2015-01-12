{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Math.FunctionTest where

import Math.Function
import Utils.Test
import Test.Framework
import Data.Complex
import System.Random

test_functionValidate = do
        let
            myFunc :: Function (Double) = function "a+b)x+(4))"
        assertEqual [(0, ("Unexpected closing bracket", 3))] (validate myFunc)
        
        let
            myFunc :: Function (Double) = function "x+(4))"
        assertEqual [(0, ("Unexpected closing bracket", 5))] (validate myFunc)

        let
            myFunc :: Function (Double) = function "x+(4)("
        assertEqual [(0, ("Missing closing bracket for opening bracket", 5))] (validate myFunc)

        let
            myFunc :: Function (Double) = function "x+((4)"
        assertEqual [(0, ("Missing closing bracket", 5))] (validate myFunc)

        let
            myFunc :: Function (Double) = function "x+()()"
        assertEqual [(0, ("Parse error", 2))] (validate myFunc)

        let
            myFunc :: Function (Double) = function "x**"
        assertEqual [(0, ("Parse error", 2))] (validate myFunc)

        let
            myFunc :: Function (Double) = function "x++"
        assertEqual [(0, ("Parse error", 2))] (validate myFunc)

test_complexFunction = do
    let
        f :: Function (Complex Double) = toComplexFunction $ function "1+2*i+3"
    assertEqual (4 :+ 2) (getComplexValue [] f)

test_function = do
    randomGen <- newStdGen
    let
        myFunc4 :: Function (Double) = function $ 
            "a=10" ++ "\n" ++ 
            "b=x*sin(y+f(a))" ++ "\n" ++ 
            "f(i)=i^2+x" ++ "\n" ++
            "g(j)=a*f(x)+b*h(j)" ++ "\n" ++
            "(a*y+b)*g(x)"
        x = 4
        y = 3
        h [x] = sqrt x
        a = 10
        f i = i ^ 2 + x
        b = x * sin(y + f a) 
        g j = a * f x + b * h [j]
    print myFunc4
    
    assertEqual ["x","y"] (varNames myFunc4)
    assertEqual ["h"] (funcNames myFunc4) 
    assertEqualDouble ((a * y + b) * g x) (getValue [x, y] [h] randomGen myFunc4)

    let
        myFunc5 :: Function (Double) = function $ 
            "if(x>10,100,1/2)"
        x = 4
    assertEqual ["x"] (varNames myFunc5)
    assertEqual [] (funcNames myFunc5) 
    assertEqual (if x > 10 then 100 else 0.5) (getValue [x] [] randomGen myFunc5) 
    
    let
        myFunc6 :: Function Double = function $
            "xmin=getxmin()" ++ "\n" ++
            "deltaPhi=0.01" ++ "\n" ++
            "ymin(i)=gety1(f,i)" ++ "\n" ++
            "ymax(i)=gety2(f,i)" ++ "\n" ++
            "ydiff(i)=ymax(i)-ymin(i)" ++ "\n" ++
            "yiphi(i,phi)=gety(xmin+(i+phi)/f)" ++ "\n" ++
            "f(i,phi)=2*((yiphi(i,phi)-ymin(i))/ydiff(i)-0.5)" ++ "\n" ++
            "sum(i,0,98,1,sum(phi,0,1,deltaPhi,(f(i,phi)-f(i+1,phi))^2*deltaPhi))/98"
    print myFunc6
    assertEqual ["f"] (varNames myFunc6)
    assertEqual ["getxmin", "gety1", "gety2", "gety"] (funcNames myFunc6) 
    assertEqual 0 (getValue [x] [(\_ -> 0), (\_ -> 1), (\_ -> 2), (\_ -> 1)] randomGen myFunc6) 
    
    let
        myFunc7 :: Function (Double) = function "sin(5.2365114071237014e-2*x)*cos(6.283185307179586*x)"

    assertEqual ["x"] (varNames myFunc7)
    assertEqual [] (funcNames myFunc7) 
    assertEqual (sin(5.2365114071237014e-2*x)*cos(6.283185307179586*x)) (getValue [x] [] randomGen myFunc7) 
    