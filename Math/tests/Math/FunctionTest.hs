module Math.FunctionTest (tests) where

import qualified Math.Function as F
import Utils.Test
import Test.Tasty
import Test.Tasty.HUnit
import Data.Complex
import System.Random

test_functionValidate :: Assertion
test_functionValidate = do
        let
            myFunc :: F.Function (Double) = F.function "a+b)x+(4))"
        assertEqual "" [(0, ("Unexpected closing bracket", 3))] (F.validate myFunc)

        let
            myFunc2 :: F.Function (Double) = F.function "x+(4))"
        assertEqual "" [(0, ("Unexpected closing bracket", 5))] (F.validate myFunc2)

        let
            myFunc3 :: F.Function (Double) = F.function "x+(4)("
        assertEqual "" [(0, ("Missing closing bracket for opening bracket", 5))] (F.validate myFunc3)

        let
            myFunc4 :: F.Function (Double) = F.function "x+((4)"
        assertEqual "" [(0, ("Missing closing bracket", 5))] (F.validate myFunc4)

        let
            myFunc5 :: F.Function (Double) = F.function "x+()()"
        assertEqual "" [(0, ("Parse error", 2))] (F.validate myFunc5)

        let
            myFunc6 :: F.Function (Double) = F.function "x**"
        assertEqual "" [(0, ("Parse error", 2))] (F.validate myFunc6)

        let
            myFunc7 :: F.Function (Double) = F.function "x++"
        assertEqual "" [(0, ("Parse error", 2))] (F.validate myFunc7)

test_complexFunction :: Assertion
test_complexFunction = do
    let
        f :: F.Function (Complex Double) = F.toComplexFunction $ F.function "1+2*i+3"
    assertEqual "" (4 :+ 2) (F.getComplexValue [] f)

test_function :: Assertion
test_function = do
    randomGen <- newStdGen
    let
        myFunc4 :: F.Function (Double) = F.function $
            "a=10" ++ "\n" ++
            "b=x*sin(y+f(a))" ++ "\n" ++
            "f(i)=i^2+x" ++ "\n" ++
            "g(j)=a*f(x)+b*h(j)" ++ "\n" ++
            "(a*y+b)*g(x)"
        x = 4
        y = 3
        h [x'] = sqrt x'
        h _ = 0
        a = 10
        f i = i ^ (2 :: Int) + x
        b = x * sin(y + f a)
        g j = a * f x + b * h [j]
    print myFunc4

    assertEqual "" ["x","y"] (F.varNames myFunc4)
    assertEqual "" ["h"] (F.funcNames myFunc4)
    assertEqualDouble ((a * y + b) * g x) (F.getValue [x, y] [h] randomGen myFunc4)

    let
        myFunc5 :: F.Function (Double) = F.function $
            "if(x>10,100,1/2)"
    assertEqual "" ["x"] (F.varNames myFunc5)
    assertEqual "" [] (F.funcNames myFunc5)
    assertEqual "" (if x > 10 then 100 else 0.5) (F.getValue [x] [] randomGen myFunc5)

    let
        myFunc6 :: F.Function Double = F.function $
            "xmin=getxmin()" ++ "\n" ++
            "deltaPhi=0.01" ++ "\n" ++
            "ymin(i)=gety1(f,i)" ++ "\n" ++
            "ymax(i)=gety2(f,i)" ++ "\n" ++
            "ydiff(i)=ymax(i)-ymin(i)" ++ "\n" ++
            "yiphi(i,phi)=gety(xmin+(i+phi)/f)" ++ "\n" ++
            "f(i,phi)=2*((yiphi(i,phi)-ymin(i))/ydiff(i)-0.5)" ++ "\n" ++
            "sum(i,0,98,1,sum(phi,0,1,deltaPhi,(f(i,phi)-f(i+1,phi))^2*deltaPhi))/98"
    print myFunc6
    assertEqual "" ["f"] (F.varNames myFunc6)
    assertEqual "" ["getxmin", "gety1", "gety2", "gety"] (F.funcNames myFunc6)
    assertEqual "" 0 (F.getValue [x] [(\_ -> 0), (\_ -> 1), (\_ -> 2), (\_ -> 1)] randomGen myFunc6)

    let
        myFunc7 :: F.Function (Double) = F.function "sin(5.2365114071237014e-2*x)*cos(6.283185307179586*x)"

    assertEqual "" ["x"] (F.varNames myFunc7)
    assertEqual "" [] (F.funcNames myFunc7)
    assertEqual "" (sin(5.2365114071237014e-2*x)*cos(6.283185307179586*x)) (F.getValue [x] [] randomGen myFunc7)

tests :: TestTree
tests = testGroup "Math.FunctionTest"
    [ testCase "functionValidate" test_functionValidate
    , testCase "complexFunction" test_complexFunction
    , testCase "function" test_function
    ]
