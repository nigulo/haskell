
module Main where

import Math.Expression as E
import Data.Map
import Data.Char
import Data.Complex

import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty as Pretty
import Text.XML.HaXml.Parse as Parse
import Text.XML.HaXml.Util
import Text.PrettyPrint.HughesPJ
import System.Random
import Math.Function as F
import Utils.Math
import Test.HUnit
import Data.Time.Clock
import Data.List
import qualified Data.Vector.Unboxed as V
import Statistics.Test.KolmogorovSmirnov
import Statistics.Distribution.Normal
import Statistics.Types
import System.CPUTime
import Control.Monad
import Data.Array.IO

main :: IO ()
main = 
    do
        let
            a = -1 / 0
            b = 0 / 0
            c = -0
            
        randomGen <- newStdGen
  
        print $ isInfinite a
        print $ isNaN b
        print $ isNegativeZero c

        let binaryFunc :: Function (Double) = F.binaryOp F.mult (F.function "sin(x)") (F.function "sin(x)")
        print (binaryFunc)
        
        let
            sample :: Sample = V.fromList [0, 0.2, 0.4, 0.2, 0.7, -0.2, -0.1, 0.1, 0.5, 0, 0.1, -0.6, 0, 0, 0.1, -0.1, 0.1, 0.2, -0.1, 0.3, -0.2, 0.3, 0.4, -0.5, 0.6, 0.5, -0.3]
            ksTestResult = kolmogorovSmirnovD standard sample
            ksTestResult2 = kolmogorovSmirnovTest standard 0.4 sample
        putStrLn $ ("KSTest: ") ++ show ksTestResult
        putStrLn $ ("KSTest2: ") ++ show ksTestResult2
        let
                a :: [[Double]] = [[]]
        putStrLn $ ("transponse:") ++ show (transpose a)
        
        let
            f :: Expression Double = expression "x>0"
        putStrLn $ show $ E.varNames f
        let
            v = V.generate 10000000 (id)
            v2 = V.sum $ V.map (\x -> x * x) v
        putStrLn (show v2)
        let
            l = [0 :: Int, 1 .. 10000000]
            l2 = sum $ Prelude.map (\x -> x * x) l
        putStrLn (show l2)
        
        putStrLn "-------------"
        
        let
            rows@(r0:_) = Prelude.map (replicate 10) [0 .. 9]
            lastRow = length rows - 1
            lastCol = length r0 - 1
        --array :: IOArray (Int, Int) Int <- newListArray ((0, 0), (lastRow, lastCol)) [((rows !! i) !! j) | i <- [0 .. lastRow], j <- [0 .. lastCol]]
        array :: IOArray (Int, Int) Int <- newListArray ((0, 0), (lastRow, lastCol)) (concat rows)
        
        assocs <- getAssocs array
        print assocs
        