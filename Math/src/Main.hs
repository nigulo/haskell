
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
import Math.IODoubleVector as V
import Math.IODoubleMatrix as M
import Math.IODoubleLinearEquations

main :: IO ()
main = 
    do
    -- System of equations:
    -- 2x +  y +  3z = 1
    -- 2x + 6y +  8z = 3
    -- 6x + 8y + 18z = 5
    -- Solution:
    -- (x, y, z) = (3/10, 2/5, 0)
    dat <- readFile "data.txt"
    
    m <- matrix $ read dat
    result <- solveGauss m >>= values
    print result
            