{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Math.IODoubleVectorTest where

import qualified Math.IODoubleVector as V
import Utils.Test
import Test.Framework

test_elemOp = do
    v <- V.vector [0, 1 .. 9]
    let
        expectedResult = [1, 2 .. 10]
    V.elemOp (+1) v
    result <- V.values v        
    assertEqual expectedResult result
