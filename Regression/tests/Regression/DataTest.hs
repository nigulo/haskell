{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Regression.DataTest where

import Regression.Data as D
import Test.Framework
import qualified Data.Vector.Unboxed as V

test_getTangent = do
    let
        dat = data1' $ V.fromList [(0, 0.5), (1, 2), (2, 0.75), (3, 0)] 
        result = D.xys1 $ D.getTangent dat
        expectedResult = V.fromList [(1, (0.75 - 0.5) / 2), (2, (0 - 2) / 2)]
    assertEqual expectedResult result
        