{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Utils.MiscTest where

import Data.List
import Utils.Misc
import qualified Data.Vector.Unboxed as V
import Test.Framework

test_nubVector = do
    let 
        list = [7, 3, 5, 4, 7, 6, 8, 4, 5, 9, 2, 3, 8, 5, 7, 6, 3, 2]
        vect :: V.Vector Int = V.fromList list
        result = V.toList $ nubVector vect
    assertEqual (nub list) result
    
test_sortVectorBy = do
    let 
        list = [7, 3, 5, 4, 7, 6, 8, 4, 5, 9, 2, 3, 8, 5, 7, 6, 3, 2]
        vect :: V.Vector Int = V.fromList list
        result = V.toList $ sortVectorBy compare vect
    assertEqual (sortBy compare list) result

test_groupVectorBy = do
    let 
        list = "Mississippi"
        vect :: V.Vector Char = V.fromList list
        result = map V.toList $ groupVectorBy (==) vect
    assertEqual (groupBy (==) list) result
