module Utils.MiscTest (tests) where

import Data.List
import Utils.Misc
import qualified Data.Vector.Unboxed as V
import Test.Tasty
import Test.Tasty.HUnit

test_nubVector :: Assertion
test_nubVector = do
    let
        list = [7, 3, 5, 4, 7, 6, 8, 4, 5, 9, 2, 3, 8, 5, 7, 6, 3, 2]
        vect :: V.Vector Int = V.fromList list
        result = V.toList $ nubVector vect
    assertEqual "" (nub list) result

test_sortVectorBy :: Assertion
test_sortVectorBy = do
    let
        list = [7, 3, 5, 4, 7, 6, 8, 4, 5, 9, 2, 3, 8, 5, 7, 6, 3, 2]
        vect :: V.Vector Int = V.fromList list
        result = V.toList $ sortVectorBy compare vect
    assertEqual "" (sortBy compare list) result

test_groupVectorBy :: Assertion
test_groupVectorBy = do
    let
        list = "Mississippi"
        vect :: V.Vector Char = V.fromList list
        result = map V.toList $ groupVectorBy (==) vect
    assertEqual "" (groupBy (==) list) result

test_segmentVector :: Assertion
test_segmentVector = do
    let
        vect = V.fromList "Mississippi"
        expected1 = ([V.fromList "ss", V.fromList "ss", V.fromList "pp"], [V.fromList "Mi", V.fromList "i", V.fromList "i", V.fromList "i"])
        expected2 = ([V.fromList "Mi", V.fromList "i", V.fromList "i", V.fromList "i"], [V.fromList "ss", V.fromList "ss", V.fromList "pp"])
        result1 = segmentVector (\c -> c == 's' || c == 'p') vect
        result2 = segmentVector (\c -> c /= 's' && c /= 'p') vect
    assertEqual "" expected1 result1
    assertEqual "" expected2 result2

tests :: TestTree
tests = testGroup "Utils.MiscTest"
    [ testCase "nubVector" test_nubVector
    , testCase "sortVectorBy" test_sortVectorBy
    , testCase "groupVectorBy" test_groupVectorBy
    , testCase "segmentVector" test_segmentVector
    ]
