module Utils.ConcurrentTest (tests) where

import Utils.Concurrent
import Test.Tasty
import Test.Tasty.HUnit

test_calcConcurrently :: Assertion
test_calcConcurrently = do
    let
        vals = [1, 2, 3, 4]
        expectedResult = [2, 4, 6, 8]
    mapM_ (\i -> do
            result <- calcConcurrently' i (\x _ -> return (2 * x)) (\_ -> return ()) (\_ -> return ()) (return ()) vals
            assertEqual ("with " ++ show i ++ " threads") expectedResult result
        ) [1 .. 8]

tests :: TestTree
tests = testGroup "Utils.ConcurrentTest"
    [ testCase "calcConcurrently" test_calcConcurrently
    ]
