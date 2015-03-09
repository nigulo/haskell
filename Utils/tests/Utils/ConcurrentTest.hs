{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Utils.ConcurrentTest where

import Utils.Concurrent
import Test.Framework

test_calcConcurrently = do
    let
        vals = [1, 2, 3, 4]
        expectedResult = [2, 4, 6, 8]
    mapM_ (\i -> do
            result <- calcConcurrently' i (\x _ -> return (2 * x)) (\_ -> return ()) (\_ -> return ()) (return ()) vals    
            assertEqual expectedResult result
        ) [1 .. 8]