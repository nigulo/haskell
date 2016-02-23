{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Regression.RBFTest where

import Regression.RBF as RBF
import qualified Data.Eigen.Matrix as M
import Test.Framework
import Utils.Test

test_values = do
    let
        x = 1.2040
        centres =
           [
             0,       0.1000,  0.2000,  0.3000,  0.4000,  0.5000,  0.6000,  0.7000,  0.8000,  0.9000,  1.0000,  1.1000,
             1.2000,  1.3000,  1.4000,  1.5000,  1.6000,  1.7000,  1.8000,  1.9000,  2.0000]
        lambdas = repeat 0.01
        result = RBF.values (M.fromList [[x]]) (zipWith (\c l -> ((M.fromList [[c]]), l)) centres lambdas) 
        expectedResult = [
             1.106565261209755e-63,
             1.168136715717805e-53,
             1.668865562554978e-44,
             3.226710985770889e-36,
             8.443253303424283e-29,
             2.989995988817282e-22,
             1.432987588924806e-16,
             9.294484093066720e-12,
             8.158666560768563e-08,
             9.692239027205995e-05,
             1.558260579433453e-02,
             3.390526072544416e-01,
             9.984012793176064e-01,
             3.978819204512057e-01,
             2.145923908008044e-02,
             1.566337939180254e-04,
             1.547275513312255e-07,
             2.068525475834271e-11,
             3.742528632454229e-16,
             9.163901774155839e-22,
             3.036734024414009e-28
            ]        
    assertEqualDoubleList expectedResult result
