module Main where

import qualified Data.Time as Time

import System.FilePath ((</>), )
import System.Environment (getEnv)

import Data.Array (listArray, )
import Data.Monoid (mappend, mconcat, )
import Control.Exception
import qualified Data.List as L
import qualified Data.Vector.Unboxed as V
import Utils.Misc

--test
main :: IO ()
main = do
    let
        vect :: V.Vector (Double, Double, Double, Double) = V.fromList[(7946.820000000001,0.94,1.6551744210496597,1.0),(7946.820000000001,0.96,1.653781824799582,1.0),(7946.820000000001,0.98,1.6534754672920045,1.0),(8027.910000000001,0.0,1.733112120223307,1.0),(8027.910000000001,2.0e-2,1.7304312006765095,1.0),(8027.910000000001,4.0e-2,1.7271738788111297,1.0),(8027.910000000001,6.0e-2,1.72349127418317,1.0),(8027.910000000001,7.98e-2,1.7197547192593285,1.0),(8027.910000000001,0.1,1.715362278891957,1.0),(8027.910000000001,0.12,1.7106516681672237,1.0),(8027.910000000001,0.14,1.7058619909161763,1.0),(8027.910000000001,0.1598,1.7005716109510427,1.0),(8027.910000000001,0.18,1.6954193101169759,1.0),(8027.910000000001,0.2,1.690059657361032,1.0),(8027.910000000001,0.22,1.684740919050709,1.0),(8027.910000000001,0.2398,1.6802930642892226,1.0),(8027.910000000001,0.26,1.6760356393307712,1.0),(8027.910000000001,0.28,1.6727514307395221,1.0),(8027.910000000001,0.3,1.6703166439827557,1.0),(8027.910000000001,0.3198,1.6687581547968202,1.0)]
    putStrLn (show (groupVectorBy (\(x1, _, _, _) (x2, _, _, _) -> abs (x1 - x2) < 0.1) vect))
    