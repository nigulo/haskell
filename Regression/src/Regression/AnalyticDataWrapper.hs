
module Regression.AnalyticDataWrapper (
    AnalyticDataWrapper,
    analyticDataWrapper,
    xMins,
    xMaxs,
    xMin1,
    xMax1,
    getValues,
    getValues_,
    getValue,
    getValue_
   ) where

import qualified Regression.AnalyticData as AD
import qualified Math.Function as F
import Utils.List
import Utils.Misc
import Data.List
import System.Random
import Debug.Trace

data AnalyticDataWrapper = forall d . F.Fn d => AnalyticDataWrapper (AD.AnalyticData d)

instance F.Fn AnalyticDataWrapper where
    getValue x f g (AnalyticDataWrapper ad) = F.getValue x f g ad 
    getValue_ xs (AnalyticDataWrapper ad) = F.getValue_ xs ad 

    constantOp op (AnalyticDataWrapper ad) k = analyticDataWrapper $ F.constantOp op ad k
    --binaryOp op (AnalyticDataWrapper (ad1 :: AD.AnalyticData d)) (AnalyticDataWrapper (ad2 :: AD.AnalyticData d2)) = analyticDataWrapper $ F.binaryOp op ad1 ad2  
    --binaryOp op (AnalyticDataWrapper ad1@(AD.AnalyticData (F.Function _))) (AnalyticDataWrapper ad2@(AD.AnalyticData (F.Function _))) = analyticDataWrapper $ F.binaryOp op ad1 ad2  

    
    
analyticDataWrapper :: forall d . (F.Fn d) => AD.AnalyticData d -> AnalyticDataWrapper
analyticDataWrapper = AnalyticDataWrapper

xMins :: AnalyticDataWrapper -> [Double]
xMins (AnalyticDataWrapper ad) = AD.xMins ad  

-- | Returns maximum x-value for which the analytic data is defined
xMaxs :: AnalyticDataWrapper -> [Double]
xMaxs (AnalyticDataWrapper ad) = AD.xMaxs ad

xMin1 :: AnalyticDataWrapper -> Double
xMin1 = head.xMins

xMax1 :: AnalyticDataWrapper -> Double
xMax1 = head.xMaxs

-- | Returns y-values for given array of x-values
getValues :: RandomGen g => [[Double]] -> g -> AnalyticDataWrapper -> [Double]
getValues xs g (AnalyticDataWrapper ad) = AD.getValues xs g ad  

getValues_ :: [[Double]] -> AnalyticDataWrapper -> [Double]
getValues_ xs = getValues xs (mkStdGen 1) 

getValue :: RandomGen g => [Double] -> g -> AnalyticDataWrapper -> Double
getValue x g (AnalyticDataWrapper ad) = AD.getValue x g ad

getValue_ :: [Double] -> AnalyticDataWrapper -> Double
getValue_ x d = head $ getValues_ [x] d
