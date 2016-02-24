-- | Bayesian linear regression 
module Regression.Bayes (bayesLinReg, fit) where

import Regression.RBF as RBF
import Regression.Data as D
import qualified Data.Eigen.Matrix as M
import Numeric.Limits
import Control.Monad
import qualified Data.Vector.Unboxed as V
import Data.List
import Regression.AnalyticData

data Method = MethodLinear Int | 
    MethodRBF [Int] -- ^ list of numCentres (currently same in all dimentions)
              [Double] -- ^ list of widths (currently all equal in the model)
              (Int, Double) -- ^ (maxIters, precision)

-- | Linear regression with marginal likelihood maximization (ML II)
bayesLinReg :: M.MatrixXd -> M.MatrixXd -> M.MatrixXd -> (Int, Double) -> IO (M.MatrixXd {-m-}, M.MatrixXd {-S-}, Double {-alpha-}, Double {-beta-}, Double {-margLik-})
bayesLinReg y sumPhi sumyPhi (maxIters, precision) = do
    let
        numBases = M.rows sumyPhi
        b = fromIntegral $ numBases
        n = fromIntegral $ M.rows y
        sumyy = M.squaredNorm y
        --initial guess for the hyperparameters
        calc i prevAlpha prevBeta prevMargLik = do
            let
                invS = (M.map (*prevAlpha) (M.identity numBases)) `M.add` (M.map (*prevBeta) sumPhi)
                s = M.inverse invS
                d = M.map (*prevBeta) sumyPhi
                m = s `M.mul` d
                m' = M.transpose m
                beta = n / (sumyy - 2 * ((m' `M.mul` sumyPhi) M.! (0, 0)) + (m' `M.mul` sumPhi `M.mul` m) M.! (0, 0) + M.trace (s `M.mul` sumPhi))
                alpha = b / (M.trace s + (m' `M.mul` m) M.! (0, 0))
                d' = M.transpose d
                margLik = 0.5 * (-beta * sumyy + (d' `M.mul` invS `M.mul` d) M.! (0, 0) + log (M.determinant s) + b * log alpha + n * log beta - n * log (2 * pi))
            print i
            if (i >= maxIters) || margLik - prevMargLik < precision
                then return (m, s, alpha, beta, margLik)
                else calc (i + 1) alpha beta margLik
    calc 1 1 1 (minValue)

fit :: Data -> Method -> IO (AnalyticData RBF {-mean estimate-})
fit dat (MethodRBF numCentres lambdas opts) = do
    let
        left = D.xMins dat
        right = D.xMaxs dat
        ys = M.fromList (map (\y -> [y]) (V.toList (D.ys dat)))
    results <- mapM (\(nc, lambda) -> do
            let
                centres = sequence $ zipWith (\xMin xMax ->
                        let
                            separation = (xMax - xMin) / (fromIntegral nc + 1)
                        in
                            map (\i -> xMin + fromIntegral i * separation) [1 .. nc]
                    ) left right
                centresLambdas = map (\centre -> (M.fromList [centre], lambda)) centres
                (sumPhi, sumyPhi) = foldl' (\(sPhi, syPhi) (x, y, w) ->
                        let
                            phi = M.fromList $ [RBF.values (M.fromList [x]) centresLambdas]
                            phi' = M.transpose phi
                        in
                            (sPhi `M.add` (phi `M.mul` phi'), syPhi `M.add` (M.map (*y) phi))
                    ) (0, 0) (D.values dat)
            (_, _, _, _, margLik) <- bayesLinReg ys sumPhi sumyPhi (50, 0.001)
            return (centresLambdas, (sumPhi, sumyPhi), margLik)
        ) [(nc, lambda) | nc <- numCentres, lambda <- lambdas]
    let
        (centresLambdas, (sumPhi, sumyPhi), margLik) = maximumBy (\(_, _, margLik1) (_, _, margLik2) -> compare margLik1 margLik2) results
    -- fit the model one more time
    (m, s, alpha, beta, margLik) <- bayesLinReg ys sumPhi sumyPhi opts
    let
        rbf = RBF $ zipWith (\[w] (c, l) -> (w, c, l)) (M.toList m) centresLambdas
    return (AnalyticData [(left, right, rbf)])
