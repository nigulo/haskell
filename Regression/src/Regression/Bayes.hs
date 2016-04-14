-- | Bayesian linear regression 
module Regression.Bayes (rbfMLII, linRegWithMLII, Method(..)) where

import Regression.RBF as RBF
import Regression.Data as D
import qualified Data.Eigen.Matrix as M
import Numeric.Limits
import Control.Monad
import qualified Data.Vector.Unboxed as V
import Data.List
import Regression.AnalyticData

data Method = MethodLinear Int | 
    MethodRBF Int  -- ^ number of centres (currently same in all dimensions)
              [(Double, Double)] -- ^ ranges per dimension
              [Double] -- ^ list of trial widths (currently all equal in the model)
              (Int, Double) -- ^ (maxIters, precision)


-- Private method, only made accessible for unit tests 
-- | Linear regression with marginal likelihood maximization (ML II)
rbfMLII :: M.MatrixXd -> M.MatrixXd -> M.MatrixXd -> (Int, Double) -> IO (M.MatrixXd {-m-}, M.MatrixXd {-S-}, Double {-alpha-}, Double {-beta-}, Double {-margLik-})
rbfMLII y sumPhi sumyPhi (maxIters, precision) = do
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
            if (i >= maxIters) || margLik - prevMargLik < precision
                then return (m, s, alpha, beta, margLik)
                else calc (i + 1) alpha beta margLik
    calc 1 1 1 (minValue)

linRegWithMLII :: Data -> Method -> IO (AnalyticData RBF {-mean estimate-}, ([Double] -> Double) {-varFunc-})
linRegWithMLII dat (MethodRBF numCentres ranges lambdas opts) = do
    let
        ys = M.fromList (map (\y -> [y]) (V.toList (D.ys dat)))
    results <- mapM (\lambda -> do
            let
                centres = sequence $ map (\(xMin, xMax) ->
                        let
                            separation = (xMax - xMin) / (fromIntegral numCentres - 1)
                        in
                            map (\i -> xMin + fromIntegral i * separation) [0 .. numCentres - 1]
                    ) ranges
                centresLambdas = map (\centre -> (M.fromList [centre], lambda)) centres
                (sumPhi, sumyPhi) = foldl' (\(sPhi, syPhi) (x, y, w) ->
                        let
                            phi = M.fromList $ map (\p -> [p]) $ RBF.values (M.fromList [x]) centresLambdas
                            phi' = M.transpose phi
                        in
                            (sPhi `M.add` (phi `M.mul` phi'), syPhi `M.add` (M.map (*y) phi))
                    ) (M.zero numCentres numCentres, M.zero numCentres 1) (D.values dat)
            (m, s, alpha, beta, margLik) <- rbfMLII ys sumPhi sumyPhi opts
            return (centresLambdas, m, s, alpha, beta, margLik)
        ) lambdas
    let
        (centresLambdas, m, s, alpha, beta, margLik) = maximumBy (\(_, _, _, _, _, margLik1) (_, _, _, _, _, margLik2) -> compare margLik1 margLik2) results
    let
        rbf = RBF $ zipWith (\[w] (c, l) -> (w, c, l)) (M.toList m) centresLambdas
    return (AnalyticData [(map (fst) ranges, map (snd) ranges, rbf)], \x ->
            let
                phi = M.fromList $ map (\p -> [p]) $ RBF.values (M.fromList [x]) centresLambdas
                phi' = M.transpose phi
            in
                (phi' `M.mul` s `M.mul` phi) M.! (0, 0) + 1/ beta
        )
