-- | Bayesian linear regression
module Regression.Bayes (rbfMLII, linRegWithMLII, Method(..)) where

import Prelude hiding ((<>))
import Regression.RBF as RBF
import Regression.Data as D
import Numeric.LinearAlgebra as M
import Numeric.Limits
import Control.Monad
import qualified Data.Vector.Unboxed as V
import Data.List
import Regression.AnalyticData

-- | Matrix type alias for compatibility
type MatrixXd = Matrix Double

data Method = MethodLinear Int |
    MethodRBF Int  -- ^ number of centres (currently same in all dimensions)
              [(Double, Double)] -- ^ ranges per dimension
              [Double] -- ^ list of trial widths (currently all equal in the model)
              (Int, Double) -- ^ (maxIters, precision)


-- Private method, only made accessible for unit tests
-- | Linear regression with marginal likelihood maximization (ML II)
rbfMLII :: MatrixXd -> MatrixXd -> MatrixXd -> (Int, Double) -> IO (MatrixXd {-m-}, MatrixXd {-S-}, Double {-alpha-}, Double {-beta-}, Double {-margLik-})
rbfMLII y sumPhi sumyPhi (maxIters, precision) = do
    let
        numBases = M.rows sumyPhi
        b = fromIntegral $ numBases
        n = fromIntegral $ M.rows y
        sumyy = squaredNorm y
        --initial guess for the hyperparameters
        calc i prevAlpha prevBeta prevMargLik = do
            let
                invS = (M.cmap (*prevAlpha) (M.ident numBases)) `add` (M.cmap (*prevBeta) sumPhi)
                s = M.inv invS
                d = M.cmap (*prevBeta) sumyPhi
                m = s <> d
                m' = M.tr m
                beta = n / (sumyy - 2 * ((m' <> sumyPhi) `atIndex` (0, 0)) + (m' <> sumPhi <> m) `atIndex` (0, 0) + M.sumElements (M.takeDiag (s <> sumPhi)))
                alpha = b / (M.sumElements (M.takeDiag s) + (m' <> m) `atIndex` (0, 0))
                d' = M.tr d
                margLik = 0.5 * (-beta * sumyy + (d' <> invS <> d) `atIndex` (0, 0) + log (M.det s) + b * log alpha + n * log beta - n * log (2 * pi))
            if (i >= maxIters) || margLik - prevMargLik < precision
                then return (m, s, alpha, beta, margLik)
                else calc (i + 1) alpha beta margLik
    calc 1 1 1 (minValue)
  where
    squaredNorm m = M.sumElements (m * m)
    add a b = a + b

linRegWithMLII :: Data -> Method -> IO (AnalyticData RBF {-mean estimate-}, ([Double] -> Double) {-varFunc-})
linRegWithMLII dat (MethodRBF numCentres ranges lambdas opts) = do
    let
        ys = M.fromLists (map (\y -> [y]) (V.toList (D.ys dat)))
    results <- mapM (\lambda -> do
            let
                centres = sequence $ map (\(xMin, xMax) ->
                        let
                            separation = (xMax - xMin) / (fromIntegral numCentres - 1)
                        in
                            map (\i -> xMin + fromIntegral i * separation) [0 .. numCentres - 1]
                    ) ranges
                centresLambdas = map (\centre -> (M.fromLists [centre], lambda)) centres
                (sumPhi, sumyPhi) = foldl' (\(sPhi, syPhi) (x, y, w) ->
                        let
                            phi = M.fromLists $ map (\p -> [p]) $ RBF.values (M.fromLists [x]) centresLambdas
                            phi' = M.tr phi
                        in
                            (sPhi + (phi <> phi'), syPhi + (M.cmap (*y) phi))
                    ) (M.konst 0 (numCentres, numCentres), M.konst 0 (numCentres, 1)) (D.values dat)
            (m, s, alpha, beta, margLik) <- rbfMLII ys sumPhi sumyPhi opts
            return (centresLambdas, m, s, alpha, beta, margLik)
        ) lambdas
    let
        (centresLambdas, m, s, alpha, beta, margLik) = maximumBy (\(_, _, _, _, _, margLik1) (_, _, _, _, _, margLik2) -> compare margLik1 margLik2) results
    let
        rbf = RBF $ zipWith (\[w] (c, l) -> (w, c, l)) (M.toLists m) centresLambdas
    return (AnalyticData [(map (fst) ranges, map (snd) ranges, rbf)], \x ->
            let
                phi = M.fromLists $ map (\p -> [p]) $ RBF.values (M.fromLists [x]) centresLambdas
                phi' = M.tr phi
            in
                (phi' <> s <> phi) `atIndex` (0, 0) + 1/ beta
        )
