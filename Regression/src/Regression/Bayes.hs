-- | Bayesian linear regression 
module Regression.Bayes where

import Data.Eigen.Matrix as M
import Numeric.Limits

data OptMethod = ML2
data Method = Linear Int | RBF [(MatrixXd, Double)]


rbf :: MatrixXd -> [(MatrixXd {-center-}, Double {-lambda-})] -> MatrixXd
rbf x centresLambdas =
    fromList [Prelude.map (\(centre, lambda) -> exp(-squaredNorm (x - centre) / lambda)) centresLambdas]


bayesLinReg :: MatrixXd -> MatrixXd -> MatrixXd -> (Int, Double) -> IO (MatrixXd {-m-}, MatrixXd {-S-}, Double {-alpha-}, Double {-beta-}, Double {-margLik-})
bayesLinReg y sumPhi sumyPhi (maxIters, precision) = do
    let
        numBases = cols sumyPhi
        b = fromIntegral $ numBases
        n = fromIntegral $ rows y
        sumyy = squaredNorm y
        --initial guess for the hyperparameters
        calc i prevAlpha prevBeta prevMargLik = do
            let
                invS = (M.map (*prevAlpha) (identity numBases)) `add` (M.map (*prevAlpha) sumPhi)
                s = inverse invS
                d = M.map (*prevBeta) sumyPhi
                m = s `mul` d
                m' = transpose m
                beta = n / (sumyy - 2 * (((m' `mul` sumyPhi) `add` (m' `mul` sumPhi `mul` m)) ! (0, 0)) + trace (s `mul` sumPhi))
                alpha = b / (trace s + (m' `mul` m) ! (0, 0))
                d' = transpose d
                margLik = 0.5 * (-beta * sumyy + (d' `mul` invS `mul` d) ! (0, 0) + log (determinant s) + b * log alpha + n * log beta - n * log (2 * pi))
            if (i + 1 >= maxIters) || margLik - prevMargLik < precision
                then return (m, s, alpha, beta, margLik)
                else calc (i + 1) alpha beta margLik
    calc 0 1 1 (maxValue)
