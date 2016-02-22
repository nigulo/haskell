-- | Radial basis functions
module Regression.RBF (RBF(..), values) where

import qualified Math.Function as F
import qualified Data.Eigen.Matrix as M
import System.Random

newtype RBF = RBF [(Double {-weight-}, M.MatrixXd {-centre-}, Double {-lambda-})]


values :: M.MatrixXd -> [(M.MatrixXd {-centre-}, Double {-lambda-})] -> [Double]
values x rbf = map (\(centre, lambda) -> exp(-M.squaredNorm (x - centre) / lambda)) rbf

value :: M.MatrixXd -> RBF -> Double
value x (RBF rbf) = sum $ map (\(weight, centre, lambda) -> weight * exp(-M.squaredNorm (x - centre) / lambda)) rbf

instance F.Fn RBF where

    -- | Returns the polynom's value at the given coordinate
    getValue xs _ g (RBF rbf) = value (M.fromList [xs]) (RBF rbf)

    getValue_ xs g = F.getValue xs [] g 

    constantOp op (RBF rbf) k = RBF (map (\(weight, centre, lambda) -> (k * weight, centre, lambda)) rbf) 

    -- | Binary opration between the weights only
    binaryOp op (RBF rbf1) (RBF rbf2) = RBF (zipWith (\(weight1, centre1, lambda1) (weight2, centre2, lambda2) -> (F.getValue_ [weight1, weight2] (mkStdGen 1234) op, centre1, lambda1)) rbf1 rbf2)

{-
instance Xml.XmlElement Polynom where
    toElement p@(RBF rbf) = Xml.element xmlElementName [] (map mapOp pol) where
        mapOp (coefs, f, d) =
            Left $ Xml.element "polynom" [("version", "1")]
            [
                Left (Xml.element "coeficients" [] (map (\coef -> Right (show coef)) coefs)),
                Left (Xml.element "modulatorfunc" [] (
                    case f of 
                        Just expression -> [Left $ Xml.toElement expression]
                        otherwise -> []
                )), 
                Left (Xml.element "modulatorderiv" [] ( 
                    case d of 
                        Just expression -> [Left $ Xml.toElement expression]
                        otherwise -> []
                ))
            ]

    fromElement e = 
        Polynom (map mapOp (Xml.contents e)) where
            mapOp (Left e) =     
                let 
                    name = Xml.name e
                    attrs = Xml.attrs e
                    version = case Xml.maybeAttrValue e "version" of
                        Just v -> v
                        Nothing -> "1"
                    coefs = Xml.contents $ head $ Xml.contentElements e "coeficients"
                    f = Xml.contents $ head $ Xml.contentElements e "modulatorfunc"
                    d = Xml.contents $ head $ Xml.contentElements e "modulatorderiv"
                in
                    (
                        map (\(Right coef) -> read coef) coefs,
                        (case f of 
                            [] -> Nothing
                            otherwise ->
                                Just $ head $ map (\(Left e) -> Xml.fromElement e) f
                            ),
                        (case d of 
                            [] -> Nothing
                            otherwise -> 
                                Just $ head $ map (\(Left e) -> Xml.fromElement e) d
                            )
                    )

xmlElementName :: String
xmlElementName = "rbf"
-}