-- | Radial basis functions
module Regression.RBF (RBF(..), RBFs, values, xmlElementName) where

import Regression.AnalyticData
import qualified Math.Function as F
import Numeric.LinearAlgebra as M
import System.Random
import qualified Utils.Xml as Xml
import Data.List
import Data.Maybe

-- | Matrix type alias for compatibility
type MatrixXd = Matrix Double

newtype RBF = RBF [(Double {-weight-}, MatrixXd {-centre-}, Double {-lambda-})] deriving (Show, Read)

type RBFs = AnalyticData RBF

values :: MatrixXd -> [(MatrixXd {-centre-}, Double {-lambda-})] -> [Double]
values x rbf = map (\(centre, lambda) -> exp(-squaredNorm (x - centre) / lambda)) rbf
  where
    squaredNorm m = M.sumElements (m * m)

value :: MatrixXd -> RBF -> Double
value x (RBF rbf) = sum $ map (\(weight, centre, lambda) -> weight * exp(-squaredNorm (x - centre) / lambda)) rbf
  where
    squaredNorm m = M.sumElements (m * m)

instance F.Fn RBF where

    -- | Returns the polynom's value at the given coordinate
    getValue xs _ g (RBF rbf) = value (M.fromLists [xs]) (RBF rbf)

    getValue_ xs g = F.getValue xs [] g

    constantOp op (RBF rbf) k = RBF (map (\(weight, centre, lambda) -> (k * weight, centre, lambda)) rbf)

    -- | Binary opration between the weights only
    binaryOp op (RBF rbf1) (RBF rbf2) = RBF (zipWith (\(weight1, centre1, lambda1) (weight2, centre2, lambda2) -> (F.getValue_ [weight1, weight2] (mkStdGen 1234) op, centre1, lambda1)) rbf1 rbf2)


instance Xml.XmlElement RBF where
    toElement p@(RBF rbf) = Xml.element xmlElementName [] (map mapOp rbf) where
        mapOp (weight, centre, lambda) =
            Left $ Xml.element "rbf" [("weight", show weight), ("lambda", show lambda)]
            [Right (show (M.toLists centre))]

    fromElement e =
        RBF (map mapOp (Xml.contents e)) where
            mapOp (Left e) = (read (Xml.attrValue e "weight"), M.fromLists (read (head (Xml.contentTexts e))), read (Xml.attrValue e "lambda"))

instance Xml.XmlElement (AnalyticData RBF) where
    toElement (AnalyticData rbfs) = Xml.element xmlElementName [] (map mapOp rbfs) where
        mapOp (xMin, xMax, rbf) = Left $ Xml.element "node" [("left", show (head xMin)), ("right", show (head xMax))] [Left (Xml.toElement rbf)]

    fromElement e =
        AnalyticData (map mapOp (Xml.contents e)) where
            mapOp (Left e) =
                let
                    xMin = read $ head $ Xml.attrValues e "left"
                    xMax = read $ head $ Xml.attrValues e "right"
                    rbf = Xml.fromElement $ head $ Xml.contentElements e xmlElementName
                in ([xMin], [xMax], rbf)

xmlElementName :: String
xmlElementName = "rbfs"
