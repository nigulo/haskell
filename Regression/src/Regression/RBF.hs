-- | Radial basis functions
module Regression.RBF (RBF(..), values) where

import qualified Math.Function as F
import qualified Data.Eigen.Matrix as M
import System.Random
import qualified Utils.Xml as Xml
import Data.List
import Data.Maybe

newtype RBF = RBF [(Double {-weight-}, M.MatrixXd {-centre-}, Double {-lambda-})] 

values :: M.MatrixXd -> [(M.MatrixXd {-centre-}, Double {-lambda-})] -> [Double]
values x rbf = map (\(centre, lambda) -> exp(-M.squaredNorm (x - centre) / lambda)) rbf

value :: M.MatrixXd -> RBF -> Double
value x (RBF rbf) = sum $ map (\(weight, centre, lambda) -> weight * exp(-M.squaredNorm (x - centre) / lambda)) rbf

-- NB show and read are untested!
instance Show RBF where
    show (RBF rbf) = "RBF" ++ concatMap (\(weight, centre, lambda) -> "(" ++ show weight ++ "," ++ show (M.toList centre) ++ "," ++ show lambda ++ ")\n") rbf

instance Read (RBF) where
    readsPrec _ = \s ->
        case take 3 s of
            "RBF" -> [(RBF (map (\line -> 
                    let
                        Just i = elemIndex ',' line
                        weight = take i line
                        line1 = drop (i + 1) line
                        i1 = length line1 - (fromJust (elemIndex ',' (reverse line1)))
                        centre = take i1 line1
                        lambda = drop (i1 + 1) line1
                    in
                        (read weight, M.fromList (read centre), read lambda)
                ) (lines (drop 3 s))), "")]

instance F.Fn RBF where

    -- | Returns the polynom's value at the given coordinate
    getValue xs _ g (RBF rbf) = value (M.fromList [xs]) (RBF rbf)

    getValue_ xs g = F.getValue xs [] g 

    constantOp op (RBF rbf) k = RBF (map (\(weight, centre, lambda) -> (k * weight, centre, lambda)) rbf) 

    -- | Binary opration between the weights only
    binaryOp op (RBF rbf1) (RBF rbf2) = RBF (zipWith (\(weight1, centre1, lambda1) (weight2, centre2, lambda2) -> (F.getValue_ [weight1, weight2] (mkStdGen 1234) op, centre1, lambda1)) rbf1 rbf2)


instance Xml.XmlElement RBF where
    toElement p@(RBF rbf) = Xml.element xmlElementName [] (map mapOp rbf) where
        mapOp (weight, centre, lambda) =
            Left $ Xml.element "rbf" [("weight", show weight), ("lambda", show lambda)]
            [Right (show (M.toList centre))]

    fromElement e = 
        RBF (map mapOp (Xml.contents e)) where
            mapOp (Left e) = (read (Xml.attrValue e "weight"), M.fromList (read (head (Xml.contentTexts e))), read (Xml.attrValue e "lambda"))

xmlElementName :: String
xmlElementName = "rbfs"
