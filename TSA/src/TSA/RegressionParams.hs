module TSA.RegressionParams (
    FitParams (..),
    FitType(..), 
    SplineParams(..),
    HarmonicParams(..),
    LsqParams (..), 
    BayesLinRegMethodParams (..),
    BayesLinRegParams (..), 
    EnvParams (..),
    newBayesLinReg,
    newEnv,
    newFitParams
    ) where

import TSA.CommonParams    
import qualified Utils.Xml as Xml

import Data.Either

data FitType = FitTypeSpline | FitTypeHarmonic deriving (Show, Read)

data SplineParams = 
    SplineParams {
        splineNumNodes :: Int
    } deriving (Show, Read)

instance Xml.XmlElement SplineParams where
    toElement params = Xml.element "splineparams" [
        ("numnodes", show (splineNumNodes params))
        ] []
        
    fromElement e =
        SplineParams {
            splineNumNodes = read (Xml.attrValue e "numnodes")
        }
    
data HarmonicParams = HarmonicParams {
    harmonicCoverageFactor :: Double,
    harmonicCount :: Int
} deriving (Show, Read) 

instance Xml.XmlElement HarmonicParams where
    toElement params = Xml.element "harmonicparams" [
        ("coveragefactor", show (harmonicCoverageFactor params)),
        ("count", show (harmonicCount params))
        ] []
        
    fromElement e =
        HarmonicParams {
            harmonicCoverageFactor = read $
                case Xml.maybeAttrValue e "coveragefactor" of
                    Just u -> u
                    Nothing -> 
                        case Xml.maybeAttrValue e "period" of
                            Just u -> u
                            Nothing -> 
                                case Xml.maybeAttrValue e "upper" of
                                    Just u -> u,
            harmonicCount = read (Xml.attrValue e "count")
        }

data FitParams = FitParams {
    fitPolynomRank :: Int,
    fitPeriod :: Double,
    fitNumHarmonics :: Int,
    fitCommonParams :: CommonParams,
    fitType :: FitType,
    fitSplineParams :: SplineParams,
    fitHarmonicParams :: HarmonicParams
} deriving (Show, Read) {-! derive : XmlContent !-} -- this line is for DrIFT

instance Xml.XmlElement FitParams where
    toElement params = Xml.element "fitparams" [
        ("type", show (fitType params)),
        ("polynomdegree", show (fitPolynomRank params)),
        ("period", show (fitPeriod params)),
        ("numharmonics", show (fitNumHarmonics params))
        ] 
        [Left (Xml.toElement (fitCommonParams params)), 
        Left (Xml.toElement (fitSplineParams params)),
        Left (Xml.toElement (fitHarmonicParams params))
        ]
        
    fromElement e =
        FitParams {
            fitType = read (Xml.attrValue e "type"),
            fitPolynomRank = read (Xml.attrValue e "polynomdegree"),
            fitPeriod = read (Xml.attrValue e "period"),
            fitNumHarmonics = read (Xml.attrValue e "numharmonics"),
            fitCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName),
            fitSplineParams = Xml.fromElement (Xml.contentElement e "splineparams"),
            fitHarmonicParams = Xml.fromElement (Xml.contentElement e "harmonicparams")
        }

data LsqParams = LsqParams {
    lsqFitParams :: FitParams,
    lsqBootstrapCount :: Int
} deriving (Show, Read)

instance Xml.XmlElement LsqParams where
    toElement params = Xml.element "lsqparams" 
        [("bootstrapcount", show (lsqBootstrapCount params))] 
        [Left (Xml.toElement (lsqFitParams params))]

    fromElement e = 
        LsqParams {
            lsqFitParams = Xml.fromElement (Xml.contentElement e "fitparams"),
            lsqBootstrapCount = case Xml.maybeAttrValue e "bootstrapcount" of
                Just count -> read count
                Nothing -> 0
        }


data BayesLinRegMethodParams = RBFParams {
    rbfNumCentres :: Int,
    rbfNumLambdas :: Int
} deriving (Show, Read)


instance Xml.XmlElement BayesLinRegMethodParams where

    toElement (RBFParams numCentres numLambdas) = Xml.element "rbfparams" 
        [("numcentres", show numCentres),
         ("numlambdas", show numLambdas)
        ] 
        []
        
    fromElement e = 
        case Xml.name e of
            "rbfparams" ->
                RBFParams {
                    rbfNumCentres = read $ Xml.attrValue e "numcentres",
                    rbfNumLambdas = read $ Xml.attrValue e "numlambdas"
                }

data BayesLinRegParams = BayesLinRegParams {
    bayesLinRegData :: Maybe DataParams,
    bayesLinRegAlgo :: Int,
    bayesLinRegMethod :: Int,
    bayesLinRegMethodParams :: [BayesLinRegMethodParams],
    bayesLinRegCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement BayesLinRegParams where

    toElement params = Xml.element "bayeslinregparams" 
        [("algo", show (bayesLinRegAlgo params)), 
         ("method", show (bayesLinRegMethod params))]
        (
            [Left (Xml.element "methodparams" [] (map (Left . Xml.toElement) (bayesLinRegMethodParams params)))]
            ++ case bayesLinRegData params of
                Just dataParams -> [Left (Xml.toElement dataParams)]
                Nothing -> []
            ++ [Left (Xml.toElement (bayesLinRegCommonParams params))]
        )
        
    fromElement e = 
        BayesLinRegParams {
            bayesLinRegData =
                case Xml.contentElements e "dataparams" of
                    [dataParams] -> Just $ Xml.fromElement dataParams
                    otherwise -> Nothing
                ,
            bayesLinRegAlgo = read $ Xml.attrValue e "algo",
            bayesLinRegMethod = read $ Xml.attrValue e "method",
            bayesLinRegMethodParams = map Xml.fromElement (lefts (Xml.contents (Xml.contentElement e "methodparams"))),
            bayesLinRegCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }


data EnvParams = EnvParams {
    envUpperParams :: CommonParams,
    envLowerParams :: CommonParams,
    envMeanParams :: CommonParams,
    envMethod :: Bool,
    envStartExtrema :: Int,
    envData :: Maybe DataParams
} deriving (Show, Read)

instance Xml.XmlElement EnvParams where
    toElement params = Xml.element "envparams" 
        [("version", "1"),
         ("method", show (envMethod params)),
         ("startextrema", show (envStartExtrema params)) 
        ]
        (
            [Left (Xml.element "upper" [] [Left (Xml.toElement (envUpperParams params))]), 
            Left (Xml.element "lower" [] [Left (Xml.toElement (envLowerParams params))])] ++
            [Left (Xml.element "meanparams" [] [Left (Xml.toElement (envMeanParams params))])] ++
            [Left (Xml.element "data" [] (
                case envData params of
                    Just dataParams -> [Left (Xml.toElement dataParams)]
                    otherwise -> []
            ))]
        )
    
    fromElement e = case Xml.maybeAttrValue e "version" of
        Nothing -> newEnv
        Just _ -> 
            EnvParams {
                envUpperParams = Xml.fromElement $ Xml.contentElement (Xml.contentElement e "upper") commonParamsXmlElementName,
                envLowerParams = Xml.fromElement $ Xml.contentElement (Xml.contentElement e "lower") commonParamsXmlElementName,
                envMeanParams = Xml.fromElement (Xml.contentElement (Xml.contentElement e "meanparams") commonParamsXmlElementName),
                envMethod = read $ Xml.attrValue e "method",
                envStartExtrema = read $ Xml.attrValue e "startextrema",
                envData =
                    case Xml.contents $ Xml.contentElement e "data" of
                        [Left envDataElem] -> Just $ Xml.fromElement envDataElem
                        otherwise -> Nothing
            }

newBayesLinReg :: BayesLinRegParams
newBayesLinReg = BayesLinRegParams {
    bayesLinRegData = Nothing, 
    bayesLinRegAlgo = 0, 
    bayesLinRegMethod = 0, 
    bayesLinRegMethodParams = [RBFParams {rbfNumCentres = 10, rbfNumLambdas = 100}], 
    bayesLinRegCommonParams = CommonParams {
        commonName = "BayesLinReg",
        commonNo = 1
        }
    }

newEnv :: EnvParams
newEnv = EnvParams {
    envUpperParams = CommonParams {
        commonName = "UpperEnv",
        commonNo = 1
    },
    envLowerParams = CommonParams {
        commonName = "LowerEnv",
        commonNo = 1
    }, 
    envMethod = False,
    envStartExtrema = 1,
    envData = Nothing,
    envMeanParams = CommonParams {
        commonName = "EnvMean",
        commonNo = 1
    }
}

newFitParams :: String -> Int -> Int -> Double -> FitParams
newFitParams name numNodes polynomRank period = 
    FitParams {
        fitPolynomRank = polynomRank, 
        fitPeriod = period,
        fitNumHarmonics = 1,
        fitCommonParams = CommonParams {
            commonName = name,
            commonNo = 1
        },
        fitType = FitTypeSpline,
        fitSplineParams = SplineParams numNodes,
        fitHarmonicParams = HarmonicParams 1.2 0
    }
