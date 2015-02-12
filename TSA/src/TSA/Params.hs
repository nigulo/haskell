
module TSA.Params (
    Params (..), 
    CommonParams(..),
    FitParams (..),
    FitType(..), 
    SplineParams(..),
    HarmonicParams(..),
    LsqParams (..), 
    EnvParams (..), 
    SubDataParams (..),
    DataParams (..),
    ImfParams (..),
    FftParams (..),
    LocalPhaseParams (..),
    FindPeriodParams (..),
    D2Params(..),
    FindExtremaParams (..),
    FunctionParams (..),
    AnalyticSignalParams (..),
    ModifyParams(..),
    SelectionParams(..),
    CorrelationParams(..),
    StatisticParams(..),
    AttractorParams(..),
    SampleParams(..),
    BuildParams(..),
    InterpolateParams(..),
    ParamsRef,
    EnvExtrema (..),
    
    getStatisticByName,
    removeStatisticByName,
    addStatistic,    
    updateStatistic,    
    newParams,
    newFitParams,
    getNameWithNo,
    updateCommonParams,
    updateLsqParams,
    updateEnvParams,
    readParams,

    ProgressUpdateFunc,
    LogFunc,
    DataUpdateFunc (..)
    ) where

--import Graphics.UI.Gtk hiding (addWidget, Plus, Cross, Circle)
import Control.Concurrent.MVar
import Data.List
import Data.Word
import Data.Array
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace
import Regression.Data as D
import Regression.Spline as S
import Regression.Functions as F
import Regression.AnalyticData as A
import Regression.Utils as U
import Regression.Statistic
import Utils.Misc
import Utils.List
import System.Random

import Control.Concurrent
import qualified Utils.Xml as Xml


import Utils.Str
import Utils.Misc

data CommonParams = CommonParams {
    commonName :: String,
    commonNo :: Int
} deriving (Show, Read)

commonParamsXmlElementName :: String
commonParamsXmlElementName = "commonparams"

instance Xml.XmlElement CommonParams where
    toElement params = Xml.element commonParamsXmlElementName [
        ("name", commonName params), 
        ("no", show (commonNo params))
        ] []
        
    fromElement e =
        CommonParams {
            commonName = Xml.attrValue e "name",
            commonNo = read $ Xml.attrValue e "no"
        }

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

data EnvExtrema = EnvExtremaStrict | EnvExtremaStatistical deriving (Show, Read)

instance Xml.XmlElement EnvExtrema where
    toElement envExtrema = Xml.element "envextrema" [("type", show envExtrema)] []
    fromElement e = read $ Xml.attrValue e "type"

data EnvParams = EnvParams {
    envUpperParams :: FitParams,
    envLowerParams :: FitParams,
    envPrecision :: Double,
    envExtrema :: EnvExtrema,
    envMeanParams :: CommonParams,
    envData :: Maybe DataParams
} deriving (Show, Read)

instance Xml.XmlElement EnvParams where
    toElement params = Xml.element "envparams" 
        [("precision", show (envPrecision params)), 
         ("extrema", show (envExtrema params))
        ]
        (
            [Left (Xml.element "upper" [] [Left (Xml.toElement (envUpperParams params))]), 
            Left (Xml.element "lower" [] [Left (Xml.toElement (envLowerParams params))])] ++
            [Left (Xml.element "data" [] (
                case envData params of
                    Just dataParams -> [Left (Xml.toElement dataParams)]
                    otherwise -> []
            ))] ++
            [Left (Xml.element "meanparams" [] [Left (Xml.toElement (envMeanParams params))])]
        )
    
    fromElement e =
        EnvParams {
            envUpperParams = Xml.fromElement $ Xml.contentElement (Xml.contentElement e "upper") "fitparams",
            envLowerParams = Xml.fromElement $ Xml.contentElement (Xml.contentElement e "lower") "fitparams",
            envPrecision = read $ Xml.attrValue e "precision",
            envExtrema = read $ Xml.attrValue e "extrema",
            envMeanParams = Xml.fromElement (Xml.contentElement (Xml.contentElement e "meanparams") commonParamsXmlElementName),
            envData =
                case Xml.contents $ Xml.contentElement e "data" of
                    [Left envDataElem] -> Just $ Xml.fromElement envDataElem
                    otherwise -> Nothing
        }

-- | deprecated
data ImfParams = ImfParams {
    imfUpperParams :: FitParams,
    imfLowerParams :: FitParams,
    imfPrecision :: Double,
    imfData :: Maybe D.Data,
    imfCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement ImfParams where
    toElement params = Xml.element "imfparams" 
        [("precision", show (imfPrecision params))]
        (
            [Left (Xml.element "upper" [] [Left (Xml.toElement (imfUpperParams params))]), 
            Left (Xml.element "lower" [] [Left (Xml.toElement (imfLowerParams params))])] ++
            [Left (Xml.element "data" [] (
                case imfData params of
                    Just d -> [Left (Xml.toElement d)]
                    otherwise -> []
            ))] ++
            [Left (Xml.toElement (imfCommonParams params))]
        )
    
    fromElement e =
        ImfParams {
            imfUpperParams = Xml.fromElement $ Xml.contentElement (Xml.contentElement e "upper") "fitparams",
            imfLowerParams = Xml.fromElement $ Xml.contentElement (Xml.contentElement e "lower") "fitparams",
            imfPrecision = read $ Xml.attrValue e "precision",
            imfCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName),
            imfData =
                case Xml.contents $ Xml.contentElement e "data" of
                    [Left imfDataElem] -> Just $ Xml.fromElement imfDataElem
                    otherwise -> Nothing
        }

data FftParams = FftParams {
    fftDirection :: Bool,
    fftPhaseShift :: Double,
    fftRealData :: Maybe D.Data,
    fftImagData :: Maybe D.Data,
    fftCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement FftParams where
    toElement params = 
        Xml.element "fftparams" 
            [("direction", show (fftDirection params)),
             ("phaseshift", show (fftPhaseShift params))
            ] 
            (
                [Left (Xml.element "realdata" [] (
                    case fftRealData params of
                        Just d -> [Left (Xml.toElement d)]
                        otherwise -> []
                ))] ++
                [Left (Xml.element "imagdata" [] (
                    case fftImagData params of
                        Just d -> [Left (Xml.toElement d)]
                        otherwise -> []
                ))] ++
                [Left (Xml.toElement (fftCommonParams params))]
            )
    
    fromElement e = 
        FftParams {
            fftDirection = read $ Xml.attrValue e "direction",
            fftPhaseShift = read $ Xml.attrValue e "phaseshift",
            fftCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName),
            fftRealData =
                case Xml.contents $ Xml.contentElement e "realdata" of
                    [Left dataElem] -> Just $ Xml.fromElement dataElem
                    otherwise -> Nothing
                ,
            fftImagData =
                case Xml.contents $ Xml.contentElement e "imagdata" of
                    [Left dataElem] -> Just $ Xml.fromElement dataElem
                    otherwise -> Nothing
        }


data SubDataParams = SubDataParams {
    subDataRange :: ([Double], [Double]),
    subData :: Either D.Data (Either S.Spline F.Functions),
    subDataBootstrapSet :: [Either D.Data (Either S.Spline F.Functions)]
} deriving (Show, Read)

instance Xml.XmlElement SubDataParams where
    toElement params = Xml.element "subdataparams" 
        [("range", show (subDataRange params))]
        ([Left (Xml.element "data" [] [
            case subData params of
                Left d -> Left (Xml.toElement d) 
                Right (Left s) -> Left (Xml.toElement s)
                Right (Right e) -> Left (Xml.toElement e)
                ])] ++
            [Left (Xml.element "bootstrapset" [] (map (\ds ->
                        case ds of
                            Left d -> Left (Xml.toElement d) 
                            Right (Left s) -> Left (Xml.toElement s)
                            Right (Right e) -> Left (Xml.toElement e)
                            ) (subDataBootstrapSet params)))]
        )

    fromElement e = 
        SubDataParams {
            subDataRange = case Xml.maybeAttrValue e "range" of
                Just r -> read r
                Nothing -> ([0], [0]),
            subData =
                let 
                    Left dataSetElem = head $ Xml.contents $ Xml.contentElement e "data" 
                in
                    if Xml.name dataSetElem == D.xmlElementName then Left $ Xml.fromElement dataSetElem
                    else if Xml.name dataSetElem == S.xmlElementName then Right $ Left $ Xml.fromElement dataSetElem
                    else Right $ Right $ Xml.fromElement dataSetElem,
            subDataBootstrapSet =
                case Xml.maybeContentElement e "bootstrapset" of
                    Just elem -> map (\(Left dataSetElem) ->
                            if Xml.name dataSetElem == D.xmlElementName then Left $ Xml.fromElement dataSetElem
                            else if Xml.name dataSetElem == S.xmlElementName then Right $ Left $ Xml.fromElement dataSetElem
                            else Right $ Right $ Xml.fromElement dataSetElem
                        ) (Xml.contents elem)
                    Nothing -> []
        }

data DataParams = DataParams {
    dataName :: String,
    dataDesc :: String,
    dataSet :: [SubDataParams]
} deriving (Show, Read)

instance Xml.XmlElement DataParams where
    toElement params = Xml.element "dataparams" 
        [("version", "1"), ("name", dataName params), ("description", dataDesc params)]
        [Left (Xml.element "dataset" [] (map (\d -> Left (Xml.toElement d)) (dataSet params)))]


    fromElement e = 
        DataParams {
            dataName = Xml.attrValue e "name",
            dataDesc = case Xml.maybeAttrValue e "description" of
                Just desc -> desc
                Nothing -> Xml.attrValue e "name",
            dataSet = 
                    case Xml.maybeAttrValue e "version" of
                        Just "1" ->
                            map (\(Left elem) -> Xml.fromElement elem) (Xml.contents (Xml.contentElement e "dataset"))
                        Nothing -> 
                            [SubDataParams {subDataRange = U.dataRange sd, subData = sd, subDataBootstrapSet = []}] where
                                sd =
                                    let 
                                        Left dataSetElem = head $ Xml.contents $ Xml.contentElement e "dataset" 
                                    in
                                        if Xml.name dataSetElem == D.xmlElementName then Left $ Xml.fromElement dataSetElem
                                        else if Xml.name dataSetElem == S.xmlElementName then Right $ Left $ Xml.fromElement dataSetElem
                                        else Right $ Right $ Xml.fromElement dataSetElem
                
        }
        
data AnalyticSignalParams = AnalyticSignalParams {
    asAmplitudeParams :: CommonParams,
    asPhaseParams :: CommonParams,
    asFrequencyParams :: CommonParams,
    asRealData :: Maybe DataParams,
    asImagData :: Maybe DataParams
} deriving (Show, Read)

instance Xml.XmlElement AnalyticSignalParams where
    toElement params = 
        Xml.element "analyticsignalparams" 
            [] 
            (
                [Left (Xml.element "realdata" [] (
                    case asRealData params of
                        Just dataParams -> [Left (Xml.toElement dataParams)]
                        otherwise -> []
                ))] ++
                [Left (Xml.element "imagdata" [] (
                    case asImagData params of
                        Just dataParams -> [Left (Xml.toElement dataParams)]
                        otherwise -> []
                ))] ++
                [Left (Xml.element "amplitudeparams" [] [Left (Xml.toElement (asAmplitudeParams params))]),
                 Left (Xml.element "phaseparams" [] [Left (Xml.toElement (asPhaseParams params))]),
                 Left (Xml.element "frequencyparams" [] [Left (Xml.toElement (asFrequencyParams params))])
                ]
            )
    
    fromElement e = 
        AnalyticSignalParams {
            asRealData =
                case Xml.contents $ Xml.contentElement e "realdata" of
                    [Left dataParamsElem] -> Just $ Xml.fromElement dataParamsElem
                    otherwise -> Nothing,
            asImagData =
                case Xml.contents $ Xml.contentElement e "imagdata" of
                    [Left dataParamsElem] -> Just $ Xml.fromElement dataParamsElem
                    otherwise -> Nothing,
            asAmplitudeParams = Xml.fromElement (Xml.contentElement (Xml.contentElement e "amplitudeparams") commonParamsXmlElementName),
            asPhaseParams = Xml.fromElement (Xml.contentElement (Xml.contentElement e "phaseparams") commonParamsXmlElementName),
            asFrequencyParams = Xml.fromElement (Xml.contentElement (Xml.contentElement e "frequencyparams") commonParamsXmlElementName)
        }

data LocalPhaseParams = LocalPhaseParams {
    localPhaseData :: Maybe DataParams,
    localPhasePeriod :: Double,
    localPhaseMaxPeriod :: Double,
    localPhasePrecision :: Int,
    localPhaseEpoch :: Double,
    localPhaseCalcColorMap :: Bool,
    localPhaseCalcPhaseDisp :: Bool,
    localPhaseAvgOverCycles :: Int,
    localPhaseCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement LocalPhaseParams where

    toElement params = Xml.element "localphaseparams" 
        [("period", show (localPhasePeriod params)),
         ("maxperiod", show (localPhaseMaxPeriod params)),
         ("precision", show (localPhasePrecision params)),
         ("epoch", show (localPhaseEpoch params)),
         ("calccolormap", show (localPhaseCalcColorMap params)),
         ("calcphasedisp", show (localPhaseCalcPhaseDisp params)),
         ("avegovercycles", show (localPhaseAvgOverCycles params))
        ]
        (
            case localPhaseData params of
                Just dataParams -> [Left (Xml.toElement dataParams)]
                Nothing -> []
            ++ [Left (Xml.toElement (localPhaseCommonParams params))]
        )
        
    fromElement e = 
        LocalPhaseParams {
            localPhaseData =
                case Xml.contentElements e "dataparams" of
                    [dataParams] -> Just $ Xml.fromElement dataParams
                    otherwise -> Nothing
                ,
            localPhasePeriod = read $ Xml.attrValue e "period",
            localPhaseMaxPeriod = case Xml.maybeAttrValue e "maxperiod" of
                Just maxPeriod -> read maxPeriod
                Nothing -> 0, 
            localPhasePrecision = read $ Xml.attrValue e "precision",
            localPhaseEpoch = read $ Xml.attrValue e "epoch",
            localPhaseCalcColorMap = case Xml.maybeAttrValue e "calccolormap" of
                Just calcColorMap -> read calcColorMap
                Nothing -> True, 
            localPhaseCalcPhaseDisp = case Xml.maybeAttrValue e "calcphasedisp" of
                Just calcPhaseDisp -> read calcPhaseDisp
                Nothing -> False, 
            localPhaseAvgOverCycles = case Xml.maybeAttrValue e "avegovercycles" of
                Just avgOverCycles -> read avgOverCycles
                Nothing -> 100, 
            localPhaseCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data FindPeriodParams = FindPeriodParams {
    findPeriodData :: Maybe DataParams,
    findPeriodMethod :: Int,
    findPeriodStart :: Double,
    findPeriodEnd :: Double,
    findPeriodPrecision :: Int,
    findPeriodCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement FindPeriodParams where

    toElement params = Xml.element "findperiodparams" 
        [("method", show (findPeriodMethod params)),
         ("periodstart", show (findPeriodStart params)),
         ("periodend", show (findPeriodEnd params)),        
         ("precision", show (findPeriodPrecision params))
        ]
        (
            case findPeriodData params of
                Just dataParams -> [Left (Xml.toElement dataParams)]
                Nothing -> []
            ++ [Left (Xml.toElement (findPeriodCommonParams params))]
        )
        
    fromElement e = 
        FindPeriodParams {
            findPeriodData =
                case Xml.contentElements e "dataparams" of
                    [dataParams] -> Just $ Xml.fromElement dataParams
                    otherwise -> Nothing
                ,
            findPeriodMethod = case Xml.maybeAttrValue e "method" of
                Just method -> read method
                Nothing -> 0,
            findPeriodStart = read $ Xml.attrValue e "periodstart",
            findPeriodEnd = read $ Xml.attrValue e "periodend",
            findPeriodPrecision = read $ Xml.attrValue e "precision",
            findPeriodCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data D2Params = D2Params {
    d2Data :: Maybe DataParams,
    d2PeriodStart :: Double,
    d2PeriodEnd :: Double,
    d2CorrLenStart :: Double,
    d2CorrLenEnd :: Double,
    d2Method :: Int,
    d2Precision :: Int,
    d2CommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement D2Params where

    toElement params = Xml.element "findperiodparams" 
        [("periodstart", show (d2PeriodStart params)),
         ("periodend", show (d2PeriodEnd params)),  
         ("corrlenstart", show (d2CorrLenStart params)),
         ("corrlenend", show (d2CorrLenEnd params)),
         ("method", show (d2Method params)),
         ("precision", show (d2Precision params))
        ]
        (
            case d2Data params of
                Just dataParams -> [Left (Xml.toElement dataParams)]
                Nothing -> []
            ++ [Left (Xml.toElement (d2CommonParams params))]
        )
        
    fromElement e = 
        D2Params {
            d2Data =
                case Xml.contentElements e "dataparams" of
                    [dataParams] -> Just $ Xml.fromElement dataParams
                    otherwise -> Nothing
                ,
            d2PeriodStart = read $ Xml.attrValue e "periodstart",
            d2PeriodEnd = read $ Xml.attrValue e "periodend",
            d2CorrLenStart = read $ Xml.attrValue e "corrlenstart",
            d2CorrLenEnd = read $ Xml.attrValue e "corrlenend",
            d2Method = read $ Xml.attrValue e "method",
            d2Precision = read $ Xml.attrValue e "precision",
            d2CommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data FindExtremaParams = FindExtremaParams {
    findExtremaData :: Maybe DataParams,
    findExtremaPrecision :: Int,
    findExtremaCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement FindExtremaParams where

    toElement params = Xml.element "findextremaparams" 
        [("precision", show (findExtremaPrecision params))]
        (
            case findExtremaData params of
                Just dataParams -> [Left (Xml.toElement dataParams)]
                Nothing -> []
            ++ [Left (Xml.toElement (findExtremaCommonParams params))]
        )
        
    fromElement e = 
        FindExtremaParams {
            findExtremaData =
                case Xml.contentElements e "dataparams" of
                    [dataParams] -> Just $ Xml.fromElement dataParams
                    otherwise -> Nothing
                ,
            findExtremaPrecision = read $ Xml.attrValue e "precision",
            findExtremaCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data FunctionParams = FunctionParams {
    functionDefinition :: String,
    functionLeft :: Maybe Double,
    functionRight :: Maybe Double,
    functionBottom :: Maybe Double,
    functionTop :: Maybe Double,
    functionCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement FunctionParams where

    toElement params = Xml.element "functionparams" 
        [("function", functionDefinition params),
         ("left", show (functionLeft params)),        
         ("right", show (functionRight params)),
         ("bottom", show (functionBottom params)),        
         ("top", show (functionTop params))
        ] [Left (Xml.toElement (functionCommonParams params))]

        
    fromElement e = 
        FunctionParams {
            functionDefinition = Xml.attrValue e "function",
            functionLeft = read $ Xml.attrValue e "left",
            functionRight = read $ Xml.attrValue e "right",
            functionBottom = case Xml.maybeAttrValue e "bottom" of
                Just bottom -> read bottom
                Nothing -> Nothing, 
            functionTop = case Xml.maybeAttrValue e "top" of
                Just top -> read top
                Nothing -> Nothing, 
            functionCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data ModifyParams = ModifyParams {
    modifyOp :: Int,
    modifyType :: Int,
    modifyConstant :: Double,
    modifyCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement ModifyParams where

    toElement params = Xml.element "modifyparams" 
        [("op", show (modifyOp params)),
         ("type", show (modifyType params)),        
         ("constant", show (modifyConstant params))
        ] [Left (Xml.toElement (modifyCommonParams params))]
        
    fromElement e = 
        ModifyParams {
            modifyOp = read $ Xml.attrValue e "op",
            modifyType = read $ Xml.attrValue e "type",
            modifyConstant = read $ Xml.attrValue e "constant",
            modifyCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data SelectionParams = SelectionParams {
    selectionOp :: Int,
    selectionModifyOriginal :: Bool,
    selectionCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement SelectionParams where

    toElement params = Xml.element "selectionparams" 
        [("op", show (selectionOp params)),
         ("modifyoriginal", show (selectionModifyOriginal params))
        ] 
        [Left (Xml.toElement (selectionCommonParams params))]
        
    fromElement e = 
        SelectionParams {
            selectionOp = read $ Xml.attrValue e "op",
            selectionModifyOriginal = read $ Xml.attrValue e "modifyoriginal",
            selectionCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data CorrelationParams = CorrelationParams {
    correlationPrecision :: Int,
    correlationCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement CorrelationParams where

    toElement params = Xml.element "correlationparams" 
        [("precision", show (correlationPrecision params))
        ] [Left (Xml.toElement (correlationCommonParams params))]
        
    fromElement e = 
        CorrelationParams {
            correlationPrecision = read $ Xml.attrValue e "precision",
            correlationCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data StatisticParams = StatisticParams {
    statisticDefinition :: String,
    statisticVarValsDef :: String,
    statisticCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement StatisticParams where

    toElement params = Xml.element "statisticparams" 
        [("definition", statisticDefinition params),
         ("varvalsdef", statisticVarValsDef params)
        ] [Left (Xml.toElement (statisticCommonParams params))]

        
    fromElement e = 
        StatisticParams {
            statisticDefinition = Xml.attrValue e "definition",
            statisticVarValsDef = case Xml.maybeAttrValue e "varvalsdef" of
                Just varValsDef -> varValsDef
                Nothing -> "left, right, 1, x",
            statisticCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data AttractorParams = AttractorParams {
    attractorData :: Maybe DataParams,
    attractorDimension :: Int,
    attractorCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement AttractorParams where

    toElement params = Xml.element "attractorparams" 
        [("dimension", show (attractorDimension params))
        ]
        (
            case attractorData params of
                Just dataParams -> [Left (Xml.toElement dataParams)]
                Nothing -> []
            ++ [Left (Xml.toElement (attractorCommonParams params))]
        )
        
    fromElement e = 
        AttractorParams {
            attractorData =
                case Xml.contentElements e "dataparams" of
                    [dataParams] -> Just $ Xml.fromElement dataParams
                    otherwise -> Nothing
                ,
            attractorDimension = read $ Xml.attrValue e "dimension",
            attractorCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data SampleParams = SampleParams {
    sampleCount :: Int,
    sampleRandomness :: Int,
    sampleCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement SampleParams where

    toElement params = Xml.element "sampleparams" 
        [("count", show (sampleCount params)),
         ("randomness", show (sampleRandomness params))
        ] [Left (Xml.toElement (sampleCommonParams params))]
        
    fromElement e = 
        SampleParams {
            sampleCount = read $ Xml.attrValue e "count",
            sampleRandomness = read $ Xml.attrValue e "randomness",
            sampleCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data BuildParams = BuildParams {
    buildCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement BuildParams where

    toElement params = Xml.element "buildparams" 
        [] [Left (Xml.toElement (buildCommonParams params))]
        
    fromElement e = 
        BuildParams {
            buildCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data InterpolateParams = InterpolateParams {
    interpolateCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement InterpolateParams where

    toElement params = Xml.element "interpolateparams" 
        [] [Left (Xml.toElement (interpolateCommonParams params))]
        
    fromElement e = 
        InterpolateParams {
            interpolateCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

{-
data BootstrapStatistic = LeastSquaresFit deriving (Show, Read)

data BootstrapParams = BootstrapParams {
    bootstrapData :: Maybe DataParams,
    bootstrapStatistic :: BootstrapStatistic,
    bootstrapCount :: Int
} deriving (Show, Read)

instance Xml.XmlElement BootstrapParams where

    toElement params = Xml.element "bootstrapparams" 
        [("count", show (bootstrapCount params)),
        ("statistic", show (bootstrapStatistic params))
        ]
        (
            [Left (Xml.element "data" [] (
                case bootstrapData params of
                    Just dataParams -> [Left (Xml.toElement dataParams)]
                    otherwise -> []
            ))]
        )
        
    fromElement e = 
        BootstrapParams {
            bootstrapCount = read $ Xml.attrValue e "count",
            bootstrapStatistic = read $ Xml.attrValue e "statistic",
            bootstrapData =
                case Xml.contents $Xml.contentElement e "data" of
                    [Left dataParamsElem] -> Just $ Xml.fromElement dataParamsElem
                    otherwise -> Nothing
        }

--}


data Params = Params {
    dataParams :: [DataParams],
    lsqParams :: LsqParams,
    envParams :: EnvParams,
    imfParams :: ImfParams,
    fftParams :: FftParams,
    asParams :: AnalyticSignalParams,
    localPhaseParams :: LocalPhaseParams,
    findPeriodParams :: FindPeriodParams,
    d2Params :: D2Params,
    findExtremaParams :: FindExtremaParams,
    functionParams :: FunctionParams,
    modifyParams :: ModifyParams,
    selectionParams :: SelectionParams,
    correlationParams :: CorrelationParams,
    attractorParams :: AttractorParams,
    sampleParams :: SampleParams,
    buildParams :: BuildParams,
    interpolateParams :: InterpolateParams,
    statisticParams :: [StatisticParams]
}

instance Xml.XmlElement Params where
    toElement state = Xml.element "tsaparams" [("version", "1")] 
        ((map (\dp -> Left (Xml.toElement dp)) (dataParams state)) ++ 
        [Left (Xml.toElement (lsqParams state)),
         Left (Xml.toElement (envParams state)),
         Left (Xml.toElement (imfParams state)),
         Left (Xml.toElement (fftParams state)),
         Left (Xml.toElement (asParams state)),
         Left (Xml.toElement (localPhaseParams state)),
         Left (Xml.toElement (findPeriodParams state)),
         Left (Xml.toElement (d2Params state)),
         Left (Xml.toElement (functionParams state)),
         Left (Xml.toElement (modifyParams state)),
         Left (Xml.toElement (selectionParams state)),
         Left (Xml.toElement (correlationParams state)),
         Left (Xml.toElement (attractorParams state)),
         Left (Xml.toElement (sampleParams state)),
         Left (Xml.toElement (buildParams state)),
         Left (Xml.toElement (interpolateParams state))
        ] ++
         (map (\(i, sp) -> Left (Xml.element "statistics" [("index", show i)] [Left (Xml.toElement sp)])) (zip [0 .. length (statisticParams state) - 1] (statisticParams state)))
        )

    fromElement e = 
        let
            version = maybe_ "0" $ Xml.maybeAttrValue e "version"
        in
            Params {
                dataParams = map Xml.fromElement $ Xml.contentElements e "dataparams",
                lsqParams = Xml.fromElement $ Xml.contentElement e "lsqparams",
                envParams = Xml.fromElement $ Xml.contentElement e "envparams",
                imfParams = Xml.fromElement $ Xml.contentElement e "imfparams",
                fftParams = Xml.fromElement $ Xml.contentElement e "fftparams",
                asParams = Xml.fromElement $ Xml.contentElement e "analyticsignalparams",
                localPhaseParams = 
                    (case Xml.maybeContentElement e "localphaseparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> Xml.fromElement $ Xml.contentElement e "flipflopparams"
                        ),
                findPeriodParams = 
                    (case Xml.maybeContentElement e "findperiodparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newFindPeriod),
                d2Params = 
                    (case Xml.maybeContentElement e "d2params" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newD2),
                findExtremaParams = 
                    (case Xml.maybeContentElement e "findextremaparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newFindExtrema),
                functionParams =
                    (case Xml.maybeContentElement e "functionparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newFunction
                        ),
                modifyParams =
                    (case Xml.maybeContentElement e "modifyparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newModify
                        ),
                selectionParams =
                    (case Xml.maybeContentElement e "selectionparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newSelection
                        ),
                correlationParams =
                    (case Xml.maybeContentElement e "correlationparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newCorrelation
                        ),
                attractorParams = 
                    (case Xml.maybeContentElement e "attractorparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newAttractor
                        ),
                sampleParams =
                    (case Xml.maybeContentElement e "sampleparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newSample
                        ),
                buildParams = 
                    (case Xml.maybeContentElement e "buildparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newBuild
                        ),
                interpolateParams =
                    (case Xml.maybeContentElement e "interpolateparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newInterpolate
                        ),
                statisticParams =
                    (case Xml.maybeContentElement e "statisticparams" of
                        Just e ->
                            -- old format
                            map (\(name, definition) -> newStatistic (Just name) (Just definition)) $ map (\e -> (Xml.attrValue e "name", Xml.attrValue e "definition")) (Xml.contentElements e "statistic") 
                        Nothing -> 
                            let 
                                statisticElements = Xml.contentElements e "statistics"
                            in
                                if length statisticElements > 0 
                                    then
                                        let
                                            sts :: Map.Map Int StatisticParams = 
                                                    Map.fromList $ map 
                                                        (\spElem -> (read (Xml.attrValue spElem "index"), 
                                                            Xml.fromElement (Xml.contentElement spElem "statisticparams"))) statisticElements
                                        in
                                            snd $ unzip $ Map.toAscList sts
                                    else
                                        [newStatistic Nothing Nothing]

                        )
            }

instance Show Params where
    show s =
        "1\n" ++ --version 
        (show (dataParams s)) ++ "\n" ++
        (show (lsqParams s)) ++ "\n" ++
        (show (envParams s)) ++ "\n" ++
        (show (imfParams s)) ++ "\n" ++
        (show (fftParams s)) ++ "\n" ++
        (show (asParams s)) ++ "\n" ++
        (show (localPhaseParams s)) ++ "\n" ++
        (show (findPeriodParams s)) ++ "\n" ++
        (show (d2Params s)) ++ "\n" ++
        (show (findExtremaParams s)) ++ "\n" ++
        (show (functionParams s)) ++ "\n" ++
        (show (modifyParams s)) ++ "\n" ++
        (show (selectionParams s)) ++ "\n" ++
        (show (correlationParams s)) ++ "\n" ++
        (show (attractorParams s)) ++ "\n" ++
        (show (sampleParams s)) ++ "\n" ++
        (show (buildParams s)) ++ "\n" ++
        (show (interpolateParams s)) ++ "\n" ++
        (show (statisticParams s)) ++ "\n"
        
readParams :: ParamsRef -> String -> IO ()
readParams stateRef s =
    do
        let 
            state = (Xml.fromDocument (Xml.parse "tsastate" s))
        modifyMVar_ stateRef $ \_ -> return state
        

type ParamsRef = MVar Params

updateStatistic :: StatisticParams -> Params -> Params
updateStatistic statistic state =
    let
        statisticParms = statisticParams state
    in
        state {
            
            statisticParams = case findIndex (\sp -> (commonName . statisticCommonParams) sp == (commonName . statisticCommonParams) statistic) statisticParms of
                Just i -> (take i statisticParms) ++ [statistic] ++ (drop (i + 1) statisticParms)
                Nothing -> (init statisticParms) ++ [statistic, last statisticParms]
        }

addStatistic :: String -> String -> Params -> Params
addStatistic name definition state =
    let
        statisticParms = statisticParams state
    in
        case findIndex (\sp -> (commonName . statisticCommonParams) sp == name) (init statisticParms) of
            Nothing -> 
                state {
                    statisticParams = (init statisticParms) ++ [newStatistic (Just name) (Just definition)] ++ [last statisticParms]
                    }
            otherwise -> state

getStatisticByName :: String -> Params -> StatisticParams
getStatisticByName name state = 
    let
        Just sp = find (\sp -> (commonName . statisticCommonParams) sp == name) (statisticParams state)
    in
        sp

removeStatisticByName :: String -> Params -> Params
removeStatisticByName name state =
    let
        statisticParms = statisticParams state
    in
        state {statisticParams = filter (\sp -> (commonName . statisticCommonParams) sp /= name) statisticParms}

newParams :: Params
newParams =
    Params {dataParams = [],
            lsqParams = LsqParams {
                lsqFitParams = newFitParams "LsqFit" 3 3 0, 
                lsqBootstrapCount = 0
                },
            envParams = EnvParams {
                envUpperParams = newFitParams "UpperEnv" 3 3 0,
                envLowerParams = newFitParams "LowerEnv" 3 3 0,
                envPrecision = 10,
                envExtrema = EnvExtremaStrict,
                envData = Nothing,
                envMeanParams = CommonParams {
                    commonName = "EnvMean",
                    commonNo = 1
                }
            },
            imfParams = ImfParams {
                imfUpperParams = newFitParams "" 3 3 0,
                imfLowerParams = newFitParams "" 3 3 0,
                imfPrecision = 10,
                imfData = Nothing,
                imfCommonParams = CommonParams {
                    commonName = "IMF",
                    commonNo = 1
                }
            },
            fftParams = FftParams {
                fftDirection = True,
                fftPhaseShift = 0,
                fftRealData = Nothing,
                fftImagData = Nothing,
                fftCommonParams = CommonParams {
                    commonName = "FFT",
                    commonNo = 1
                }
            },
            asParams = AnalyticSignalParams {
                asRealData = Nothing,
                asImagData = Nothing,
                asAmplitudeParams = CommonParams {
                    commonName = "Amplitude",
                    commonNo = 1
                },
                asPhaseParams = CommonParams {
                    commonName = "Phase",
                    commonNo = 1
                },
                asFrequencyParams = CommonParams {
                    commonName = "Frequency",
                    commonNo = 1
                }
            },
            localPhaseParams = newLocalPhase,
            findPeriodParams = newFindPeriod,
            d2Params = newD2,
            findExtremaParams = newFindExtrema,
            functionParams = newFunction,
            modifyParams = newModify,
            selectionParams = newSelection,
            correlationParams = newCorrelation,
            attractorParams = newAttractor,
            sampleParams = newSample,
            buildParams = newBuild,
            interpolateParams = newInterpolate,
            statisticParams = [newStatistic Nothing Nothing]
    }

newFindPeriod :: FindPeriodParams
newFindPeriod = FindPeriodParams {
    findPeriodData = Nothing, 
    findPeriodMethod = 0, 
    findPeriodStart = 1, 
    findPeriodEnd = 10, 
    findPeriodPrecision = 10000,
    findPeriodCommonParams = CommonParams {
        commonName = "Period",
        commonNo = 1
        }
    }

newD2 :: D2Params
newD2 = D2Params {
    d2Data = Nothing, 
    d2PeriodStart = 1, 
    d2PeriodEnd = 10, 
    d2CorrLenStart = 1, 
    d2CorrLenEnd = 100, 
    d2Method = 0,
    d2Precision = 10000,
    d2CommonParams = CommonParams {
        commonName = "D2",
        commonNo = 1
        }
    }

newLocalPhase :: LocalPhaseParams
newLocalPhase =     
    LocalPhaseParams {
        localPhaseData = Nothing, 
        localPhasePeriod = 0, 
        localPhaseMaxPeriod = 0, 
        localPhasePrecision = 100,
        localPhaseEpoch = 0,
        localPhaseCalcColorMap = True,
        localPhaseCalcPhaseDisp = False,
        localPhaseAvgOverCycles = 100,
        localPhaseCommonParams = CommonParams {
            commonName = "LocalPhase",
            commonNo = 1
        }
    }

newFindExtrema :: FindExtremaParams
newFindExtrema = FindExtremaParams {
    findExtremaData = Nothing, 
    findExtremaPrecision = 10000,
    findExtremaCommonParams = CommonParams {
        commonName = "Extrema",
        commonNo = 1
        }
    }

newFunction :: FunctionParams
newFunction = 
    FunctionParams {
        functionDefinition = "sin(x)",
        functionLeft = Nothing, 
        functionRight = Nothing,
        functionBottom = Nothing, 
        functionTop = Nothing,
        functionCommonParams = CommonParams {
            commonName = "Function",
            commonNo = 1
        }
}

newModify :: ModifyParams
newModify = 
    ModifyParams {
        modifyOp = 0,
        modifyType = 0,
        modifyConstant = 0,
        modifyCommonParams = CommonParams {
            commonName = "Modify",
            commonNo = 1
        }
    }

newSelection :: SelectionParams
newSelection = 
    SelectionParams {
        selectionOp = 0,
        selectionModifyOriginal = False,
        selectionCommonParams = CommonParams {
            commonName = "_sel",
            commonNo = 1
        }
    }

newCorrelation :: CorrelationParams
newCorrelation = 
    CorrelationParams {
        correlationPrecision = 10000,
        correlationCommonParams = CommonParams {
            commonName = "Correlation",
            commonNo = 1
        }
    }

newAttractor :: AttractorParams
newAttractor =
    AttractorParams {
        attractorData = Nothing, 
        attractorDimension = 2, 
        attractorCommonParams = CommonParams {
            commonName = "Attractor",
            commonNo = 1
        }
    }

newSample :: SampleParams
newSample = 
    SampleParams {
        sampleCount = 1000,
        sampleRandomness = 0,
        sampleCommonParams = CommonParams {
            commonName = "Sample",
            commonNo = 1
        }
    }

newBuild :: BuildParams
newBuild =
    BuildParams {
        buildCommonParams = CommonParams {
            commonName = "Build",
            commonNo = 1
        }
    }
     
newInterpolate :: InterpolateParams
newInterpolate =
    InterpolateParams {                            
        interpolateCommonParams = CommonParams {
            commonName = "Interpolate",
            commonNo = 1
        }
    }

newStatistic :: Maybe String -> Maybe String -> StatisticParams
newStatistic maybeName maybeDefinition =
    let 
        name = 
            case maybeName of 
                Just n -> n
                Nothing -> "Statistic"
        definition = 
            case maybeDefinition of 
                Just d -> d
                Nothing -> "sum(i,0,getlength()-1,1,getxat(i))/getlength()"
    in
        StatisticParams {
            statisticDefinition = definition, 
            statisticVarValsDef = "left, right, 1, x", 
            statisticCommonParams = CommonParams {
                commonName = name,
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

getNameWithNo :: CommonParams -> String
getNameWithNo commonParams = commonName commonParams ++ (show (commonNo commonParams))

updateCommonParams :: String -> CommonParams -> CommonParams
updateCommonParams name commonParams =
    let 
        no = commonNo commonParams
        (newName, newNo) = 
            if name == (commonName commonParams) ++ (show no) 
            then (commonName commonParams, no + 1) 
            else (name, no)
    in
        commonParams {
            commonName = newName,
            commonNo = newNo
        }
  
updateLsqParams :: LsqParams -> Params -> Params
updateLsqParams lsqParms state = 
    state {lsqParams = lsqParms}

updateEnvParams :: EnvParams -> Params -> Params
updateEnvParams envParms state = 
    state {envParams = envParms}

type ProgressUpdateFunc = Double -> IO ()
type LogFunc = String -> IO ()
newtype (Eq id) => DataUpdateFunc id = DataUpdateFunc (Either D.Data (Either S.Spline F.Functions) -> id -> Bool -> IO ())

