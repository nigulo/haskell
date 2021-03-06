
module TSA.Params (
    Params (..), 
    FftParams (..),
    LocalPhaseParams (..),
    FindPeriodParams (..),
    D2Params(..),
    SpecificPointsParams (..),
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
    
    getStatisticByName,
    removeStatisticByName,
    addStatistic,    
    updateStatistic,    
    newParams,
    newFitParams,
    getNameWithNo,
    updateLsqParams,
    updateEnvParams,
    readParams,
    unboxSubData,

    TaskEnv (..),
    defaultTaskEnv,
    DataUpdateFunc (..)
    ) where

import TSA.CommonParams
import TSA.RegressionParams
import Math.Function as Fn
import Regression.Data as D
import Regression.Statistic
import Utils.Misc
import Utils.List

import Control.Concurrent.MVar
import Data.List
import Data.Word
import Data.Array
import Data.Maybe
import qualified Data.Map.Strict as Map
import Debug.Trace
import System.Random
import Control.Concurrent
import qualified Utils.Xml as Xml


import Utils.Str
import Utils.Misc

data FftParams = FftParams {
    fftDirection :: Bool,
    fftPhaseShift :: Double,
    fftRealData :: Maybe D.Data,
    fftImagData :: Maybe D.Data,
    fftCalcPower :: Bool,
    fftCalcReAndIm :: Bool,
    fftCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement FftParams where
    toElement params = 
        Xml.element "fftparams" 
            [("direction", show (fftDirection params)),
             ("phaseshift", show (fftPhaseShift params)),
             ("calcpower", show (fftCalcPower params)),
             ("calcreandim", show (fftCalcReAndIm params))
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
            fftCalcPower = case Xml.maybeAttrValue e "calcpower" of
                Just calcPower -> read calcPower
                Nothing -> True,
            fftCalcReAndIm = case Xml.maybeAttrValue e "calcreandim" of
                Just calcReAndIm -> read calcReAndIm
                Nothing -> False,
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
    d2Normalize :: Bool,
    d2Precision :: Int,
    d2CommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement D2Params where

    toElement params = Xml.element "d2params" 
        [("periodstart", show (d2PeriodStart params)),
         ("periodend", show (d2PeriodEnd params)),  
         ("corrlenstart", show (d2CorrLenStart params)),
         ("corrlenend", show (d2CorrLenEnd params)),
         ("method", show (d2Method params)),
         ("normalize", show (d2Normalize params)),
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
            d2Normalize = case Xml.maybeAttrValue e "normalize" of
                Just normalize -> read normalize
                Nothing -> True,
            d2Precision = read $ Xml.attrValue e "precision",
            d2CommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
        }

data SpecificPointsParams = SpecificPointsParams {
    specificPointsData :: Maybe DataParams,
    specificPointsType :: Int,
    specificPointsPrecision :: Int,
    specificPointsCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement SpecificPointsParams where

    toElement params = Xml.element "specificpointsparams" 
        [("precision", show (specificPointsPrecision params)),
         ("type", show (specificPointsType params))]
        (
            case specificPointsData params of
                Just dataParams -> [Left (Xml.toElement dataParams)]
                Nothing -> []
            ++ [Left (Xml.toElement (specificPointsCommonParams params))]
        )
        
    fromElement e = 
        SpecificPointsParams {
            specificPointsData =
                case Xml.contentElements e "dataparams" of
                    [dataParams] -> Just $ Xml.fromElement dataParams
                    otherwise -> Nothing,
            specificPointsType = case Xml.maybeAttrValue e "type" of
                Just t -> read t
                Nothing -> 0,
            specificPointsPrecision = read $ Xml.attrValue e "precision",
            specificPointsCommonParams = Xml.fromElement (Xml.contentElement e commonParamsXmlElementName)
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
    sampleType :: Bool,
    sampleCommonParams :: CommonParams
} deriving (Show, Read)

instance Xml.XmlElement SampleParams where

    toElement params = Xml.element "sampleparams" 
        [("count", show (sampleCount params)),
         ("randomness", show (sampleRandomness params)),
         ("type", show (sampleType params))
        ] [Left (Xml.toElement (sampleCommonParams params))]
        
    fromElement e = 
        SampleParams {
            sampleCount = read $ Xml.attrValue e "count",
            sampleRandomness = read $ Xml.attrValue e "randomness",
            sampleType = case Xml.maybeAttrValue e "type" of
                Just t -> read t
                Nothing -> True,
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
    bayesLinRegParams :: BayesLinRegParams,
    envParams :: EnvParams,
    fftParams :: FftParams,
    asParams :: AnalyticSignalParams,
    localPhaseParams :: LocalPhaseParams,
    findPeriodParams :: FindPeriodParams,
    d2Params :: D2Params,
    specificPointsParams :: SpecificPointsParams,
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
         Left (Xml.toElement (bayesLinRegParams state)),
         Left (Xml.toElement (envParams state)),
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
                bayesLinRegParams = 
                    (case Xml.maybeContentElement e "bayeslinregparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> newBayesLinReg
                        ),
                envParams = Xml.fromElement $ Xml.contentElement e "envparams",
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
                specificPointsParams = 
                    case Xml.maybeContentElement e "specificpointsparams" of
                        Just e1 -> Xml.fromElement e1
                        Nothing -> 
                            case Xml.maybeContentElement e "findextremaparams" of
                                Just e1 -> Xml.fromElement e1
                                Nothing -> newSpecificPoints,
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
        (show (bayesLinRegParams s)) ++ "\n" ++
        (show (envParams s)) ++ "\n" ++
        (show (fftParams s)) ++ "\n" ++
        (show (asParams s)) ++ "\n" ++
        (show (localPhaseParams s)) ++ "\n" ++
        (show (findPeriodParams s)) ++ "\n" ++
        (show (d2Params s)) ++ "\n" ++
        (show (specificPointsParams s)) ++ "\n" ++
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
            bayesLinRegParams = newBayesLinReg,
            envParams = newEnv,
            fftParams = newFft,
            asParams = newAnalyticSignal,
            localPhaseParams = newLocalPhase,
            findPeriodParams = newFindPeriod,
            d2Params = newD2,
            specificPointsParams = newSpecificPoints,
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

newFft :: FftParams
newFft = FftParams {
        fftDirection = True,
        fftPhaseShift = 0,
        fftCalcPower = True,
        fftCalcReAndIm = False,                
        fftRealData = Nothing,
        fftImagData = Nothing,
        fftCommonParams = CommonParams {
            commonName = "FFT",
            commonNo = 1
        }
    }

newAnalyticSignal :: AnalyticSignalParams
newAnalyticSignal = AnalyticSignalParams {
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
    d2Normalize = True,
    d2Precision = 100,
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

newSpecificPoints :: SpecificPointsParams
newSpecificPoints = SpecificPointsParams {
    specificPointsData = Nothing,
    specificPointsType = 0, 
    specificPointsPrecision = 10000,
    specificPointsCommonParams = CommonParams {
        commonName = "SpecificPoints",
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
        sampleCount = 1024,
        sampleRandomness = 0,
        sampleType = True,
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

getNameWithNo :: CommonParams -> String
getNameWithNo commonParams = commonName commonParams ++ (show (commonNo commonParams))

updateLsqParams :: LsqParams -> Params -> Params
updateLsqParams lsqParms state = 
    state {lsqParams = lsqParms}

updateEnvParams :: EnvParams -> Params -> Params
updateEnvParams envParms state = 
    state {envParams = envParms}

type ProgressUpdateFunc = Double -> IO ()

type LogFunc = String -> IO ()

data TaskEnv = TaskEnv {
    progressUpdateFunc :: Double -> IO (),
    logFunc :: String -> IO (),
    taskInitializer :: ThreadId -> IO (),
    taskFinalizer :: IO ()
}

defaultTaskEnv :: TaskEnv
defaultTaskEnv = TaskEnv {
    progressUpdateFunc = \_ -> return (),
    logFunc = putStrLn,
    taskInitializer = \_ -> return (),
    taskFinalizer = return ()
} 

newtype (Eq id) => DataUpdateFunc id = DataUpdateFunc (SubData -> id -> Bool -> IO ())

