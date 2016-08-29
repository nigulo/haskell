module TSA.CommonParams (
    CommonParams(..),
    commonParamsXmlElementName,
    SubDataParams (..),
    SubData (..),
    DataParams (..),
    unboxSubData,
    updateCommonParams
    
    ) where

import Regression.Data as D
import Regression.Spline as S
import Regression.Functions as F
import Regression.Utils as U
import Regression.AnalyticDataWrapper as ADW
import Regression.RBF as RBF

import qualified Utils.Xml as Xml

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

data SubData = SD1 D.Data | SD2 S.Spline | SD3 F.Functions | SD4 RBF.RBFs deriving (Show, Read)

unboxSubData :: SubData -> Either D.Data ADW.AnalyticDataWrapper
unboxSubData (SD1 d) = Left d
unboxSubData (SD2 s) = Right (ADW.analyticDataWrapper s)
unboxSubData (SD3 f) = Right (ADW.analyticDataWrapper f)
unboxSubData (SD4 rbf) = Right (ADW.analyticDataWrapper rbf)


data SubDataParams = SubDataParams {
    subDataRange :: ([Double], [Double]),
    subData :: SubData
} deriving (Show, Read)

instance Xml.XmlElement SubDataParams where
    toElement params = Xml.element "subdataparams" 
        [("range", show (subDataRange params))]
        ([Left (Xml.element "data" [] [
            case subData params of
                SD1 d -> Left (Xml.toElement d) 
                SD2 s -> Left (Xml.toElement s)
                SD3 f -> Left (Xml.toElement f)
                SD4 rbf -> Left (Xml.toElement rbf)
                ])]
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
                    if Xml.name dataSetElem == D.xmlElementName then SD1 $ Xml.fromElement dataSetElem
                    else if Xml.name dataSetElem == S.xmlElementName then SD2 $ Xml.fromElement dataSetElem
                    else if Xml.name dataSetElem == F.xmlElementName then SD3 $ Xml.fromElement dataSetElem
                    else SD4 $ Xml.fromElement dataSetElem
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
                            [SubDataParams {subDataRange = U.dataRange (unboxSubData sd), subData = sd}] where
                                sd =
                                    let 
                                        Left dataSetElem = head $ Xml.contents $ Xml.contentElement e "dataset" 
                                    in
                                        if Xml.name dataSetElem == D.xmlElementName then SD1 $ Xml.fromElement dataSetElem -- Data
                                        else if Xml.name dataSetElem == S.xmlElementName then SD2 $ Xml.fromElement dataSetElem -- Spline
                                        else SD3 $ Xml.fromElement dataSetElem -- Functions
                
        }

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
