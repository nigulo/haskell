
module Regression.Functions (
    Functions,
    xmlElementName
    ) where
    
import Regression.AnalyticData
import qualified Math.Function as F
import Debug.Trace
import Utils.Misc
import Utils.List

import Data.List
import qualified Utils.Xml as Xml

type Functions = AnalyticData (F.Function Double)

instance Xml.XmlElement Functions where
    toElement (AnalyticData s) = Xml.element xmlElementName [] (map mapOp s) where
        mapOp (xMins, xMaxs, p) = Left $ Xml.element "node" 
            ([("left", show (head xMins)), ("right", show (head xMaxs))] ++
                (case tail xMins of 
                    [yMin] -> [("bottom", show yMin)]
                    otherwise -> []) ++
                (case tail xMaxs of 
                    [yMax] -> [("top", show yMax)]
                    otherwise -> [])
                )
            [Left (Xml.toElement p)]

    fromElement e = 
        AnalyticData (map mapOp (Xml.contents e)) where 
            mapOp (Left e) =
                let 
                    xMin = read $ Xml.attrValue e "left" 
                    xMax = read $ Xml.attrValue e "right"
                    maybeyMin = Xml.maybeAttrValue e "bottom" 
                    maybeyMax = Xml.maybeAttrValue e "top"
                    yMin = case maybeyMin of 
                        Just y -> [read y]
                        Nothing -> [] 
                    yMax = case maybeyMax of 
                        Just y -> [read y]
                        Nothing -> [] 
                    
                    p = Xml.fromElement $ head $ Xml.contentElements e F.xmlElementName
                in (xMin:yMin, xMax:yMax, p)

xmlElementName :: String
xmlElementName = "functions"

