
module Utils.Xml (
    XmlElement(..), Element, Document, 
    toDocument, fromDocument, 
    render, parse, renderToFile, parseFromFile, 
    element,
    name, attrs, contents, attrValues, contentElements, contentTexts,
    maybeAttrValue, attrValue, contentElement, maybeContentElement
    ) where 

import qualified Text.XML.HaXml.Types as Types
import qualified Text.XML.HaXml.Pretty as Pretty
import qualified Text.XML.HaXml.Parse as Parse
import Text.XML.HaXml.Posn
import qualified Text.XML.HaXml.Util as Util
import qualified Text.PrettyPrint.HughesPJ as HughesPJ
import Debug.Trace
import qualified Data.ByteString.Lazy as B
import Codec.Binary.UTF8.String

type Element = Types.Element Posn 
type Document = Types.Document Posn 

class XmlElement a where

    toElement :: a -> Element
    fromElement :: Element-> a


toDocument :: (XmlElement a) => a -> Document
toDocument a = Types.Document (Types.Prolog (Just (Types.XMLDecl "1.0" (Just (Types.EncodingDecl "UTF-8")) Nothing)) [] Nothing []) [] (toElement a) []

fromDocument :: (XmlElement a) => Document -> a
fromDocument (Types.Document _ _ e _ ) = fromElement e

render :: Document -> String
render doc = HughesPJ.render (Pretty.document doc)

parse :: String -> String -> Document
parse = Parse.xmlParse

renderToFile :: Document -> String -> IO ()
renderToFile doc fileName = do
    let
        byteStr = B.pack (encode (render doc))
    B.writeFile fileName byteStr


parseFromFile :: String -> String -> IO (Document)
parseFromFile elementName fileName = do
    byteStr <- B.readFile fileName
    let
        doc = parse elementName (decode (B.unpack byteStr))
    return doc 


element :: String -> [(String, String)] -> [Either Element String] -> Element
element name attrs content =
    Types.Elem 
        (Types.N name)
        (map (\(name, value) -> (Types.N name, Types.AttValue [Left value])) attrs) 
        (map mapOp content) where 
            mapOp (Left e) = Types.CElem e noPos
            mapOp (Right s) = Types.CString True s noPos 

name :: Element -> String
name (Types.Elem (Types.N n) _ _) = n

attrs :: Element -> [(String, String)]
attrs e = 
    map mapOp (filter filterOp (Util.attrs e)) where
        filterOp (key, Types.AttValue [Left value]) = True
        filterOp _ = False
        
        mapOp (Types.N key, Types.AttValue [Left value]) = (key, value)
    
contents :: Element -> [Either Element String]
contents (Types.Elem _ _ cs) = 
    map mapOp (filter filterOp cs) where
        filterOp (Types.CElem _ _) = True
        filterOp (Types.CString _ _ _) = True
        filterOp _ = False
        
        mapOp (Types.CElem e _) = Left e
        mapOp (Types.CString _ s _) = Right s
    
contentElements :: Element -> String -> [Element]
contentElements e elementName = 
    map mapOp (filter filterOp (contents e)) where 
        filterOp (Left e) = (name e == elementName) 
        filterOp _ = False
        
        mapOp (Left e) = e

attrValues :: Element -> String -> [String]
attrValues e attrName =
    map mapOp (filter filterOp (attrs e)) where
        filterOp (key, value) = (key == attrName)
        
        mapOp (key, value) = value

contentTexts :: Element -> [String]
contentTexts e = 
    map mapOp (contents e) where
        filterOp (Right _) = True
        filterOp _ = False
        
        mapOp (Right s) = s

maybeAttrValue :: Element -> String -> Maybe String
maybeAttrValue e name = 
    let 
        values = attrValues e name
    in 
        if length values <= 0
            then Nothing
            else Just $ head values

attrValue :: Element -> String -> String
attrValue e name = 
    let 
        values = attrValues e name
    in 
        head values

contentElement :: Element -> String -> Element
contentElement e name = 
    let 
        elements = contentElements e name
    in
        head elements

maybeContentElement :: Element -> String -> Maybe Element
maybeContentElement e name = 
    let 
        elements = contentElements e name
    in
        if length elements <= 0
            then Nothing
            else Just $ head elements
