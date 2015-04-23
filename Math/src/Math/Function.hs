
module Math.Function (
    Fn,
    ComplexFn,
    Function,
    getValue,
    getValue_,
    getComplexValue,
    constantOp,
    binaryOp,
    xmlElementName,
    function,
    add,
    subtr,
    mult,
    divide,
    varNames,
    funcNames,
    initialExpression,
    toComplexFunction,
    validate,
    isValid,
    fromExpression,
    setVarValue
    ) where

import qualified Math.Expression as E
import Data.Map.Strict as M hiding (mapMaybe)
import Data.List as L
import Data.Complex
import qualified Utils.Xml as Xml
import Utils.Str
import Utils.List
import Utils.Misc
import Prelude hiding ((.))
import System.Random
import Debug.Trace
import Data.Maybe

-- | Defines a function. The most generic form of the function is the following: 
--   a1=<expression1>
--   a2=<expression2>
--   ..
--   an=<expressionn>
--   f1(x11, x12 ..)=<expression1'>
--   f2(x21, x22 ..)=<expression2'>
--   ..
--   fm(xm1, xm2 ..)=<expressionm'>
--   <expression>, 
--      where 
--      ai is is variable name that can be used in any of the expressions, 
--      fi is fnction name that can be used in any of the expressions,
--      xij are local variables names of the given function fi,
--      <expression>, <expressioni> and <expressioni'> are arbitrary expressions which may contain the names of above defined variables 
--          and functions as well as undefined ones. The definitions of the latter ones must be given 
--          as parameters to the getValue method. The function evalution begins from the <expresion>.
--
data Function a =
    Function {
        initialExpr :: String, -- ^ initial function definition
        expr :: E.Expression a, -- ^ parsed expression
        defs ::  [(String, ([String], E.Expression a))], -- ^ function and variable definitions
        varDefs :: [(String {- varName -}, (E.Expression a, [String] {- localVarNames -}, [String] {- localOpNames -}))], 
        funcDefs :: [(String {- opName -}, ([String] {- arguments -}, E.Expression a, [String] {- localVarNames -}, [String] {- localOpNames -}))],
        unknownVarNames :: [String],
        unknownFuncNames :: [String]
    } deriving (Show, Read, Eq)

class Fn f where
    getValue :: (RandomGen g) => [Double] -> [[Double] -> Double] -> g -> f -> Double
    getValue_ :: [Double] -> f -> Double
    constantOp :: Function Double -> f -> Double -> f
    binaryOp :: Function Double -> f -> f -> f

class ComplexFn f where    
    getComplexValue :: [Complex Double] -> f -> Complex Double

instance Fn (Function Double) where
    getValue xs fs g func =
        let
            varNamesAndValues = zip (unknownVarNames func) xs
            opNamesAndDefs = zip (unknownFuncNames func) fs
            
            --getVarValue :: E.Expression Double -> [String] -> [String] -> a
            getVarValue expr localVarNames localOpNames =  
                let 
                    localVarNamesAndValues = L.map (\varName ->
                        let
                            Just (expr1, localVarNames1, localOpNames1) = L.lookup varName (varDefs func)
                        in
                            (varName, getVarValue expr1 localVarNames1 localOpNames1)
                        ) localVarNames
                    localOpNamesAndDefs = L.map (\opName ->
                        let
                            Just (varNames, expr1, localVarNames1, localOpNames1) = L.lookup opName (funcDefs func)
                        in
                            (opName, getFuncDef varNames expr1 localVarNames1 localOpNames1)
                        ) localOpNames
                in
                    E.calcDouble expr (M.fromList (varNamesAndValues ++ localVarNamesAndValues)) (M.fromList (opNamesAndDefs ++ localOpNamesAndDefs)) g
        
            --getOpDef :: [String] -> E.Expression Double -> [String] -> [String] -> ([a] -> a)
            getFuncDef varNames expr localVarNames localOpNames =  
                let 
                    localVarNames = (E.varNames expr L.\\ varNames) L.\\ unknownVarNames func
                    localVarNamesAndValues = L.map (\varName ->
                        let
                            Just (expr1, localVarNames1, localOpNames1) = L.lookup varName (varDefs func)
                        in
                            (varName, getVarValue expr1 localVarNames1 localOpNames1)
                        ) localVarNames
                    localOpNames = E.opNames expr L.\\ unknownFuncNames func
                    localOpNamesAndDefs = L.map (\funcName ->
                        let
                            Just (varNames, expr1, localVarNames1, localOpNames1) = L.lookup funcName (funcDefs func)
                        in
                            (funcName, getFuncDef varNames expr1 localVarNames1 localOpNames1)
                        ) localOpNames
                in
                    (\xs ->
                        E.calcDouble expr (M.fromList (varNamesAndValues ++ localVarNamesAndValues ++ zip varNames xs)) (M.fromList (opNamesAndDefs ++ localOpNamesAndDefs)) g)

            defVarNamesAndValues = L.map (\(varName, (expr, localVarNames, localOpNames)) -> (varName, getVarValue expr localVarNames localOpNames)) (varDefs func)
            defOpNamesAndDefs = L.map (\(funcName, (varNames, expr, localVarNames, localOpNames)) -> (funcName, getFuncDef varNames expr localVarNames localOpNames)) (funcDefs func)

        in            
            E.calcDouble (expr func) (M.fromList (varNamesAndValues ++ defVarNamesAndValues)) (M.fromList (opNamesAndDefs ++ defOpNamesAndDefs)) g
        
    getValue_ xs = getValue xs [] (mkStdGen 1)
    binaryOp func func1 func2 = 
        let 
            [x, y] = E.varNames (expr func)
            exprStr = replaceAll y (show (expr func2)) (replaceAll x (show (expr func1)) (show (expr func)))
        in
            Function {
                initialExpr = exprStr,
                expr = read exprStr,
                defs = defs func1 ++ defs func2,
                varDefs = varDefs func1 ++ varDefs func2, 
                funcDefs = funcDefs func1 ++ funcDefs func2,
                unknownVarNames = nub $ unknownVarNames func1 ++ unknownVarNames func2,
                unknownFuncNames = nub $ unknownFuncNames func1 ++ unknownFuncNames func2
            }
    constantOp func func1 a = 
        let
            (x, replaceConstFunc) = case E.varNames (expr func) of
                [x] -> (x, id)
                [x, y] -> (x, replaceAll y (show a))
        in
            func {
                expr = read (replaceConstFunc (replaceAll x (show (expr func1)) (show (expr func)))),
                defs = []
            }

instance ComplexFn (Function (Complex Double)) where
    getComplexValue zs func = E.calc (expr func) (M.fromList (zip (E.varNames (expr func)) zs)) empty

instance (Floating a, Read a, Show a) => Xml.XmlElement (Function a) where
    toElement func = Xml.element xmlElementName [("initialExpression", initialExpr func)] [Right (initialExpr func)]

    fromElement e = 
        case Xml.contentTexts e of
            [initialExpression] -> function initialExpression
            otherwise ->
                let
                    ex = Xml.fromElement $ head $ Xml.contentElements e E.xmlElementName
                in
                    case Xml.maybeAttrValue e "initialExpression" of
                        Just s -> function s
                        Nothing ->
                            Function {
                                initialExpr = show ex,
                                expr = ex,
                                defs = [],
                                varDefs = [],
                                funcDefs = [],
                                unknownVarNames = [],
                                unknownFuncNames = []
                            }

xmlElementName :: String
xmlElementName = "function"

-- | Parses a function from String
function :: (Floating a, Read a) => String -> Function a
function s = 
    let
        splitFunc str =
            case L.elemIndex '=' str of
                Just i ->
                    let 
                        prevChar = str !! (i - 1)
                        nextChar = str !! (i + 1)
                    in
                        if prevChar `notElem` "=!<>" && nextChar `notElem` "=" then [take i str, drop (i + 1) str] else [str]
                Nothing -> [str]
        -- get all the lines of the input string and split them using '=' 
        strLines = L.map splitFunc $ lines $ replaceAll ";" "\n" s
        -- The line without '=' is the expression from where function calculation begins,
        -- all other lines are considered to be definitions of variables and functions
        Just [f] = find (\s -> length s == 1) strLines

        funcOrVarDefs = L.map (\(leftExpr, rightExpr) -> 
            case leftExpr of 
                E.Variable var -> (var, ([], rightExpr)) -- variable definition
                E.UnaryExpression (E.UnaryOp op, E.Variable var) -> (op, ([var], rightExpr)) -- definition of single-variable function 
                E.BinaryExpression (E.BinaryOp op, E.Variable var, expr) -> (op, (var:getFuncVars expr, rightExpr)) -- definition of multiple-variable function  
            ) funcOrVarDefs' where
            
                funcOrVarDefs' :: (Floating a, Read a) => [(E.Expression a, E.Expression a)]
                funcOrVarDefs' = L.map (\[funcOrVar, expr] -> (read funcOrVar, read expr)) $ L.filter (\s -> length s == 2) strLines
            
                -- returns the list of variable names found in given expression
                getFuncVars (E.Variable var) = [var] 
                getFuncVars (E.BinaryExpression (E.Comma, E.Variable var1, E.Variable var2)) = [var1, var2] 
                getFuncVars (E.BinaryExpression (E.Comma, E.Variable var, expr)) = var:getFuncVars expr
        func =
            Function {
                initialExpr = s,
                expr = read f,
                defs = funcOrVarDefs,
                varDefs = L.map (\(varName, ([], e)) -> (varName, (e, E.varNames e L.\\ unknownVarNames func, E.opNames e L.\\ unknownFuncNames func))) $
                    L.filter (\(_, (args, _)) -> args == []) funcOrVarDefs,
                funcDefs = L.map (\(varName, (args, e)) -> (varName, (args, e, (E.varNames e L.\\ args) L.\\ unknownVarNames func, E.opNames e L.\\ unknownFuncNames func))) $
                    L.filter (\(_, (args, _)) -> length args > 0) funcOrVarDefs,
                -- Initially mark all variable and function names as undefined
                unknownVarNames = varNames func,
                unknownFuncNames = funcNames func
            }
    in
        func {
            -- Recalculate unknown variable and function names
            unknownVarNames = varNames func,
            unknownFuncNames = funcNames func
        }

toComplexFunction :: Function Double -> Function (Complex Double)
toComplexFunction func = 
    Function {
        initialExpr = initialExpr func,
        expr = E.toComplexExpression (expr func),
        defs = [],
        varDefs = [],
        funcDefs = [],
        unknownVarNames = [],
        unknownFuncNames = []
    } 

-- | Returns the names of undefined variables found in given funtion 
varNames :: (Floating a) => Function a -> [String]
varNames func = 
    let
        allVarNames = fst $ unzip $ L.filter (\(_, (args, _)) -> args == []) $ defs func
        unknownVarNames1 = (concatMap mapOp (defs func)) where
            mapOp (_, (varNames, expr)) = E.varNames expr L.\\ varNames
        unknownVarNames2 = E.varNames (expr func) 
    in
        nub (unknownVarNames1 ++ unknownVarNames2) L.\\ allVarNames

-- | Returns the names of undefined functions found in given function
funcNames :: (Floating a) => Function a -> [String]
funcNames func =
    let 
        allFuncNames = fst $ unzip $ L.filter (\(_, (args, _)) -> length args > 0) $ defs func
        unknownFuncNames1 = concatMap mapOp (defs func) where
            mapOp (_, (_, expr)) = E.opNames expr
        unknownFuncNames2 = E.opNames (expr func)
    in
        nub (unknownFuncNames1 ++ unknownFuncNames2) L.\\ allFuncNames

initialExpression :: (Floating a) => Function a -> String
initialExpression  = initialExpr

add :: (Floating a, Read a) => Function a
add = function "x+y"

subtr :: (Floating a, Read a) => Function a
subtr = function "x-y"

mult :: (Floating a, Read a) => Function a
mult = function "x*y"

divide :: (Floating a, Read a) => Function a
divide = function "x/y"

validate :: (Floating a, Show a) => Function a -> [(Int, (String, Int))]
validate func = 
    let
        op (i, (_, ([], E.InvalidExpression str j))) = Just (i, (str, j))
        op (i, (_, (_, E.InvalidExpression str j))) = Just (i, (str, j))
        op _ = Nothing
        
        retVal = mapMaybe op (zip [1 .. length (defs func)] (defs func)) 
    in
        case expr func of 
            E.InvalidExpression str i -> (0, (str, i)):retVal
            e -> trace ("e: " ++ show e) retVal

isValid :: (Floating a, Show a) => Function a -> Bool
isValid f = validate f == []

fromExpression :: (Floating a, Read a, Show a)=> E.Expression a -> Function a
fromExpression expr = function (show expr)

setVarValue :: (Show a) => (String, a) -> Function a -> Function a
setVarValue (varName, varValue) f = 
    let 
        newExpr = E.setVarValue (varName, varValue) (expr f)
    in
        f {
            initialExpr = show newExpr,
            expr = newExpr, 
            unknownVarNames = L.filter (/= varName) (unknownVarNames f)
        }
 