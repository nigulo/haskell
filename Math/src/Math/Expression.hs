module Math.Expression (
    Expression(..),
    UnaryOp(..),
    BinaryOp(..),
    const0,
    const1,
    constPi,
    const2Pi,
    constHalfPi,
    calc,
    calc_,
    calc__,
    calc___,
    calcDouble,
    calcDouble_,
    calcDouble__,
    calcDouble___,
    variable,
    constant,
    unaryExpression,
    binaryExpression,
    sine,
    cosine,
    dsin,
    dcos,
    varNames,
    opNames,
    toComplexExpression,
    isValid,
    expression,
    setVarValue,
    xmlElementName) where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Complex 
import Data.Char 
import Debug.Trace

import Utils.Str
import Utils.Num
import Utils.Misc
import Utils.Math
import System.Random

import qualified Utils.Xml as Xml

newtype VoidOp = VoidOp String deriving (Eq)

instance Xml.XmlElement VoidOp where
    toElement op = Xml.element "voidop" [("name", show op)] []
    fromElement e = read $ head (Xml.attrValues e "name")    

data UnaryOp = UnaryPlus | UnaryMinus | Fact | Sqrt | Sin | Cos | Tan | Sinh | Cosh | Tanh | 
    Log | Exp | Abs | Sgn | Asin | Acos | Atan | Asinh | Acosh | Atanh | Round | Trunc | Floor | Ceil | UnaryOp String deriving (Eq)

xmlElementName :: String
xmlElementName = "expression"

instance Xml.XmlElement UnaryOp where
    toElement op = Xml.element "unaryop" [("name", show op)] []
    fromElement e = read $ head (Xml.attrValues e "name")    
    
data BinaryOp = Comma | Equals | NotEquals | Lower | Greather | LowerEquals | GreatherEquals |
    Plus | Minus | Mul | Div | Pow | LogBase | Min | Max | Rnd | Normal | Sum | Product | If |
    BinaryOp String deriving (Eq)

instance Xml.XmlElement BinaryOp where
    toElement op = Xml.element "binaryop" [("name", show op)] []
    fromElement e = read $ head (Xml.attrValues e "name")


-- Operator list in precedence order
opList = [
    ",", "==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/", "^", "!", 
    "logbase", "min", "max", "rnd", "normal", "sum", "product", "if", 
    "sqrt", "asinh", "acosh", "atanh", "asin", "acos", "atan", "sinh", "cosh", "tanh", 
    "sin", "cos", "tan", "log", "exp", "abs", "sgn", "round", "trunc", "floor", "ceil"]

e = 2.7182818284590452353602874713527

instance Show (VoidOp) where
    show (VoidOp op) = op

instance Read (VoidOp) where
    readsPrec _ = \s ->
        case s of
            op -> [(VoidOp op, "")]

instance Show (UnaryOp) where
    show UnaryPlus = "+"
    show UnaryMinus = "-"
    show Fact = "!"
    show Sqrt = "sqrt"
    show Sin = "sin"
    show Cos = "cos"
    show Tan = "tan"
    show Sinh = "sinh"
    show Cosh = "cosh"
    show Tanh = "tanh"
    show Log = "log"
    show Exp = "exp"
    show Abs = "abs"
    show Sgn = "sgn"
    show Asin = "asin"
    show Acos = "acos"
    show Atan = "atan"
    show Asinh = "asinh"
    show Acosh = "acosh"
    show Atanh = "atanh"
    show Round = "round"
    show Trunc = "trunc"
    show Floor = "floor"
    show Ceil = "ceil"
    show (UnaryOp op) = op
    
instance Read (UnaryOp) where
    readsPrec _ = \s ->
        case s of
            "+" -> [(UnaryPlus, "")] 
            "-" -> [(UnaryMinus, "")]
            "!" -> [(Fact, "")]
            "sqrt" -> [(Sqrt, "")]
            "sin" -> [(Sin, "")]
            "cos" -> [(Cos, "")]
            "tan" -> [(Tan, "")]
            "sinh" -> [(Sinh, "")]
            "cosh" -> [(Cosh, "")]
            "tanh" -> [(Tanh, "")]
            "log" -> [(Log, "")]
            "exp" -> [(Exp, "")]
            "abs" -> [(Abs, "")]
            "sgn" -> [(Sgn, "")]
            "asin" -> [(Asin, "")]
            "acos" -> [(Acos, "")]
            "atan" -> [(Atan, "")]
            "asinh" -> [(Asinh, "")]
            "acosh" -> [(Acosh, "")]
            "atanh" -> [(Atanh, "")]
            "round" -> [(Round, "")]
            "trunc" -> [(Trunc, "")]
            "floor" -> [(Floor, "")]
            "ceil" -> [(Ceil, "")]
            op -> [(UnaryOp op, "")]
    
instance Show (BinaryOp) where
    show Comma = ","
    show Equals = "=="
    show NotEquals = "!="
    show Lower = "<"
    show Greather = ">"
    show LowerEquals = "<="
    show GreatherEquals = ">="
    show Plus = "+"
    show Minus = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"
    show LogBase = "logbase"
    show Min = "min"
    show Max = "max"
    show Rnd = "rnd"
    show Normal = "normal"
    show Sum = "sum"
    show Product = "product"
    show If = "if"
    show (BinaryOp op) = op

instance Read (BinaryOp) where
    readsPrec _ = \s ->
        case s of
            "," -> [(Comma, "")]
            "==" -> [(Equals, "")]
            "!=" -> [(NotEquals, "")]
            "<" -> [(Lower, "")]
            ">" -> [(Greather, "")]
            "<=" -> [(LowerEquals, "")]
            ">=" -> [(GreatherEquals, "")]
            "+" -> [(Plus, "")]
            "-" -> [(Minus, "")]
            "*" -> [(Mul, "")]
            "/" -> [(Div, "")]
            "^" -> [(Pow, "")]
            "logbase" -> [(LogBase, "")]
            "min" -> [(Min, "")]
            "max" -> [(Max, "")]
            "rnd" -> [(Rnd, "")]
            "normal" -> [(Normal, "")]
            "sum" -> [(Sum, "")]
            "product" -> [(Product, "")]
            "if" -> [(If, "")]
            op -> [(BinaryOp op, "")]
            

data Expression a = 
    Variable String |
    Constant a |
    VoidExpression VoidOp |
    UnaryExpression (UnaryOp, Expression a) | 
    BinaryExpression (BinaryOp, Expression a, Expression a) |
    InvalidExpression String Int deriving (Eq)

instance (Show a, Read a) => Xml.XmlElement (Expression a) where
    toElement e@(Variable v) = Xml.element xmlElementName [("type", "variable"), ("name", v)] [] 
    toElement e@(Constant c) = Xml.element xmlElementName [("type", "constant"), ("value", show c)] [] 
    toElement e@(VoidExpression op) = Xml.element xmlElementName [("type", "void")] [Left (Xml.toElement op)] 
    toElement e@(UnaryExpression (op, ex)) = Xml.element xmlElementName [("type", "unary")] [
        Left (Xml.toElement op), Left (Xml.toElement ex)] 
    toElement e@(BinaryExpression (op, ex1, ex2)) = Xml.element xmlElementName [("type", "binary")] 
        [Left (Xml.toElement op), 
        Left (Xml.element "left" [] [Left (Xml.toElement ex1)]),
        Left (Xml.element "right" [] [Left (Xml.toElement ex2)])]

    fromElement e =
        let 
            expressionType = head $ Xml.attrValues e "type"
        in
            case expressionType of
                "variable" -> Variable $ head $ Xml.attrValues e "name"
                "constant" ->  Constant $ read $ head $ Xml.attrValues e "value"
                "void" ->  VoidExpression (Xml.fromElement (head (Xml.contentElements e "voidop")))
                "unary" -> UnaryExpression (Xml.fromElement (head (Xml.contentElements e "unaryop")), Xml.fromElement (head (Xml.contentElements e xmlElementName)))
                "binary" -> BinaryExpression (
                    Xml.fromElement (head (Xml.contentElements e "binaryop")), 
                    Xml.fromElement (head (Xml.contentElements (head (Xml.contentElements e "left")) xmlElementName)), 
                    Xml.fromElement (head (Xml.contentElements (head (Xml.contentElements e "right")) xmlElementName))) 

     
const0 :: Expression Double
const0 = Constant 0

const1 = Constant (fromIntegral 1)
constPi = Constant pi
const2Pi = Constant (2 * pi)
constHalfPi = Constant (pi / 2)
constE = Constant e
constI = Constant (0 :+ 1)

variable :: String -> Expression a
variable x = Variable x

constant :: a -> Expression a
constant a = Constant a

unaryExpression :: UnaryOp -> Expression a -> Expression a
unaryExpression op expr = UnaryExpression (op, expr)

binaryExpression :: BinaryOp -> Expression a -> Expression a -> Expression a
binaryExpression op expr1 expr2 = BinaryExpression (op, expr1, expr2)

-- sine
sine freq = unaryExpression Sin (binaryExpression Mul (constant freq) (variable "x"))

-- cosine
cosine freq = unaryExpression Cos (binaryExpression Mul (constant freq) (variable "x"))

-- i'th derivative of sine
dsin freq = binaryExpression 
    Mul 
    (binaryExpression 
        Pow 
        (constant freq) 
        (variable "i")) 
    (unaryExpression 
        Sin 
         (binaryExpression 
            Plus 
            (binaryExpression 
                Mul 
                (constant freq) 
                (variable "x")) 
            (binaryExpression 
                Mul 
                (variable "i") 
                constHalfPi)))

-- i'th derivative of cosine
dcos freq = binaryExpression 
    Mul
    (binaryExpression 
        Pow 
        (constant freq) 
        (binaryExpression 
            Plus 
            (variable "i") 
            const1)) 
    (unaryExpression 
        Sin 
        (binaryExpression 
            Plus 
            (binaryExpression 
                Mul 
                (constant freq) 
                (variable "x"))
            (binaryExpression 
                Mul
                (binaryExpression 
                    Plus 
                    (variable "i")
                    const1) 
                constHalfPi)))

--------------------------------------------------------------------------------
-- | This is a helper function (not exposed) 
calc' :: (RandomGen g, Floating a) => 
    Expression a -> Map.Map String a -> Map.Map String ([a] -> a) -> g -> 
    (Expression a -> Map.Map String a -> Map.Map String ([a] -> a) -> g -> a) -> a
calc' (Variable "pi") _ _ _ _ = pi 
calc' (Variable var) m _ _ _ = val where Just val = Map.lookup var m
calc' (Constant c) _ _ _ _ = c
calc' (VoidExpression (VoidOp op)) m1 m2 g calc = f [] where Just f = Map.lookup op m2
calc' (UnaryExpression (UnaryPlus, expr)) m1 m2 g calc = calc expr m1 m2 g
calc' (UnaryExpression (UnaryMinus, expr)) m1 m2 g calc = (-calc expr m1 m2 g)
calc' (UnaryExpression (Sqrt, expr)) m1 m2 g calc = sqrt (calc expr m1 m2 g)
calc' (UnaryExpression (Sin, expr)) m1 m2 g calc = sin (calc expr m1 m2 g)
calc' (UnaryExpression (Cos, expr)) m1 m2 g calc = cos (calc expr m1 m2 g)
calc' (UnaryExpression (Tan, expr)) m1 m2 g calc = tan (calc expr m1 m2 g)
calc' (UnaryExpression (Sinh, expr)) m1 m2 g calc = sinh (calc expr m1 m2 g)
calc' (UnaryExpression (Cosh, expr)) m1 m2 g calc = cosh (calc expr m1 m2 g)
calc' (UnaryExpression (Tanh, expr)) m1 m2 g calc = tanh (calc expr m1 m2 g)
calc' (UnaryExpression (Log, expr)) m1 m2 g calc = log (calc expr m1 m2 g)
calc' (UnaryExpression (Exp, expr)) m1 m2 g calc = exp (calc expr m1 m2 g)
calc' (UnaryExpression (Abs, expr)) m1 m2 g calc = abs (calc expr m1 m2 g)
calc' (UnaryExpression (Sgn, expr)) m1 m2 g calc = signum (calc expr m1 m2 g)
calc' (UnaryExpression (Asin, expr)) m1 m2 g calc = asin (calc expr m1 m2 g)
calc' (UnaryExpression (Acos, expr)) m1 m2 g calc = acos (calc expr m1 m2 g)
calc' (UnaryExpression (Atan, expr)) m1 m2 g calc = atan (calc expr m1 m2 g)
calc' (UnaryExpression (Asinh, expr)) m1 m2 g calc = asinh (calc expr m1 m2 g)
calc' (UnaryExpression (Acosh, expr)) m1 m2 g calc = acosh (calc expr m1 m2 g)
calc' (UnaryExpression (Atanh, expr)) m1 m2 g calc = atanh (calc expr m1 m2 g)
calc' (UnaryExpression (UnaryOp op, expr)) m1 m2 g calc = f [calc expr m1 m2 g] where Just f = Map.lookup op m2
calc' (BinaryExpression (Plus, expr1, expr2)) m1 m2 g calc = (calc expr1 m1 m2 g) + (calc expr2 m1 m2 g)
calc' (BinaryExpression (Minus, expr1, expr2)) m1 m2 g calc = (calc expr1 m1 m2 g) - (calc expr2 m1 m2 g)
calc' (BinaryExpression (Mul, expr1, expr2)) m1 m2 g calc = (calc expr1 m1 m2 g) * (calc expr2 m1 m2 g)
calc' (BinaryExpression (Div, expr1, expr2)) m1 m2 g calc = (calc expr1 m1 m2 g) / (calc expr2 m1 m2 g)
calc' (BinaryExpression (Pow, expr1, expr2)) m1 m2 g calc = (calc expr1 m1 m2 g) ** (calc expr2 m1 m2 g)
-- returns the logarithm of the second argument in the base of the first one
calc' (BinaryExpression (LogBase, expr1, expr2)) m1 m2 g calc = logBase (calc expr1 m1 m2 g) (calc expr2 m1 m2 g)
calc' (BinaryExpression (Normal, expr, BinaryExpression(Comma, meanExpr, sigmaExpr))) m1 m2 g calc =
    let 
        x = calc expr m1 m2 g
        mean = calc meanExpr m1 m2 g
        sigma = calc sigmaExpr m1 m2 g
    in
        exp (-0.5 * ((x - mean) / sigma) ^ 2) / (sigma * sqrt(2 * pi))
calc' (BinaryExpression (BinaryOp op, expr1, expr2)) m1 m2 g calc = 
    let
        vars = calc expr1 m1 m2 g:calcAll expr2 m1 m2 g where
            calcAll (BinaryExpression(Comma, expr1, expr2)) m1 m2 g = calc expr1 m1 m2 g: calcAll expr2 m1 m2 g
            calcAll expr m1 m2 g = [calc expr m1 m2 g]
    in
        f vars where Just f = Map.lookup op m2

        
--------------------------------------------------------------------------------
-- | Calculates the value of the expression
calc :: (Floating a) => 
    Expression a 
    -> Map.Map String a -- ^ Variable name-value pairs
    -> Map.Map String ([a] -> a) -- ^ Function name-expression pairs
    -> a
calc e m1 m2 = calc' e m1 m2 (mkStdGen 1) (\e m1 m2 _ -> calc e m1 m2)

calc_ :: (Floating a) => Expression a -> [(String, a)] -> a
calc_ expr vals = calc expr (Map.fromList vals) Map.empty

calc__ :: (Floating a) => Expression a -> (String, a) -> a
calc__ expr val = calc_ expr [val]

calc___ :: (Floating a) => Expression a -> a
calc___ expr = calc_ expr []

-- | Special case for expressions of type Double
calcDouble :: (Floating a, RealFrac a, Random a, Enum a, RandomGen g) => 
    Expression a 
    -> Map.Map String a -- ^ Variable name-value pairs
    -> Map.Map String ([a] -> a) -- ^ Function name-expression pairs
    -> g 
    -> a
calcDouble (Variable "e") _ _ _ = exp 1
calcDouble (UnaryExpression (Fact, expr)) m1 m2 g = fromIntegral $ factorial $ round $ calcDouble expr m1 m2 g
calcDouble (UnaryExpression (Round, expr)) m1 m2 g = fromIntegral $ round $ calcDouble expr m1 m2 g
calcDouble (UnaryExpression (Trunc, expr)) m1 m2 g = fromIntegral $ truncate $ calcDouble expr m1 m2 g
calcDouble (UnaryExpression (Floor, expr)) m1 m2 g = fromIntegral $ floor $ calcDouble expr m1 m2 g
calcDouble (UnaryExpression (Ceil, expr)) m1 m2 g = fromIntegral $ ceiling $ calcDouble expr m1 m2 g
calcDouble (BinaryExpression (Equals, expr1, expr2)) m1 m2 g = if (calcDouble expr1 m1 m2 g) == (calcDouble expr2 m1 m2 g) then 1 else 0
calcDouble (BinaryExpression (NotEquals, expr1, expr2)) m1 m2 g = if (calcDouble expr1 m1 m2 g) /= (calcDouble expr2 m1 m2 g) then 1 else 0
calcDouble (BinaryExpression (Lower, expr1, expr2)) m1 m2 g = if (calcDouble expr1 m1 m2 g) < (calcDouble expr2 m1 m2 g) then 1 else 0
calcDouble (BinaryExpression (Greather, expr1, expr2)) m1 m2 g = if (calcDouble expr1 m1 m2 g) > (calcDouble expr2 m1 m2 g) then 1 else 0
calcDouble (BinaryExpression (LowerEquals, expr1, expr2)) m1 m2 g = if (calcDouble expr1 m1 m2 g) <= (calcDouble expr2 m1 m2 g) then 1 else 0
calcDouble (BinaryExpression (GreatherEquals, expr1, expr2)) m1 m2 g = if (calcDouble expr1 m1 m2 g) >= (calcDouble expr2 m1 m2 g) then 1 else 0
calcDouble (BinaryExpression (Min, expr1, expr2)) m1 m2 g = min (calcDouble expr1 m1 m2 g) (calcDouble expr2 m1 m2 g)
calcDouble (BinaryExpression (Max, expr1, expr2)) m1 m2 g = max (calcDouble expr1 m1 m2 g) (calcDouble expr2 m1 m2 g)
calcDouble (BinaryExpression (Rnd, expr1, expr2)) m1 m2 g = 
    lVal + r * (rVal - lVal) where
        (r, g') = randomR (0, 1) g
        (g1, g2) = System.Random.split g'
        lVal' = calcDouble expr1 m1 m2 g1
        rVal' = calcDouble expr2 m1 m2 g2
        lVal = min lVal' rVal'
        rVal = max lVal' rVal'
calcDouble (BinaryExpression (Sum, Variable indexName, BinaryExpression(Comma, iMinExpr, BinaryExpression(Comma, iMaxExpr, BinaryExpression(Comma, stepExpr, expr))))) m1 m2 g =
    let 
        iMin = calcDouble iMinExpr m1 m2 g
        iMax = calcDouble iMaxExpr m1 m2 g
        step = calcDouble stepExpr m1 m2 g
    in
        sum $ map (\i -> calcDouble expr (Map.insert indexName i m1) m2 g) [iMin, iMin + step .. iMax]
calcDouble (BinaryExpression (Product, Variable indexName, BinaryExpression(Comma, iMinExpr, BinaryExpression(Comma, iMaxExpr, BinaryExpression(Comma, stepExpr, expr))))) m1 m2 g =
    let 
        iMin = calcDouble iMinExpr m1 m2 g
        iMax = calcDouble iMaxExpr m1 m2 g
        step = calcDouble stepExpr m1 m2 g
    in
        product $ map (\i -> calcDouble expr (Map.insert indexName i m1) m2 g) [iMin, iMin + step .. iMax]
calcDouble (BinaryExpression (If, condition, BinaryExpression(Comma, trueExpr, falseExpr))) m1 m2 g =
    if (calcDouble condition m1 m2 g) /= 0 then calcDouble trueExpr m1 m2 g else calcDouble falseExpr m1 m2 g  
calcDouble e m1 m2 g = 
    calc' e m1 m2 g (calcDouble)

calcDouble_ :: (Floating a, RealFrac a, Random a, Enum a) => Expression a -> [(String, a)] -> a
calcDouble_ expr vals = calcDouble expr (Map.fromList vals) Map.empty (mkStdGen 1) 

calcDouble__ :: (Floating a, RealFrac a, Random a, Enum a) => Expression a -> (String, a) -> a
calcDouble__ expr val = calcDouble_ expr [val]

calcDouble___ :: (Floating a, RealFrac a, Random a, Enum a) => Expression a -> a
calcDouble___ expr = calcDouble_ expr []

-- | Casts given double valued expression to complex expression.
--   Here we assume that no functions needing ordering are present
toComplexExpression :: Expression Double -> Expression (Complex Double)
toComplexExpression (Variable "i") = constI
toComplexExpression (Variable var) = Variable var
toComplexExpression (Constant c) = Constant (c :+ 0)
toComplexExpression (UnaryExpression (op, expr)) =
    (UnaryExpression (op, toComplexExpression expr))
toComplexExpression (BinaryExpression (Plus, Constant c, Variable "i")) =
     Constant (c :+ 1)
toComplexExpression (BinaryExpression (Plus, Variable "i", Constant c)) =
     Constant (c :+ 1)
toComplexExpression (BinaryExpression (Mul, Variable "i", Constant c)) =
     Constant (0 :+ c)
toComplexExpression (BinaryExpression (Mul, Constant c, Variable "i")) =
     Constant (0 :+ c)
toComplexExpression (BinaryExpression (Plus, Constant c1, BinaryExpression (Mul, Variable "i", Constant c2))) =
     Constant (c1 :+ c2)
toComplexExpression (BinaryExpression (Plus, BinaryExpression (Mul, Variable "i", Constant c2), Constant c1)) =
     Constant (c1 :+ c2)
toComplexExpression (BinaryExpression (op, expr1, expr2)) = 
    (BinaryExpression (op, toComplexExpression expr1, toComplexExpression expr2))

instance (Show a) => Show (Expression a) where
    show (Variable var) = var 
    show (Constant c) = Prelude.show c 
    show (VoidExpression (VoidOp op)) = op ++ "()"
    show (UnaryExpression (op, expr)) = (show op) ++ "(" ++ (show expr) ++ ")"
            
    show (BinaryExpression (Normal, expr1, BinaryExpression(Comma, expr2, expr3))) =
            "(normal(" ++ (show expr1) ++ "," ++ (show expr2) ++ "," ++ (show expr3) ++ "))"
    show (BinaryExpression (Sum, Variable indexName, BinaryExpression(Comma, iMinExpr, BinaryExpression(Comma, iMaxExpr, BinaryExpression(Comma, stepExpr, expr))))) =
            "(sum(" ++ indexName ++ "," ++ (show iMinExpr) ++ "," ++ (show iMaxExpr) ++ "," ++ (show stepExpr) ++ "," ++ (show expr) ++ "))"
    show (BinaryExpression (Product, Variable indexName, BinaryExpression(Comma, iMinExpr, BinaryExpression(Comma, iMaxExpr, BinaryExpression(Comma, stepExpr, expr))))) =
            "(product(" ++ indexName ++ "," ++ (show iMinExpr) ++ "," ++ (show iMaxExpr) ++ "," ++ (show stepExpr) ++ "," ++ (show expr) ++ "))"
    show (BinaryExpression (If, condition, BinaryExpression(Comma, trueExpr, falseExpr))) =
            "(if(" ++ (show condition) ++ "," ++ (show trueExpr) ++ "," ++ (show falseExpr) ++ "))"
    show (BinaryExpression (op@Min, expr1, expr2)) = "(" ++ (show op) ++ "(" ++ (show expr1) ++ "," ++ (show expr2) ++ "))"
    show (BinaryExpression (op@Max, expr1, expr2)) = "(" ++ (show op) ++ "(" ++ (show expr1) ++ "," ++ (show expr2) ++ "))"
    show (BinaryExpression (op@Rnd, expr1, expr2)) = "(" ++ (show op) ++ "(" ++ (show expr1) ++ "," ++ (show expr2) ++ "))"
    show (BinaryExpression (BinaryOp op , expr1, expr2)) = 
        let
            vars = show expr1:",":showAll expr2 where
                showAll (BinaryExpression(Comma, expr1, expr2)) = show expr1:",":showAll expr2
                showAll expr = [show expr]
        in
            "(" ++ op ++ "(" ++ concat vars ++ "))"
    show (BinaryExpression (op, expr1, expr2)) = "(" ++ (show expr1) ++ ")" ++ (show op) ++ "(" ++ (show expr2) ++ ")"
    show (InvalidExpression str i) = str ++ " at index " ++ show i

instance (Read a) => Read (Expression a) where
    readsPrec _ = \str ->
        let
            str2 = replaceAll " " "" str
        in
            case validateBrackets str2 of
                Just (str, i) -> [(InvalidExpression str i, "")]
                otherwise -> [(parse str2 opList Nothing Nothing 0, "")]
            
varNames :: Expression a -> [String]
varNames (Variable "pi") = []
varNames (Variable "e") = []
varNames (Variable v) = [v]
varNames (Constant _) = []
varNames (VoidExpression _) = [] 
varNames (UnaryExpression (_, e)) = nub (varNames e)
varNames (BinaryExpression (Sum, Variable indexName, e)) = delete indexName $ nub $ varNames e
varNames (BinaryExpression (_, e1, e2)) = nub (varNames e1 ++ varNames e2)

opNames :: Expression a -> [String]
opNames (Variable v) = []
opNames (Constant _) = []
opNames (VoidExpression (VoidOp op)) = [op] 
opNames (UnaryExpression (UnaryOp op, e)) = nub (op:opNames e) 
opNames (UnaryExpression (_, e)) = nub (opNames e)
opNames (BinaryExpression (BinaryOp op, e1, e2)) = nub (op:(opNames e1 ++ opNames e2))
opNames (BinaryExpression (_, e1, e2)) = nub (opNames e1 ++ opNames e2)

setVarValue :: (String, a) -> Expression a -> Expression a
setVarValue (varName, varValue) (Variable v) = if varName == v then Constant varValue else Variable v
setVarValue _ (Constant c) = Constant c
setVarValue _ (VoidExpression e) = VoidExpression e
setVarValue varNameAndValue (UnaryExpression (op, expr)) = (UnaryExpression (op, setVarValue varNameAndValue expr))
setVarValue varNameAndValue (BinaryExpression (op, expr1, expr2)) = (BinaryExpression (op, setVarValue varNameAndValue expr1, setVarValue varNameAndValue expr2))

parse :: (Read a) => String -> [String] -> Maybe (Expression a) -> Maybe (Expression a) -> Int -> Expression a
parse str1 ops leftExpr rightExpr offset = 
    let
        (str, bCount) = removeBrackets str1
        maybeOp = case ops of
            [] -> Nothing
            otherwise -> Just $ head ops
    in
        case findOperator maybeOp str of
            Just (op, i) ->
                let 
                    (left, bCount1) = removeBrackets $ take i str
                    (right, bCount2) = removeBrackets $ drop (i + (length op)) str
                    leftE = if length left == 0 then leftExpr else Just (parse left opList leftExpr Nothing (offset + bCount + bCount1)) 
                    rightE = if length right == 0 then rightExpr else Just (parse right opList Nothing rightExpr (offset + bCount + (i + (length op)) + bCount))
                in
                    case leftE of
                        Just e@(InvalidExpression _ _) -> e
                        otherwise ->
                            case rightE of 
                                Just e@(InvalidExpression _ _) -> e
                                otherwise -> createExpression op leftE rightE (offset + bCount)
            Nothing ->
                case ops of
                    [] -> 
                        if (isNumeric str)  || (isComplex str) then 
                            Constant (read str)
                        else 
                            if str == "" || findIndices (\ch -> ch == '(' || ch ==')')  str /= [] 
                            then InvalidExpression "Parse error" (offset + bCount) 
                            else Variable str 
                    otherwise ->                  
                        parse str (tail ops) leftExpr rightExpr (offset + bCount)

createExpression :: (Read a) => String -> Maybe (Expression a) -> Maybe (Expression a) -> Int -> Expression a
createExpression opStr leftE rightE offset =
    let 
        checkOp opStr = if opStr `elem` opList then False else True
        checkExpression (BinaryExpression (op, _, _)) =
            case op of 
                BinaryOp opStr -> checkOp opStr
                otherwise -> True
        checkExpression (UnaryExpression (op, _)) = 
            case op of 
                UnaryOp opStr -> checkOp opStr
                otherwise -> True
        checkExpression (VoidExpression (VoidOp opStr)) = checkOp opStr 
        
        e =
            case leftE of
                Nothing ->
                    case rightE of
                        Just (BinaryExpression (Comma, le, re)) -> BinaryExpression (read opStr, le, re)
                        Just rightE -> UnaryExpression (read opStr, rightE)
                        Nothing -> VoidExpression $ read opStr
                Just expr ->
                    case rightE of
                        Nothing -> UnaryExpression (read opStr, expr)
                        Just rightE -> BinaryExpression (read opStr, expr, rightE)
                            
    in
        if checkExpression e 
        then e 
        else InvalidExpression "Parse error" offset
            


validateBrackets :: String -> Maybe (String, Int)
validateBrackets str =
    let
        validateBrackets1 [] 0 = Nothing
        validateBrackets1 (bracketIndex:bracketIndices) counter =
            if (str !! bracketIndex) == '(' then
                if bracketIndices == [] then
                    Just ("Missing closing bracket for opening bracket", bracketIndex)
                else 
                    validateBrackets1 bracketIndices (counter + 1)
            else
                if counter <= 0 then
                    Just ("Unexpected closing bracket", bracketIndex)
                else
                    if counter > 1 && bracketIndices == [] then
                        Just ("Missing closing bracket", (length str - 1)) 
                    else
                        validateBrackets1 bracketIndices (counter - 1)
        bracketIndices = findIndices (\ch -> ch == '(' || ch == ')') str
    in  
        validateBrackets1 bracketIndices 0

-- | removes leading and trailing brackets from given string
removeBrackets :: String -> (String, Int {- number of brackets removed-})
removeBrackets str = removeBrackets1 str 0 where
    removeBrackets1 :: String -> Int -> (String, Int)
    removeBrackets1 str count =
        let
            findOuterBrackets [] 0 = True
            findOuterBrackets [] _ = False
            findOuterBrackets (bracketIndex1:bracketIndex2:bracketIndices) 0 = False
            findOuterBrackets (bracketIndex:bracketIndices) counter =
                if (str !! bracketIndex) == '(' then
                    findOuterBrackets bracketIndices (counter + 1)
                else
                    findOuterBrackets bracketIndices (counter - 1)
            bracketIndices = findIndices (\ch -> ch == '(' || ch == ')') str
        in    
            if length str > 0 && head str == '(' && last str == ')' && findOuterBrackets (tail bracketIndices) 1  
            then removeBrackets1 (drop 1 (take (length str - 1) str)) (count + 1) 
            else (str, count)

-- | returns the index of operator in given sub-expression if present
findOperator :: Maybe String -> String -> Maybe (String, Int)
findOperator Nothing str =
    let
        bracketIndices = findIndices (\ch -> ch == '(') str
    in
        if length bracketIndices == 0 || head bracketIndices == 0 then Nothing
        else Just (take (head bracketIndices) str, 0)
findOperator (Just op) str =
    let
        indices = 
            let
                is = stringIndices op str
            in
                -- filter out complex numbers and exponents
                case op of
                    "+" -> filter (\i -> filterComplex str i && filterExponent str i) is
                    "-" -> filter (filterExponent str) is
                    otherwise -> 
                        if isLetter (head op)
                        then filter (\i -> (i <= 0 || not (isLetter (str !! (i - 1)))) && (length str <= (i + length op) || not (isLetter (str !! (i + length op))))) is
                        else is
                    
        findOuterBrackets1 (bracketIndex:[]) counter =
            if counter == 0 then
                case find (\i -> i > bracketIndex) indices of
                    Just i -> Just i
                    Nothing -> Nothing
            else Nothing
        findOuterBrackets1 (bracketIndex1:bracketIndex2:bracketIndices) counter =
            if counter == 0 && (str !! bracketIndex2) == '(' then
                case find (\i -> bracketIndex1 < i && bracketIndex2 > i) indices of
                    Just i -> Just i
                    Nothing -> findOuterBrackets1 (bracketIndex2:bracketIndices) 1
            else if (str !! bracketIndex2) == '(' then
                findOuterBrackets1 (bracketIndex2:bracketIndices) (counter + 1)
            else
                findOuterBrackets1 (bracketIndex2:bracketIndices) (counter - 1)
        bracketIndices = findIndices (\ch -> ch == '(' || ch == ')') str
    in
        if length indices == 0 then Nothing
        else if length bracketIndices == 0 then Just (op, head indices)
        else if (head indices) < (head bracketIndices) then Just (op, head indices)
        else 
            case findOuterBrackets1 bracketIndices 1 of
                Just i -> Just (op, i)
                Nothing -> Nothing

filterExponent str = \i -> i <= 1  || str !! (i - 1) `notElem` "eE" || isLetter (str !! (i - 2))
filterComplex str = \i -> i == 0 || str !! (i - 1) /= ':'
            
isValid :: (Eq a) => Expression a -> Bool
isValid (InvalidExpression _ _ ) = False
isValid _ = True
          
expression :: (Eq a, Read a, Show a) => String -> Expression a
expression = read


