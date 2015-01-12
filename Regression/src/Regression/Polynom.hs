-- Module defining polynom data type and 
-- useful functions
module Regression.Polynom (
    PolynomTemplate (..), 
    Polynom (..), 
    polynom,
    unitPolynom,
    unitPolynoms,
    modulatedPolynom,
    modulatedUnitPolynom,
    modulatedUnitPolynoms,
    setCoef, 
    getCoef, 
    setCoefs, 
    getRank,
    getRanks,
    getTangent, 
    getDerivative,
    add,
    subtr,
    mult,
    divide,
    getValueAtIndex,
    getTangentAtIndex,
    getDerivativeAtIndex,
    getValues,
    getTangents,
    getDerivatives,
    getCount, -- ^ Returns the total count of polynoms in the set
    getSubPolynoms,
    unmodulate,
    isModulated,
    getModulators,
    getModulatorDerivs,
    setModulators,
    setModulatorDerivs,
    xmlElementName,
    polynomProduct,
    polynomSum,
    polynomDiff
    ) where

import Debug.Trace
import Utils.List
import Utils.Math
--import Math.Expression hiding (xmlElementName)
import qualified Data.List as List
import qualified Utils.Xml as Xml
import qualified Math.Function as F
import System.Random

newtype PolynomTemplate = PolynomTemplate (
    Int, -- ^ Polynom rank
    Maybe (F.Function Double), -- ^ Modulating function y = f(x)
    Maybe (F.Function Double) -- ^ Derivative of modulating function y = f(i, x)
    ) deriving (Show, Read, Eq)

type PolynomDef =
    ([(Int, Double)], -- ^ Polynom's coeficients
    Maybe (F.Function Double), -- ^ Modulating function
    Maybe (F.Function Double)) -- ^ Modulators i'th derivative

-- | Type representing mathematical polynom
-- | The constructor takes an array of 
-- | coefficient index and value pairs
newtype Polynom = Polynom [PolynomDef] deriving (Show, Read, Eq)

value x ([], _, _) = 0
value x (coefs, f, d) =
    List.foldl' (\sum (coefIndex, coefValue) -> sum + x ^ coefIndex * coefValue) 0 coefs

instance F.Fn Polynom where

    -- | Returns the polynom's value at the given coordinate
    getValue (x:[]) _ _ (Polynom (pol@(_, Just f, _):[])) =
        (value x pol) * (F.getValue_ [x] f) 
    getValue (x:[]) _ _ (Polynom (pol@(_, Nothing, _):[])) = value x pol
    getValue (x:[]) _ _ (Polynom (p:ps)) = 
        (F.getValue_ [x] (Polynom [p])) + (F.getValue_ [x] (Polynom ps))

    getValue_ xs = F.getValue xs [] (mkStdGen 1) 

    constantOp op (Polynom [(coefs, f, d)]) k = 
        Polynom [(map (\(i, val) -> (i, F.getValue_ [val, k] op)) coefs, f, d)]
    constantOp op (Polynom (p:ps)) k = 
        let 
            Polynom p1 = F.constantOp op (Polynom [p]) k
            Polynom p2s = F.constantOp op (Polynom ps) k
        in 
            Polynom (p1 ++ p2s)

    --------------------------------------------------------------------------------
    -- | Binary operation between polynom coeficients. Both polynoms must
    --   have same modulating function and same number of sub-polynomes
    --   NB! this is not a real binary operation between two polynomes, the binary
    --   operation is only applied between the coefficients of both polynomes having
    --   the same indices. If you need to get a product of two polynomes use the
    --   the function `product` function under this module instead.
    binaryOp op pol1@(Polynom [(coefs1, f1, d1)]) pol2@(Polynom [(coefs2, f2, d2)]) = 
        let indices = (fst (unzip coefs1)) `union` (fst (unzip coefs2));
            f i = setCoef (i, F.getValue_ [getCoef i pol1, getCoef i pol2] op)
        in
            List.foldl' (\pol i -> f i pol) (Polynom [([], getModulator pol1, getModulatorDeriv pol1)]) indices
    binaryOp op pol1@(Polynom (p1:ps1)) pol2@(Polynom (p2:ps2)) = 
        let 
            Polynom p12 = F.binaryOp op (Polynom [p1]) (Polynom [p2])
            Polynom ps12 = F.binaryOp op (Polynom ps1) (Polynom ps2)
        in
            Polynom (p12 ++ ps12)



instance Xml.XmlElement Polynom where
    toElement p@(Polynom pol) = Xml.element xmlElementName [] (map mapOp pol) where
        mapOp (coefs, f, d) =
            Left $ Xml.element "polynom" [("version", "1")]
            [
                Left (Xml.element "coeficients" [] (map (\coef -> Right (show coef)) coefs)),
                Left (Xml.element "modulatorfunc" [] (
                    case f of 
                        Just expression -> [Left $ Xml.toElement expression]
                        otherwise -> []
                )), 
                Left (Xml.element "modulatorderiv" [] ( 
                    case d of 
                        Just expression -> [Left $ Xml.toElement expression]
                        otherwise -> []
                ))
            ]

    fromElement e = 
        Polynom (map mapOp (Xml.contents e)) where
            mapOp (Left e) =     
                let 
                    name = Xml.name e
                    attrs = Xml.attrs e
                    version = case Xml.maybeAttrValue e "version" of
                        Just v -> v
                        Nothing -> "1"
                    coefs = Xml.contents $ head $ Xml.contentElements e "coeficients"
                    f = Xml.contents $ head $ Xml.contentElements e "modulatorfunc"
                    d = Xml.contents $ head $ Xml.contentElements e "modulatorderiv"
                in
                    (
                        map (\(Right coef) -> read coef) coefs,
                        (case f of 
                            [] -> Nothing
                            otherwise ->
                                Just $ head $ map (\(Left e) -> Xml.fromElement e) f
                            ),
                        (case d of 
                            [] -> Nothing
                            otherwise -> 
                                Just $ head $ map (\(Left e) -> Xml.fromElement e) d
                            )
                    )

xmlElementName :: String
xmlElementName = "polynoms"

-- | Returns an empty primitive polynom (with no modulating function)
polynom :: [(Int, Double)] -> Polynom
--polynom coefs = Polynom [(coefs, \_ -> 1, \i _ -> if i <= 0 then 1 else 0)]
polynom coefs = Polynom [(coefs, Nothing, Nothing)]

unitPolynom :: Int -> Polynom
unitPolynom rank = polynom [(i, 1) | i <- [0 .. rank]]

unitPolynoms :: Int -- ^ rank
             -> Int -- ^ count
             -> Polynom
unitPolynoms rank 1 = unitPolynom rank
unitPolynoms rank count = 
    let Polynom (p:[]) = unitPolynom rank
    in Polynom $ replicate count p


modulatedPolynom :: 
    [(Int, Double)] ->
    Maybe (F.Function Double) -- ^ Modulating function
    -> Maybe (F.Function Double) -- ^ i'th derivative of the modulating function
    -> Polynom
modulatedPolynom coefs f d = Polynom [(coefs, f, d)]

modulatedUnitPolynom :: PolynomTemplate -> Polynom
modulatedUnitPolynom (PolynomTemplate (degree, f, d)) = 
    modulatedPolynom [(i, 1) | i <- [0 .. degree]] f d

modulatedUnitPolynoms :: [PolynomTemplate]
                      -> Polynom
modulatedUnitPolynoms (t0:[]) = modulatedUnitPolynom t0
modulatedUnitPolynoms (t0:ts) = 
    let Polynom (p:[]) = modulatedUnitPolynom t0
        Polynom ps = modulatedUnitPolynoms ts
    in Polynom (p:ps)


-- | Sets the polynom's coeficient at the given index
setCoef :: (Int, Double) -> Polynom -> Polynom
setCoef (_, 0) (Polynom [([], f, d)]) = Polynom [([], f, d)]
setCoef coef (Polynom [([], f, d)]) = Polynom [([coef], f, d)]
setCoef coef@(i, c) (Polynom [(coefs, f, d)]) = 
    let 
        otherCoefs = filter (\(i1, _) -> i1 /= i) coefs
        newCoefs = 
            if c == 0 then otherCoefs
            else otherCoefs ++ [(i, c)]
    in
        newCoefs `seq` Polynom [(newCoefs, f, d)]

-- | gets the polynom's coeficient at the given index
getCoef :: Int -> Polynom -> Double
getCoef _ (Polynom [([], _, _)]) = 0
--getCoef i (Polynom [((coefIndex, coefValue):ps, f, d)]) = 
--    if coefIndex == i then coefValue
--    else getCoef i (Polynom [(ps, f, d)])
getCoef i (Polynom [(coefs, _, _)]) = 
    let coef = List.find (\(i1, _) -> i1 == i) coefs    
    in
        case coef of 
            Just c -> snd c
            Nothing -> 0

-- | sets the coeficients of all subpolynoms. In case this is a
--   primitive polynom the index must be zero
setCoefs :: [[(Int, Double)]]
         -> Polynom 
         -> Polynom
{-
setCoefs ((coef:[]):[]) p = setCoef coef p
setCoefs ((coef:coefs):[]) p = setCoefs [coefs] (setCoef coef p)
setCoefs (coefs0:coefs) (Polynom (p:ps)) = 
    let 
        Polynom (p1:[]) = setCoefs [coefs0] (Polynom [p])
        Polynom p1s = setCoefs coefs (Polynom ps)
    in 
        Polynom (p1:p1s)
-}
setCoefs coefs (Polynom p) = 
    let
        --p1 = map (\(newCoefs, (oldCoefs, f, d)) -> (newCoefs, f, d)) (zip coefs p) 
        p1 = map (\(newCoefs, (oldCoefs, f, d)) -> (newCoefs ++ (filter (\(oldCoef, _) -> (oldCoef `notElem` (fst (unzip newCoefs)))) oldCoefs), f, d)) (zip coefs p) 
    in Polynom p1

getRank :: Polynom -> Int
getRank (Polynom [([], _, _)]) = 0
getRank (Polynom [(coefs, _, _)]) = maximum $ fst $ unzip coefs

getRanks :: Polynom -> [Int]
getRanks (Polynom (p:[])) = [getRank (Polynom [p])]
getRanks (Polynom (p:ps)) = getRank (Polynom [p]):getRanks (Polynom ps)

getCount :: Polynom -> Int
getCount (Polynom ps) = length ps
--getCount (Polynom (p:[])) = 1
--getCount (Polynom (p:ps)) = getCount (Polynom [p]) + getCount (Polynom ps)

setModulator :: Maybe (F.Function Double) -> Polynom -> Polynom
setModulator f (Polynom [(ps, fOld, d)]) = Polynom [(ps, f, d)]

setModulators :: [Maybe (F.Function Double)] -> Polynom -> Polynom
setModulators fs (Polynom pols) = Polynom $ zipWith (\f polDef -> let Polynom [pd] = setModulator f (Polynom [polDef]) in pd) fs pols

getModulator :: Polynom -> Maybe (F.Function Double)
getModulator (Polynom ((_, f, _):_)) = f

getModulators :: Polynom -> [Maybe (F.Function Double)]
getModulators (Polynom pols) = map (\polDef -> getModulator (Polynom [polDef])) pols

setModulatorDeriv :: Maybe (F.Function Double) -> Polynom -> Polynom
setModulatorDeriv d (Polynom [(ps, f, dOld)]) = Polynom [(ps, f, d)]

setModulatorDerivs :: [Maybe (F.Function Double)] -> Polynom -> Polynom
setModulatorDerivs fs (Polynom pols) = Polynom $ zipWith (\f polDef -> let Polynom [pd] = setModulatorDeriv f (Polynom [polDef]) in pd) fs pols

getModulatorDeriv :: Polynom -> Maybe (F.Function Double)
getModulatorDeriv (Polynom ((_, _, d):_)) = d

getModulatorDerivs :: Polynom -> [Maybe (F.Function Double)]
getModulatorDerivs (Polynom pols) = map (\polDef -> getModulatorDeriv (Polynom [polDef])) pols

-- | returns the list of sub-polynoms given polynom is built up from
getSubPolynoms :: Polynom -> [Polynom]
getSubPolynoms pol@(Polynom (p:[])) = [pol]
getSubPolynoms (Polynom (p:ps)) = (Polynom [p]):getSubPolynoms (Polynom ps)

-- | removes the modulating function from given polynom
unmodulate :: Polynom -> Polynom
unmodulate (Polynom ((coefs, _, _):[])) = polynom coefs
unmodulate (Polynom ((coefs, _, _):ps)) = 
    let Polynom unmod = polynom coefs
        Polynom unmods = unmodulate (Polynom ps)
    in Polynom (unmod ++ unmods)

isModulated :: Polynom -> Bool
isModulated p = 
    case getModulator p of
        Just _ -> True
        Nothing -> False
    
-- | Returns the polynom's tanget value 
-- | at the given coordinate
getTangent :: Double -> Polynom -> Double
getTangent = getDerivative 1

-- | Returns the derivative value of the polynom
-- | at the given coordinate
getDerivative :: Int -> Double -> Polynom -> Double
getDerivative _ _ (Polynom [([], _, _)]) = 0
getDerivative i x pol@(Polynom ((coefs, f, d):[])) = 
    let dVal = (sum [(fromIntegral (combination j i)) * 
                (getDerivative1 (i - j)) * (func j) |
                j <- [0 .. i]]) where
        getDerivative1 :: Int -> Double
        getDerivative1 i = 
            sum $ map (deriv i) coefs where
                deriv :: Int -> (Int, Double) -> Double
                deriv i (coefIndex, coefValue) =
                    if coefIndex < i then 0
                    else if i <= 0 then x ^ coefIndex * coefValue
                    else fromIntegral (product [coefIndex - i + 1 .. coefIndex]) * x ^ (coefIndex - 1) * coefValue
        func j = 
                if j == 0 then
                case f of
                        Just f -> F.getValue_ [x] f
                        Nothing -> 1            
                else
                case d of
                        Just d ->
                            case F.varNames d of
                                ["x", "i"] -> F.getValue_ [x, fromIntegral j] d
                                otherwise -> F.getValue_ [fromIntegral j, x] d
                        Nothing -> 0

    in {-trace ("dVal=" ++ show dVal)-} dVal
getDerivative i x (Polynom (p:ps)) = (getDerivative i x (Polynom [p])) + (getDerivative i x (Polynom ps))

-- | Returns the polynom coeficient's value at the given coordinate
getValueAtIndex :: Double -> Polynom -> Int -> Double
getValueAtIndex x p@(Polynom [pol@(coef:coefs, f, d)]) coefIndex = 
    x ^ coefIndex * (getCoef coefIndex p) * func where
        func = case f of
            Just f -> F.getValue_ [x] f
            Nothing -> 1

-- | Returns all polynom coeficient values at the given coordinate
getValues :: Double -> Polynom -> [[Double]]
getValues x p@(Polynom ((coefs, f, d):[]))= [[getValueAtIndex x p i | i <- fst (unzip coefs)]]
getValues x (Polynom (p:ps)) = 
    values:getValues x (Polynom ps) where
        [values] = getValues x (Polynom [p])

-- | Returns the polynom coeficient's tangent value at the given coordinate
getTangentAtIndex :: Double -> Polynom -> Int -> Double
getTangentAtIndex = getDerivativeAtIndex 1

-- | Returns all polynom coeficient tangent values at the given coordinate
getTangents :: Double -> Polynom -> [[Double]]
getTangents x p@(Polynom ((coefs, f, d):[]))= [[getTangentAtIndex x p i | i <- fst (unzip coefs)]]
getTangents x (Polynom (p:ps)) = 
    let [tangents] = getTangents x (Polynom [p])
    in
        tangents:getTangents x (Polynom ps)


-- | Returns the polynom coeficient's derivative value at the given coordinate
getDerivativeAtIndex :: 
                Int -- ^ i'th Derivative
             -> Double -- ^ x
             -> Polynom
             -> Int -- ^ Coef index
             -> Double
getDerivativeAtIndex i x pol@(Polynom [(coef:coefs, f, d)]) coefIndex = 

--    if (fst coef /= coefIndex) then getDerivativeAtIndex i x (Polynom [(coefs, f, d)]) coefIndex
--    else
        let
            coefValue = getCoef coefIndex pol
            dVal = x ^ coefIndex * coefValue *
                    (sum [(fromIntegral (combination j i)) * 
                    (deriv (i - j)) * (func j) |
                    j <- [0 .. i]]) where

                deriv :: Int -> Double
                deriv i =
                    if coefIndex < i then 0
                    else if i <= 0 then 1 --x ^ coefIndex -- * coefValue
                    else 
                    --(signum x) * (abs x) ** (fromIntegral (-i) + logBase (abs x) (fromIntegral (product [coefIndex - i + 1 .. coefIndex])))
                    fromIntegral (product [coefIndex - i + 1 .. coefIndex]) / x ^ i --x ^ (coefIndex - i) -- * coefValue

                func j = 
                    if j == 0 then
                        case f of
                            Just f -> F.getValue_ [x] f
                            Nothing -> 1            
                    else
                        case d of
                            Just d ->
                                case F.varNames d of
                                    ["x", "i"] -> F.getValue_ [x, fromIntegral j] d
                                    otherwise -> F.getValue_ [fromIntegral j, x] d
                            Nothing -> 0
            
    
    
        in {-trace (show i ++ 
                    "'th derivative of " ++ 
                    show coefIndex ++ 
                    "'th term at " ++ 
                    show x ++ 
                    " = " ++ 
                    show dVal) -}
                    dVal

-- | Returns all polynom coeficient derivatives at the given coordinate
getDerivatives :: Int -> Double -> Polynom -> [[Double]]
getDerivatives i x p@(Polynom ((coefs, f, d):[]))= [[getDerivativeAtIndex i x p coefIndex | coefIndex <- fst (unzip coefs)]]
getDerivatives i x (Polynom (p:ps)) = 
    let [derivatives] = getDerivatives i x (Polynom [p])
    in derivatives:getDerivatives i x (Polynom ps)


add :: Polynom -> Polynom -> Polynom
add = F.binaryOp (F.add)

subtr :: Polynom -> Polynom -> Polynom
subtr = F.binaryOp (F.subtr)

mult :: Polynom -> Double -> Polynom
mult p k = 
    F.constantOp (F.mult) p k

divide :: Polynom -> Double -> Polynom
divide p k =  
    F.constantOp (F.divide) p k

polynomProduct :: Polynom -> Polynom -> Polynom
polynomProduct pol1@(Polynom [(coefs1, f1, d1)]) pol2@(Polynom [(coefs2, f2, d2)]) = 
    let 
        indices = (fst (unzip coefs1)) `union` (fst (unzip coefs2))
        maxIndex = maximum indices
        newFunc =
            case (f1, f2) of
                (Just func1, Just func2) -> Just $ F.binaryOp F.mult func1 func2 
                (Just f1, Nothing) -> Just $ f1
                (Nothing, Just f2) -> Just $ f2
                otherwise -> Nothing
        newDeriv =
            case (f1, f2, d1, d2) of
                (Just func1, Just func2, 
                    Just deriv1, Just deriv2) -> Just $ F.binaryOp F.add (F.binaryOp F.mult deriv1 func2) (F.binaryOp F.mult func1 deriv2) 
                (_, _, Just d1, Nothing) -> Just d1 
                (_, _, Nothing, Just d2) -> Just d2
                otherwise -> Nothing
        
        f (i, j) pol =
            let
                coefChange = getCoef i pol1 * getCoef j pol2
            in
                setCoef (i + j, getCoef (i + j) pol + coefChange) pol
    in
        List.foldl' (\pol (i, j) -> f (i, j) pol) (Polynom [([], newFunc, newDeriv)]) [(i, j) | i <- [0 .. maxIndex], j <- [0 .. maxIndex]]

polynomProduct pol1@(Polynom (p1:[])) pol2@(Polynom (p2:ps2)) = 
    let 
        Polynom p11 = polynomProduct (Polynom [p1]) (Polynom [p2])
        Polynom p12 = polynomProduct (Polynom [p1]) (Polynom ps2)
    in
        Polynom (p11 ++ p12)
polynomProduct pol1@(Polynom (p1:ps1)) pol2@(Polynom (p2:[])) = 
    let 
        Polynom p11 = polynomProduct (Polynom [p1]) (Polynom [p2])
        Polynom p21 = polynomProduct (Polynom ps1) (Polynom [p2])
    in
        Polynom (p11 ++ p21)
polynomProduct pol1@(Polynom (p1:ps1)) pol2@(Polynom (p2:ps2)) = 
    let 
        Polynom p11 = polynomProduct (Polynom [p1]) (Polynom [p2])
        Polynom p12 = polynomProduct (Polynom [p1]) (Polynom ps2)
        Polynom p21 = polynomProduct (Polynom ps1) (Polynom [p2])
        Polynom p22 = polynomProduct (Polynom ps1) (Polynom ps2)
    in
        Polynom (p11 ++ p12 ++ p21 ++ p22)

polynomSum :: Polynom -> Polynom -> Polynom
polynomSum (Polynom []) pol2 = pol2 
polynomSum pol1 (Polynom []) = pol1 
polynomSum pol1@(Polynom ((p1@(_, f1, _)):ps1)) pol2@(Polynom ((p2@(_, f2, _)):ps2)) =
    if f1 == f2
        then 
           let 
                Polynom p12 = F.binaryOp F.add (Polynom [p1]) (Polynom [p2])
                Polynom ps12 = polynomSum (Polynom ps1) (Polynom ps2)
            in
                Polynom (p12 ++ ps12)
        else
           let 
                Polynom ps12 = polynomSum (Polynom ps1) (Polynom ps2)
            in
                Polynom ([p1] ++ [p2] ++ ps12)

polynomDiff :: Polynom -> Polynom -> Polynom
polynomDiff pol1 pol2 = polynomSum pol1 (mult pol2 (-1))
