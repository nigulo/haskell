
module ISDA.InOut (--readData, writeSpec, readParams, 
                --getState, 
                --updateState, 
                --run,
                readState, 
                readStateFromFile, 
                writeState, 
                writeStateToFile, 
                writeLn, 
                getString, 
                getParam,
                DataHeader (..), 
                DataBlocks (..), 
                SpecHeader (..), 
                SpecBlocks (..),
                Data, 
                Spec, 
                Params (..),
                ISDAState
                ) where

import System.IO
import Data.Int
import qualified Data.Map
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Word (Word16)

import Debug.Trace

import Utils.Str
import Utils.Num
import Utils.IO

--------------------------------------------------------------------------------
-- TYPE DEFINITIONS
--------------------------------------------------------------------------------

-- Data header definition
data DataHeader = DataHeader {
                    bands :: Word16,
                    curSegs :: Word16,
                    nData :: Int32, 
                    tOff :: Double,
                    fMin :: Double,
                    fMax :: Double,
                    isIndex :: Bool
                    } deriving Show

-- Definition of data blocks
data DataBlocks = DataBlocks {
                        ts :: [Double], 
                        dataSegs :: [Double],
                        fs :: [Float],
                        ws :: [Float],
                        is :: [Word16]} deriving Show

type Data = (DataHeader, DataBlocks)

-- Spectrum header definition
data SpecHeader = SpecHeader {
                    nLim :: Int32,
                    swMin :: Double,
                    swMax :: Double
                    } deriving Show

-- Definition of spectrum blocks
data SpecBlocks = SpecBlocks {pty :: [Float]} deriving Show
type Spec = (SpecHeader, SpecBlocks)

-- Parameters from ISDA main program
data Params = Params (Data.Map.Map String String)

type ISDAState = (Params, Either Data Spec)

{-
data ISDAMonad a = ISDAMonad (ISDAState -> (ISDAState, a))

instance Monad ISDAMonad where

    ISDAMonad lt0 >>= flt1 = ISDAMonad $ \s0 -> 
                            let (s1, x) = lt0 s0; ISDAMonad lt1 = flt1 x
                            in lt1 s1
                        
    return a = ISDAMonad(\s -> (s, a))

getState :: ISDAMonad ISDAState
getState = ISDAMonad (\s -> (s, s))

updateState :: (ISDAState -> ISDAState) -> ISDAMonad ()
updateState f = ISDAMonad (\s -> (f s, ()))

run :: ISDAState -> ISDAMonad a -> ISDAState
run s0 (ISDAMonad c) = fst (c s0)
-}
--------------------------------------------------------------------------------
-- PRIVATE FUNCTIONS
--------------------------------------------------------------------------------

readDataHeader :: Handle -> IO (DataHeader)
readDataHeader fileHandle = 
    do
        headValue <- readArray fileHandle True 4 1 :: IO [CChar]
        bandsValue <- readValue fileHandle 2
        nTotalValue <- readValue fileHandle 4
        nDataValue <- readValue fileHandle 4
        tOffValue <- readArray fileHandle True 10 1
        fMinValue <- readArray fileHandle True 10 1
        fMaxValue <- readArray fileHandle True 10 1
    
        return (DataHeader {bands = bandsValue `mod` 10,
                             curSegs = fromIntegral (nTotalValue - nDataValue),
                             nData = nDataValue,
                             tOff = extendedToDouble tOffValue,
                             fMin = extendedToDouble fMinValue,
                             fMax = extendedToDouble fMaxValue,
                             isIndex = (bandsValue >= 10)})

--------------------------------------------------------------------------------
readSpecHeader :: Handle -> IO (SpecHeader)
readSpecHeader fileHandle = 
    do
        headValue <- readArray fileHandle True 4 1 :: IO [CChar]
        n <- readValue fileHandle 4
        tMin <- readArray fileHandle True 10 1
        tMax <- readArray fileHandle True 10 1
    
        return (SpecHeader {nLim = n,
                            swMin = extendedToDouble tMin,
                            swMax = extendedToDouble tMax})

--------------------------------------------------------------------------------

readDataBlocks :: Handle -> DataHeader -> IO (DataBlocks)
readDataBlocks fileHandle dataHeader = 
    let numData = fromIntegral (nData dataHeader);
        numSegs = fromIntegral (curSegs dataHeader);
        numBands = bands dataHeader
    in
        do
            tValues <- readArray fileHandle (numBands > 0) numData 8
            dataSegValues <- readArray fileHandle ((curSegs dataHeader) > 0) numSegs 16
            fValues <- readArray fileHandle (numBands > 1) numData 4
            wValues <- readArray fileHandle (numBands > 2) numData 4
            iValues <- readArray fileHandle (isIndex dataHeader) numData 2
            
            return (DataBlocks {ts = tValues,
                                 dataSegs = dataSegValues,
                                 fs = fValues,
                                 ws = wValues,
                                 is = iValues})

--------------------------------------------------------------------------------

readSpecBlocks :: Handle -> SpecHeader -> IO (SpecBlocks)
readSpecBlocks fileHandle specHeader = 
    do
        vals <- readArray fileHandle (nLim specHeader > 0) (fromIntegral (nLim specHeader)) 4
        return (SpecBlocks {pty = vals})

--------------------------------------------------------------------------------

writeDataHeader :: Handle -> DataHeader -> IO ()
writeDataHeader fileHandle dataHeader = 
    let headerType = map (castCharToCChar) ".WRK" :: [CChar];
        bnds = if (isIndex dataHeader) then bands dataHeader + 10
               else bands dataHeader;
        numData = nData dataHeader
    in
        do
        
            writeArray fileHandle headerType
            writeValue fileHandle bnds
            writeValue fileHandle (numData + fromIntegral (curSegs dataHeader))
            writeValue fileHandle numData
            writeArray fileHandle (doubleToExtended (tOff dataHeader))
            writeArray fileHandle (doubleToExtended (fMin dataHeader))
            writeArray fileHandle (doubleToExtended (fMax dataHeader))

--------------------------------------------------------------------------------

writeDataBlocks :: Handle -> DataHeader -> DataBlocks -> IO ()
writeDataBlocks fileHandle dataHeader dataBlocks = 
    let numBands = bands dataHeader
    in
        do
            if numBands > 0 
                then writeArray fileHandle (ts dataBlocks) 
                else return ()
            if curSegs dataHeader > 0 
                then writeArray fileHandle (dataSegs dataBlocks)
                else return ()
            if numBands > 1
                then writeArray fileHandle (fs dataBlocks) 
                else return ()
            if numBands > 2
                then writeArray fileHandle (ws dataBlocks) 
                else return ()
            if isIndex dataHeader
                then writeArray fileHandle (is dataBlocks) 
                else return ()
            

--------------------------------------------------------------------------------

writeSpecHeader :: Handle -> SpecHeader -> IO ()
writeSpecHeader fileHandle specHeader = 
    let headerType = map (castCharToCChar) ".SPC" :: [CChar];
        swMinExtended = doubleToExtended (swMin specHeader);
        swMaxExtended = doubleToExtended (swMax specHeader);
        
    in
        do
        
            writeArray fileHandle headerType
            writeValue fileHandle (nLim specHeader)
            writeArray fileHandle swMinExtended
            writeArray fileHandle swMaxExtended

--------------------------------------------------------------------------------

writeSpecBlocks :: Handle -> SpecHeader -> SpecBlocks -> IO ()
writeSpecBlocks fileHandle specHeader specBlocks = 
    let n = fromIntegral (nLim specHeader)
    in
        do
            writeArray fileHandle (pty specBlocks)

--------------------------------------------------------------------------------
-- Reads data points from ISDA main program via binary file "INPUT.WRK"
--------------------------------------------------------------------------------
readData :: String -> IO Data
readData fileName = do
                handle <- openBinaryFile (fileName ++ ".WRK") ReadMode
                dataHeader <- readDataHeader handle
                dataBlocks <- readDataBlocks handle dataHeader
                hClose handle
                return (dataHeader, dataBlocks)

--------------------------------------------------------------------------------
-- Reads data points from ISDA main program via binary file "INPUT.WRK"
--------------------------------------------------------------------------------
readSpec :: String -> IO Spec
readSpec fileName = do
                handle <- openBinaryFile (fileName ++ ".SPC") ReadMode
                specHeader <- readSpecHeader handle
                specBlocks <- readSpecBlocks handle specHeader
                hClose handle
                return (specHeader, specBlocks)

--------------------------------------------------------------------------------
-- Writes data points to ISDA main program via binary file "OUTPUT.WRK"
--------------------------------------------------------------------------------

writeData :: String -> Data -> IO ()
writeData fileName (dataHeader, dataBlocks) = 
    do
        handle <- openBinaryFile (fileName ++ ".WRK") WriteMode
        writeDataHeader handle dataHeader
        writeDataBlocks handle dataHeader dataBlocks
        hClose handle

--------------------------------------------------------------------------------
-- Writes spectrum data to ISDA main program via binary file "OUTPUT.SPC"
--------------------------------------------------------------------------------
--writeSpec :: Spec -> IO ()
--writeSpec = writeSpecToFile "OUTPUT.SPC"

writeSpec :: String -> Spec -> IO ()
writeSpec fileName (specHeader, specBlocks) = 
    do
        handle <- openBinaryFile (fileName ++ ".SPC") WriteMode
        writeSpecHeader handle specHeader
        writeSpecBlocks handle specHeader specBlocks
        hClose handle

--------------------------------------------------------------------------------
-- Reads parameters from ISDA main program via text file "PARMS.TXT"
--------------------------------------------------------------------------------
readParams :: IO (Params)
readParams = do
    str <- readFile "PARMS.TXT"
    return (Params (Data.Map.fromList (keyValuePairs str)))

--------------------------------------------------------------------------------
-- PUBLIC FUNCTIONS
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Gets the value of the string parameter
--------------------------------------------------------------------------------
getString :: Params -> String -> (Maybe String)
getString (Params ht) k = Data.Map.lookup k ht

--getDefaultString :: Params -> String -> String
--getDefaultString (Params ht) k = Data.Map.findWithDefault "" k ht

--------------------------------------------------------------------------------
-- Gets the value of the parameter with arbitrary type 
--------------------------------------------------------------------------------
getParam :: Read a => Params -> String -> Maybe a
getParam params str = 
        case getString params str of 
            Nothing -> Nothing
            Just s1 -> (Just (read s1))


--------------------------------------------------------------------------------
-- Send feedback to ISDA main program via text file "OUTPUT.TXT"
--------------------------------------------------------------------------------
writeLn :: String -> IO ()
writeLn str = writeFile "OUTPUT.TXT" (str ++ "\n")

readState :: IO (ISDAState)
readState = 
    do
        params <- readParams
        state <- readStateFromFile "INPUT" params
        return state

readStateFromFile :: String -> Params -> IO (ISDAState)
readStateFromFile fileName params = 
    do
        Just mode <- return (getString params "CONFIGURATION")
        case head mode of 
            'D' -> readData fileName >>= (\d -> return (params, Left d))
            'S' -> readSpec fileName >>= (\s -> return (params, Right s))

writeState :: ISDAState -> IO ()
writeState = writeStateToFile "OUTPUT"

writeStateToFile :: String -> ISDAState -> IO ()
writeStateToFile fileName (params, datOrSpec) = 
    do
        let Just mode = getString params "CONFIGURATION"
        case last mode of 
                'S' -> case datOrSpec of Right s -> writeSpec fileName s
                'D' -> case datOrSpec of Left d -> writeData fileName d
