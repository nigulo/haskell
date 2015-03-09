
module Utils.IO (
    readArray, 
    readValue, 
    writeArray, 
    writeValue,
    writeToFile,
    appendToFile,
    readFromFile) where

import System.IO
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String as UTF8

-- Reads an array from stream given by handle
readArray :: Storable a => Handle -> {-- Handle of open stream --}
                                                   Bool ->   {-- Specifies whether to read or not. If false
                                                                            the returned array is empty --}
                                                   Int ->    {-- Number of data elements to read --}
                                                   Int ->    {-- Size of data element in bytes --}
                                                   IO [a]         {-- Array containing the data elements --}
                                                   
readArray handle b numData dataSize = 
                                                if b then 
                                                        do
                                                                ptrData <- mallocArray numData
                                                                hGetBuf handle ptrData (numData  * dataSize)
                                                                values <- peekArray numData ptrData
                                                                free ptrData
                                                                return values
                                                else return []

--------------------------------------------------------------------------------

readValue :: Storable a => Handle -> Int -> IO a
readValue handle dataSize = do
        ptrData <- malloc
        hGetBuf handle ptrData dataSize
        value <- peek ptrData
        free ptrData
        return value

--------------------------------------------------------------------------------

writeArray :: Storable a => Handle -> [a] -> IO ()
writeArray handle values = case values of 
                [] -> return ()
                (val0:_) ->
                        do
                                ptrData <- mallocArray ((length values) * (sizeOf val0))
                                pokeArray ptrData values
                                hPutBuf handle ptrData ((length values) * (sizeOf val0))
                                free ptrData

--------------------------------------------------------------------------------
writeValue :: Storable a => Handle -> a -> IO ()
writeValue handle value = do
                        ptrData <- malloc
                        poke ptrData value
                        hPutBuf handle ptrData (sizeOf value)
                        free ptrData

writeToFile :: String -> String -> IO ()
writeToFile fileName str = do
    let
        byteStr = B.pack (UTF8.encode str)
    B.writeFile fileName byteStr

appendToFile :: String -> String -> IO ()
appendToFile fileName str = do
    let
        byteStr = B.pack (UTF8.encode str)
    B.appendFile fileName byteStr

readFromFile :: String -> IO String
readFromFile fileName = do
    byteStr <- B.readFile fileName
    return $ UTF8.decode $ B.unpack byteStr
