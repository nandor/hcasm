{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- Chip16 Assembler / Disassembler written in Haskell
--------------------------------------------------------------------------------


import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Digest.CRC32
import Data.Word
import System.Environment
import HC.Ops


data ROM
  = ROM { start :: Int, rom :: BSL.ByteString }
  deriving (Show)


-- Retrieves the header of the CH16 file
unpackCH16 :: Get (ROM)
unpackCH16 = do
  magic <- getByteString 4
  when (magic /= "CH16") $ fail "Invalid magic"
  
  skip 2

  size  <- getWord32le
  start <- getWord16le
  crc   <- getWord32le
  rom   <- getLazyByteString (fromIntegral size)
  when (crc32 rom /= crc) $ fail "Invalid checksum"

  return ( ROM (fromIntegral start) rom )


-- Dissassembles a ROM image
dissassemble :: ROM -> [ String ]
dissassemble (ROM start rom)
  = fetchOp rom
  where
    fetchOp rom
      | BSL.null rom = []
      | otherwise = case op' of
        Nothing -> []
        Just _  -> decode (fromJust op') : (fetchOp $ BSL.drop 4 rom )
      where
        [ op, rr, ll, hh ] = BSL.unpack $ BSL.take 4 rom
        op' = getOperator (fromIntegral op)

        decode :: Op -> String
        decode op = getName op 


-- Entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    [ fileIn ]          -> main' fileIn
    _                   -> putStrLn "Usage: hcdasm fileIn"


main' :: String -> IO ()
main' fileIn = do
  input <- BSL.readFile fileIn
  case runGetOrFail unpackCH16 input of
    Left ( _, _, msg ) -> putStrLn $ "Cannot read ROM: " ++ msg
    Right ( _, _, rom ) -> mapM_ putStrLn $ dissassemble rom

