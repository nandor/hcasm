{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- Chip16 Assembler / Disassembler written in Haskell
--------------------------------------------------------------------------------
module Main where


import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import Control.Monad
import Data.Bits
import Data.Binary.Get
import Data.Digest.CRC32
import Data.List
import Data.Maybe
import Data.Word
import Numeric
import System.Environment
import Text.Printf
import HC.Ops


-- Stores information about an image
data ROM
  = ROM { start :: Int, rom :: [ [ Int ] ] }
  deriving (Show)


-- Retrieves the header & contents of the CH16 file
groupWords :: BSL.ByteString -> [ [ Int ] ]
groupWords xs
  | BSL.length xs < 4 = [ ]
  | otherwise = map fromIntegral (BSL.unpack ls) : groupWords rs 
  where
    ( ls, rs ) = BSL.splitAt 4 xs


unpackCH16 :: Get (ROM)
unpackCH16 = do
  magic <- getByteString 4
  when (magic /= "CH16") $ fail "Invalid magic"
  
  skip 1
  ver   <- getWord8
  
  size  <- getWord32le
  start <- getWord16le
  crc   <- getWord32le
  rom   <- getLazyByteString (fromIntegral size)
  when (crc32 rom /= crc) $ fail "Invalid checksum"

  return (ROM (fromIntegral start) (groupWords rom))
  

-- Dissassembles a ROM image
dissassemble :: ROM -> [ String ]
dissassemble (ROM start rom)
  = map trans rom
  where
    trans dw
      | op == 0x12 = 'j' : cond
      | op == 0x17 = 'c' : cond
      | otherwise = name ++ " " ++ (concat $ intersperse "," $ map trans' arg)
      where
        [ op, rr, ll, hh ] = dw
        ( name, _, arg ) = fromMaybe (operators !! 0) (getOperator op)
        dw' = foldr (\x a -> (a `shiftL` 8) .|. x) 0 dw


        trans' SP       = "sp"
        trans' RegX     = printf "r%1x" $ rr .&. 0x0F
        trans' RegY     = printf "r%1x" $ (rr .&. 0xF0) `shiftR` 4
        trans' RegZ     = printf "r%1x" $ ll .&. 0x0F
        trans' (Bit x)  = if testBit dw' x then "1" else "0"
        trans' (Imm8 x) = printf "r%2x" $ dw !! x
        trans' Imm16    = printf "0x%04x" $ (hh `shiftL` 8) .|. ll
        trans' (Imm4 x) = let byte = x `div` 2
                              nibble = 1 - x `mod` 2
                          in printf "r%1x" $ (dw !! byte) `shiftR` nibble
      
        cond = (getJump (rr .&. 0x0F)) ++ " " ++ trans' Imm16


-- Entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    [ fileIn ] -> main' fileIn
    _          -> putStrLn "Usage: hcdasm fileIn"


main' :: String -> IO ()
main' fileIn = do
  input <- BSL.readFile fileIn
  case runGetOrFail unpackCH16 input of
    Left ( _, _, msg )  -> putStrLn $ "Cannot read ROM: " ++ msg
    Right ( _, _, rom ) -> mapM_ putStrLn $ dissassemble rom

