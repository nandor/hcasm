{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- Chip16 Assembler written in Haskell
--------------------------------------------------------------------------------
module HCasm where


import           Data.Char
import           Data.Foldable hiding (foldl, mapM_)
import           Control.Monad
import           Control.Applicative ((<*), (*>), (<$>))
import           System.Environment
import           System.IO
import           Text.Parsec hiding (label)
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language
import           HC.Ops


--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
data Argument = ArgReg Int
              | ArgImm Int
              | ArgLabel String
              deriving (Eq, Show)


data Statement = Label String
               | Instr String [ Argument ]
               | Importbin
               deriving (Eq, Show)


readHex :: String -> Int
readHex
  = foldl (\a x -> a * 16 + digitToInt x) 0


readDec :: String -> Int
readDec
  = foldl (\a x -> a * 10 + digitToInt x) 0


eol :: Parsec String u ()
eol 
  = void $ many1 (oneOf "\r\n")


number :: Parsec String u Int
number 
  = asum [ readHex <$> (try $ string "#"  *> many1 hexDigit)
         , readHex <$> (try $ string "0x" *> many1 hexDigit)
         , readHex <$> (try $ many1 hexDigit <* char 'h')
         , readDec <$> (try $ many1 digit)
         ]


blank :: Parsec String u ()
blank
  = asum [ void (char ' ')
         , void (char '\t')
         , void (char ';' *> manyTill anyChar (lookAhead eol))
         ]


identifier :: Parsec String u String
identifier
  = many1 (alphaNum <|> char '_')


label :: Parsec String u Statement
label
  = Label <$> (identifier <* char ':' <* many blank <* optional eol)


argument :: Parsec String u Argument
argument
  = asum [ ArgReg . digitToInt <$> (char 'r' *> hexDigit)
         , ArgImm <$> number
         , ArgLabel <$> identifier
         ]


instruction :: Parsec String u Statement
instruction = do
  sepBy (many blank) eol
  ss <- many1 alphaNum
  many1 blank
  args <- sepBy argument (char ',')
  many blank
  eol <|> eof
  return (Instr ss args)


statement :: Parsec String u Statement
statement
  = asum [ try instruction
         , try label
         ]


parser :: Parsec String u [ Statement ]
parser
  = many statement <* eof


--------------------------------------------------------------------------------
-- Assembler
--------------------------------------------------------------------------------
assemble :: [ Statement ] -> IO ()
assemble xs = do
  mapM_ print xs


--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------
main :: IO ()
main
  = getArgs >>= \xs -> case xs of
      [ fi, fo ] -> main' fi fo
      _          -> putStrLn "Usage: HCasm input [output]"


main' :: String -> String -> IO ()
main' fi fo = do
  source <- readFile fi
  
  case parse parser "" source of
    Left err -> putStrLn $ "Parse Error: " ++ show err
    Right x -> do
      assemble x
