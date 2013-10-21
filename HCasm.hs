{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- Chip16 Assembler written in Haskell
--------------------------------------------------------------------------------
module HCasm where


import           Data.Char
import           Control.Monad
import           System.Environment
import           System.IO
import           Text.ParserCombinators.Parsec hiding (label)
import qualified Text.ParserCombinators.Parsec.Token as P
import           Text.ParserCombinators.Parsec.Language
import           HC.Ops


uncomment :: String -> [ String ]
uncomment xs
  = filter (not . null) $ (fst . span (/= ';')) 
                       <$> (dropWhile isSpace) 
                       <$> lines xs


data Statement = Label String
               | Instr String [ Argument ]
               deriving (Eq, Show)


eol :: Parser ()
eol
  = many1 (oneOf "\r\n") >> return ()


blank :: Parser ()
blank
  =   (char ' ' >> return ())
  <|> (char '\t' >> return ())
  <|> (char ';' >> manyTill anyChar (lookAhead eol) >> return ())


identifier :: Parser String
identifier
  = many1 (oneOf ('_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))


label :: Parser Statement
label = do
  char ':'
  ss <- identifier
  many blank
  return (Label ss)


arg :: Parser Argument
arg
  =   ((char '#') >> many hexDigit >>= (\xs -> return (ArgImm (read xs))))
  <|> ((char 'r') >> hexDigit >>= (\xs -> return (ArgReg (digitToInt xs))))
  <|> (many1 digit >>= (\xs -> return (ArgImm (read xs))))
  <|> (identifier >>= (\xs -> return (ArgLabel xs)))


instruction :: Parser Statement
instruction = do
  many blank
  ss <- many1 alphaNum
  many blank
  stmts <- sepBy arg (char ',')
  many blank
  return (Instr ss stmts)


statement :: Parser Statement
statement
  = instruction <|> label


parser :: Parser [ Statement ]
parser = do
  stmts <- sepBy statement eol
  -- eof
  return stmts


--------------------------------------------------------------------------------
-- Assembler
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
    Right x -> putStrLn $ show x
