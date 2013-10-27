{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- Chip16 Assembler written in Haskell
--------------------------------------------------------------------------------
module HCasm where


import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8
import           Data.Char
import           Data.Foldable (asum)
import           Data.Maybe
import           Data.Word
import           Control.Applicative ((<*), (*>), (<$>))
import           Control.Monad
import           Text.Parsec hiding (label)
import           Text.Parsec.Pos
import           Text.Parsec.Language
import           System.Environment
import           System.IO
import           HC.Ops


--------------------------------------------------------------------------------
-- Syntax tree (sort of)
--------------------------------------------------------------------------------
data Argument = ArgReg Int
              | ArgImm Int
              | ArgLabel String
              deriving (Eq, Show)


data Statement = Label String
               | Instr String [ Tag Argument ]
               | Importbin String Int Int String
               | DeclByte [ Word8 ]
               deriving (Eq, Show)


data Tag a = Tag SourcePos a
           deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- Parses the newline character
eol :: Parsec String u ()
eol 
  = void $ many1 (oneOf "\r\n")


-- Skips through blank space and comments
blank :: Parsec String u ()
blank
  = asum [ void (char ' ')
         , void (char '\t')
         , void (char ';' *> manyTill anyChar (lookAhead eol))
         ]


-- Parses a decimal or a hexadecimal constant
number :: Parsec String u Int
number 
  = asum [ readBase 16 <$> (try $ string "#"  *> many1 hexDigit)
         , readBase 16 <$> (try $ string "0x" *> many1 hexDigit)
         , readBase 16 <$> (try $ many1 hexDigit <* char 'h')
         , readBase 10 <$> (try $ many1 digit)
         ]
  where
    readBase base
      = foldl (\a x -> a * base + digitToInt x) 0


-- Parses an identifier
identifier :: Parsec String u String
identifier
  = many1 (alphaNum <|> char '_')


-- Parses a command argument
argument :: Parsec String u (Tag Argument)
argument
  = getPosition >>= \pos -> (Tag pos) <$> asum 
    [ ArgReg . digitToInt <$> (char 'r' *> hexDigit)
    , ArgImm <$> number
    , ArgLabel <$> identifier
    ]
  

-- Parses an identifier followed by ':'
label :: Parsec String u (Tag Statement)
label
  = getPosition >>= \pos -> (Tag pos . Label) <$>
    (identifier <* char ':')


-- Parses an instruction followed by arguments
instruction :: Parsec String u (Tag Statement)
instruction = do
  pos <- getPosition

  ss <- many1 alphaNum
  many1 blank

  args <- sepBy argument (char ',')

  return (Tag pos $ Instr ss args)


-- Parses a data declaration
declByte :: Parsec String u (Tag Statement)
declByte = do
  pos <- getPosition

  string "db"
  many1 blank

  args <- sepBy number (char ',')

  return (Tag pos $ DeclByte (map fromIntegral $ args))


-- Parse a single statement
statement :: Parsec String u (Tag Statement)
statement
  = sepBy (many blank) eol *> asum stmts <* many blank <* (eol <|> eof)
  where
    stmts 
      = [ try declByte
        , try instruction
        , try label
        ]


-- Parse a list of statements
parser :: Parsec String u [ (Tag Statement) ]
parser
  = many statement <* eof


--------------------------------------------------------------------------------
-- Assembler
--------------------------------------------------------------------------------


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
    Right x -> mapM_ print x
