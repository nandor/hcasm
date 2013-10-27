{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- Chip16 Assembler written in Haskell
--------------------------------------------------------------------------------
module HCasm where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Char
import           Data.Bits
import           Data.Foldable (asum)
import           Data.Maybe
import           Data.Word
import           Control.Applicative ((<*), (*>), (<$>))
import           Control.Monad
import           Control.Monad.Instances
import           Numeric (showHex)
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
              | ArgSP
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
    , try $ string "sp" >> return ArgSP
    ]
  

-- Parses an identifier followed by ':'
label :: Parsec String u (Tag Statement)
label
  = getPosition >>= \pos -> (Tag pos . Label) <$> asum
    [ identifier <* char ':'
    , char ':' *> identifier
    ]


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
parser :: String -> Either String [ ( Tag Statement ) ]
parser source
  = case parse (many1 statement <* eof) "" source of
      Left  err -> Left $ show err
      Right ast -> Right ast


--------------------------------------------------------------------------------
-- Assembler
--------------------------------------------------------------------------------
linkLabels :: [ (Tag Statement) ] -> Map String Int
linkLabels
  = Map.fromList . fst . foldl step ( [ ], 0 )
  where
    step ( ls, addr ) (Tag _pos stmt)
      = case stmt of
        Label xs       -> ( ( xs, addr ) : ls, addr)
        Instr op arg   -> ( ls, addr + 4 )
        DeclByte bytes -> ( ls, addr + (length bytes) )


assembler :: [ (Tag Statement) ] -> Either String ByteString
assembler stmts
  = BS.pack <$> concat <$> mapM (link >=> emitStmnt) stmts
  where
    -- Get the address of each label
    labels = linkLabels stmts


    -- Replace labels with their addresss
    link (Tag ip (Instr name args))
      = Tag ip . Instr name <$> mapM replace args
      where
        replace (Tag ap (ArgLabel label))
          = case Map.lookup label labels of
              Nothing -> Left $ (show ap) ++ " Invalid label: " ++ label
              Just pt -> Right (Tag ap (ArgImm pt))
        replace arg
          = Right arg
    link stmnt
      = Right stmnt


    -- Emit the opcode
    emitStmnt (Tag ip (Label _))
      = Right [ ]
    emitStmnt (Tag ip (DeclByte bs))
      = Right bs
    emitStmnt (Tag ip (Instr xx@(x : xs) args))
      = map fromIntegral <$> case getOperatorByName xx of
          o : os         -> match (o : os)
          [ ] | x == 'j' -> condition 0x12 xs args
          [ ] | x == 'c' -> condition 0x17 xs args
          _              -> Left $ (show ip) ++ " Invalid instruction: " ++ xx
      where
        match ((_, op, args') : ops)
          = case emitArg args args' [ 0, 0, 0 ] of
              Right dw            -> Right $ (fromIntegral op) : dw
              Left err | null ops -> Left err
              Left err            -> match ops
        match [ ]
          = Left $ (show ip) ++ " Invalid instruction: " ++ xx


        -- Emit the code for a conditional operator
        condition op jmp [ Tag ap (ArgImm addr) ]
          = case getJumpByName jmp of
              Nothing -> Left $ (show ap) ++ " Invalid condition: " ++ jmp
              Just n  -> Right $ [ op
                                 , n
                                 , addr .&. 0xFF 
                                 , addr `shiftR` 8
                                 ]
        condition op _ (Tag ap _ : _ )
          = Left $ (show ap) ++ " Expecting imm16"


        -- Insert arguments into the opcode
        emitArg ((Tag ap (ArgImm arg)) : as) (arg' : bs) dw@[ a, b, c ]
          = case arg' of
              Imm16  -> Right [ a
                              , arg .&. 0xFF
                              , arg `shiftR` 8
                              ] >>= emitArg as bs
              Imm8 x -> Left "Unimplemented"
              Imm4 x -> Left "Unimplemented"
        emitArg ((Tag ap (ArgReg arg)) : as) (arg' : bs) dw@[ a, b, c ]
          = case arg' of
              RegX  -> Right [ (a .&. 0xF0) .|. arg
                             , b
                             , c
                             ] >>= emitArg as bs
              RegY  -> Right [ (a .&. 0x0F) .|. (arg `shiftL` 4)
                             , b
                             , c
                             ] >>= emitArg as bs
              RegZ  -> Right [ a
                             , (b .&. 0xF0) .|. arg
                             , c
                             ] >>= emitArg as bs
              p     -> Left $ (show ap) ++ " Expecting " ++ (show p)
        emitArg [ ] [ ] dw
          = Right dw
        emitArg _ [ ] _
          = Left $ (show ip) ++ " Too many arguments"
        emitArg [ ] (p : ps) _
          = Left $ (show ip) ++ " Expecting " ++ (show p)

    -- Concatenate the code generated by two statements
    append (Left err) op 
      = Left err
    append (Right bc) (Left err)
      = Left err
    append (Right bc) (Right op)
      = Right (BS.append bc (BS.pack op))


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
  
  case parser source >>= assembler of
    Left err    -> putStrLn err
    Right code  -> BS.writeFile fo code
