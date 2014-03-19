{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- Chip16 Assembler written in Haskell
--------------------------------------------------------------------------------
module Main where


import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Char
import           Data.Digest.CRC32
import           Data.Bits
import           Data.Binary.Put
import           Data.Foldable (asum)
import           Data.List
import           Data.Maybe
import           Data.Word
import           Debug.Trace
import           Control.Applicative ((<*), (*>), (<$>))
import           Control.Monad
import           Control.Monad.Instances
import           Numeric (showHex)
import           Text.Parsec hiding (label)
import           Text.Parsec.Pos
import           Text.Parsec.Language
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           HC.Ops


--------------------------------------------------------------------------------
-- Syntax tree (sort of)
--------------------------------------------------------------------------------
type Statements = [ Statement ]
type Binaries = [ ( String, ByteString ) ]


data Argument = ArgReg Int
              | ArgImm Int
              | ArgSP
              | ArgLabel String
              deriving (Eq, Show)


data Statement = Label String
               | Instr String [ Tag Argument ]
               | Importbin String Int Int String
               | DeclByte [ Word8 ]
               | DeclWord [ Word16 ]
               | Equ String Int
               deriving (Eq, Show)


data Tag a = Tag SourcePos a
           deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- Reads a number from a string
readBase :: Int -> String -> Int
readBase base
  = foldl (\a x -> a * base + digitToInt x) 0

-- Convers a word to two bytes
wordToByte :: Int -> [ Word8 ]
wordToByte num
  = map fromIntegral [ num .&. 0xFF, (num .&. 0xFF00) `shiftL` 8 ]

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

-- Multiple blank lines
manyBlank :: Parsec String u ()
manyBlank
  = void $ sepBy (many blank) eol


-- Parses a decimal or a hexadecimal constant
number :: Parsec String u Int
number
  = asum [ readBase 16 <$> (try $ string "#"  *> many1 hexDigit)
         , readBase 16 <$> (try $ string "0x" *> many1 hexDigit)
         , readBase 16 <$> (try $ many1 hexDigit <* char 'h')
         , readBase 10 <$> (try $ many1 digit)
         ]

-- Parses an identifier
identifier :: Parsec String u String
identifier = do
  first <- letter <|> char '_'
  others <- many1 (alphaNum <|> char '_')
  return (first : others)

-- Parses a command argument
argument :: Parsec String u (Tag Argument)
argument
  = getPosition >>= \pos -> (Tag pos) <$> asum
    [ ArgReg . digitToInt <$> (char 'r' *> hexDigit)
    , ArgLabel <$> identifier
    , ArgImm <$> number
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

  args <- option [] $ do
    many1 (char ' ' <|> char '\t')
    sepBy argument (char ',' >> many (char ' ' <|> char '\t'))

  return (Tag pos $ Instr ss args)


-- Parses a data declaration
declByte :: Parsec String u (Tag Statement)
declByte = do
  pos <- getPosition

  string "db"
  args <- asum [ try string', try commaSep, return [] ]

  return (Tag pos $ DeclByte (map fromIntegral $ args))
  where
    commaSep = do
      many1 (char ' ' <|> char '\t')
      sepBy number (char ',' *> many (char ' ' <|> char '\t'))

    string' = do
      many1 (char ' ' <|> char '\t')
      char '"'
      str <- many (noneOf "\"")
      char '"'
      return (map ord str)

-- Parses a word array declaration
declWord :: Parsec String u (Tag Statement)
declWord = do
  pos <- getPosition

  string "dw"
  args <- option [] $ do
    many1 (char ' ' <|> char '\t')
    sepBy number (char ',' *> many (char ' ' <|> char '\t'))

  return $ Tag pos (DeclByte (concatMap wordToByte $ args))

-- Parses a binary file inclusion
importbin :: Parsec String u (Tag Statement)
importbin = do
  pos <- getPosition

  string "importbin"
  many1 blank

  file <- manyTill anyChar blank
  many blank
  offset <- readBase 10 <$> many1 digit
  many1 blank
  size <- readBase 10 <$> many1 digit
  many1 blank
  lbl <- identifier

  return (Tag pos (Importbin file offset size lbl))

-- Alias
equ :: Parsec String u (Tag Statement)
equ = do
  pos <- getPosition
  name <- identifier
  many blank
  string "equ"
  many blank
  value <- number
  return (Tag pos (Equ name value))

-- Parse a single statement
statement :: Parsec String u (Tag Statement)
statement
  = manyBlank *> asum stmts <* manyBlank
  where
    stmts
      = [ try equ
        , try declByte
        , try declWord
        , try importbin
        , try label
        , try instruction
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
splitWord :: Int -> [ Word8 ]
splitWord x
  = [ fromIntegral $ (x `shiftR` y) .&. 0xFF | y <- [0,8..24] ]


replaceByte :: Int -> Int -> Int -> Int
replaceByte dw i what
  = (dw .&. (complement (0xFF `shiftL` (i * 8))) .|. (what `shiftL` (i * 8)))


replaceNibble :: Int -> Int -> Int -> Int
replaceNibble dw i what
  = (dw .&. (complement (0xF `shiftL` (i * 4))) .|. (what `shiftL` (i * 4)))


replaceBit :: Int -> Int -> Int -> Int
replaceBit dw i what
  = (dw .&. (complement (1 `shiftL` i)) .|. (what `shiftL` i))


assembler ::  Binaries -> [ (Tag Statement) ] ->  Either String ByteString
assembler bin stmts
  = BS.pack <$> concat <$> mapM (link >=> emitStmnt) stmts'
  where
    -- Get the address of each label
    stmts' = order stmts [ ]
    labels = linkLabels stmts'

    -- Find out the adress of each label
    linkLabels
      = Map.fromList . fst . foldl step ( [ ], 0 )
      where
        step ( ls, addr ) (Tag _pos stmt)
          = case stmt of
            Label xs           -> ( ( xs, addr ) : ls, addr )
            Instr op arg       -> ( ls, addr + 4 )
            Equ xs val         -> ( ( xs, val ) : ls, addr )
            DeclByte bytes     -> ( ls, addr + (length bytes) )
            Importbin _ _ s xs -> ( ( xs, addr ) : ls, addr + s)

    -- Moves importbin statements at the end
    order [] xs
      = reverse xs
    order (ib@(Tag ip (Importbin _ _ _ _)) : ss) xs
      = order ss (ib : xs)
    order (s : ss) xs
      = s : order ss xs

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
    emitStmnt (Tag ip (Equ _ _))
      = Right [ ]
    emitStmnt (Tag ip (Label _))
      = Right [ ]
    emitStmnt (Tag ip (DeclByte bs))
      = Right bs
    emitStmnt (Tag ip (Importbin file off size label))
      = case lookup file bin of
          Nothing -> Left  $ " Missing binary: " ++ file
          Just bs -> Right $ BS.unpack (BS.take size' (BS.drop off' bs))
      where
        off' = fromIntegral off
        size' = fromIntegral size
    emitStmnt (Tag ip (Instr xx@(x : xs) args))
      = splitWord <$> case getOperatorByName xx of
          o : os         -> match (o : os)
          [ ] | x == 'j' -> emitJump xs args 0x12
          [ ] | x == 'c' -> emitJump xs args 0x17
          _              -> Left $ (show ip) ++ " Invalid instruction: " ++ xx
      where
        match ((_, op, args') : ops)
          = case emitArg args args' op of
              Right dw            -> Right dw
              Left err | null ops -> Left err
              Left err            -> match ops
        match [ ]
          = Left $ (show ip) ++ " Invalid instruction: " ++ xx

        -- Emit a jump instruction
        emitJump jmp args op
          = case getJumpByName jmp of
              Nothing -> Left $ " Invalid jump: " ++ jmp
              Just x  -> emitArg args [ Imm16 ] $ replaceNibble op 2 x

        -- Insert arguments into the opcode
        emitArg ((Tag ap (ArgImm arg)) : as) (arg' : bs) dw
          = emitArg as bs =<< case arg' of
              Imm16  -> Right $ (dw .&. 0xFFFF) .|. (arg `shiftL` 16)
              Imm8 x -> Right $ replaceByte   dw x arg
              Imm4 x -> Right $ replaceNibble dw x arg
              Bit x  -> Right $ replaceBit    dw x arg
              p      -> Left  $ (show ap) ++ " Expecting " ++ (show p)
        emitArg ((Tag ap (ArgReg arg)) : as) (arg' : bs) dw
          = emitArg as bs =<< case arg' of
              RegX  -> Right $ replaceNibble dw 2 arg
              RegY  -> Right $ replaceNibble dw 3 arg
              RegZ  -> Right $ replaceNibble dw 4 arg
              p     -> Left  $ (show ap) ++ " Expecting " ++ (show p)
        emitArg ((Tag ap ArgSP) : as) (SP : bs) dw
          = Right dw >>= emitArg as bs
        emitArg [ ] [ ] dw
          = Right dw
        emitArg _ [ ] _
          = Left $ (show ip) ++ " Too many arguments"
        emitArg [ ] (p : ps) _
          = Left $ (show ip) ++ " Expecting " ++ (show p)


header :: ByteString -> Put
header byteCode = do
  putByteString "CH16"
  putWord16le 0x1100
  putWord32le $ fromIntegral (BS.length byteCode)
  putWord16le $ 0x0000
  putWord32le $ crc32 byteCode
  putLazyByteString byteCode


--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------
readBin :: String -> [ ( Tag Statement ) ] -> IO (Either String Binaries)
readBin root statements
  = mapM readBin' files >>= (\bs -> return (concat <$> sequence bs))
  where
    files = nub $ concatMap getFileName statements

    getFileName (Tag _ (Importbin file _ _ _))
      = [ file ]
    getFileName _
      = [ ]

    readBin' file = do
      let path = root ++ "/" ++ file

      exists <- doesFileExist path

      if not exists
        then return (Left $ "Not found: " ++ file)
        else do
          handle <- openFile path ReadMode
          open <- hIsOpen handle
          if not open
            then return (Left $ "Cannot open: " ++ file)
            else do
              contents <- BS.hGetContents handle
              return (Right [ ( file, contents ) ])


main :: IO ()
main
  = getArgs >>= \xs -> case xs of
      [ fi, fo ] -> main' fi fo
      _          -> putStrLn "Usage: HCasm input [output]"


main' :: String -> String -> IO ()
main' fi fo = do
  source <- readFile fi

  case parser source of
    Left err -> putStrLn err
    Right ast -> do
      bins <- readBin (takeDirectory fi) ast
      case bins of
        Left err -> putStrLn err
        Right bin -> do
          case assembler bin ast of
            Left err -> putStrLn err
            Right code -> BS.writeFile fo . runPut . header $ code
