--------------------------------------------------------------------------------
-- Chip16 Assembler written in Haskell
--------------------------------------------------------------------------------
module HC.Ops where


import Data.Bits
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Control.Applicative

data Parameter
  = Bit Int
  | Imm4 Int
  | Imm8 Int
  | Imm16
  | RegX
  | RegY
  | RegZ
  | SP
  deriving (Eq, Show)


type Op
  = ( String, Int, [ Parameter ] )


jump :: [ ( String, Int ) ]
jump
  = [ ( "z",  0x0 )
    , ( "mz", 0x0 )
    , ( "nz", 0x1 )
    , ( "n",  0x2 )
    , ( "nn", 0x3 )
    , ( "p",  0x4 )
    , ( "o",  0x5 )
    , ( "no", 0x6 )
    , ( "a",  0x7 )
    , ( "ae", 0x8 )
    , ( "nc", 0x8 )
    , ( "b",  0x9 )
    , ( "c",  0x9 )
    , ( "mc", 0x9 )
    , ( "be", 0xA )
    , ( "g",  0xB )
    , ( "ge", 0xC )
    , ( "l",  0xD )
    , ( "le", 0xE )
    , ( "f",  0xF ) -- Reserved
    ]


operators :: [ Op ]
operators
  = [ ( "nop",      0x00, [ ] )
    , ( "cls",      0x01, [ ] )
    , ( "vblnk",    0x02, [ ] )
    , ( "bgc",      0x03, [ Imm4 4 ] )
    , ( "spr",      0x04, [ Imm16 ] )
    , ( "drw",      0x05, [ RegX, RegY, Imm16 ] )
    , ( "drw",      0x06, [ RegX, RegY, RegZ ] )
    , ( "rnd",      0x07, [ RegX, Imm16 ] )
    , ( "flip",     0x08, [ Bit 25, Bit 24] )
    , ( "snd0",     0x09, [ ] )
    , ( "snd1",     0x0A, [ Imm16 ] )
    , ( "snd2",     0x0B, [ Imm16 ] )
    , ( "snd3",     0x0C, [ Imm16 ] )
    , ( "snp",      0x0D, [ RegX, Imm16 ] )
    , ( "sng",      0x0E, [ Imm8 1, Imm16 ] )
    , ( "jmp",      0x10, [ Imm16 ] )
    , ( "",         0x12, [ Imm16 ] ) -- special
    , ( "jme",      0x13, [ RegX, RegY, Imm16 ] )
    , ( "call",     0x14, [ Imm16 ] )
    , ( "ret",      0x15, [ ] )
    , ( "jmp",      0x16, [ RegX ] )
    , ( "",         0x17, [ Imm16 ] ) -- special
    , ( "call",     0x18, [ Imm16 ] )
    , ( "ldi",      0x20, [ RegX, Imm16 ] )
    , ( "ldi",      0x21, [ SP, Imm16 ] )
    , ( "ldm",      0x22, [ RegX, Imm16] )
    , ( "ldm",      0x23, [ RegX, RegY ] )
    , ( "mov",      0x24, [ RegX, RegY ] )
    , ( "stm",      0x30, [ RegX, Imm16 ] )
    , ( "stm",      0x31, [ RegX, RegY ] )
    , ( "addi",     0x40, [ RegX, Imm16 ] )
    , ( "add",      0x41, [ RegX, RegY ] )
    , ( "add",      0x42, [ RegX, RegY, RegZ ] )
    , ( "subi",     0x50, [ RegX, Imm16 ] )
    , ( "sub",      0x51, [ RegX, RegY ] )
    , ( "sub",      0x52, [ RegX, RegY, RegZ ] )
    , ( "cmpi",     0x53, [ RegX, Imm16 ] )
    , ( "cmp",      0x54, [ RegX, RegY ] )
    , ( "andi",     0x60, [ RegX, Imm16 ] )
    , ( "and",      0x61, [ RegX, RegY ] )
    , ( "and",      0x62, [ RegX, RegY, RegZ ] )
    , ( "tsti",     0x63, [ RegX, Imm16 ] )
    , ( "tst",      0x64, [ RegX, RegY ] )
    , ( "ori",      0x70, [ RegX, Imm16] )
    , ( "or",       0x71, [ RegX, RegY ] )
    , ( "or",       0x72, [ RegX, RegY, RegZ ] )
    , ( "xori",     0x80, [ RegX, Imm16 ] )
    , ( "xor",      0x81, [ RegX, RegY ] )
    , ( "xor",      0x82, [ RegX, RegY, RegZ ] )
    , ( "muli",     0x90, [ RegX, Imm16 ] )
    , ( "mul",      0x91, [ RegX, RegY ] )
    , ( "mul",      0x92, [ RegX, RegY, RegZ ] )
    , ( "divi",     0xA0, [ RegX, Imm16 ] )
    , ( "div",      0xA1, [ RegX, RegY] )
    , ( "div",      0xA2, [ RegX, RegY, RegZ ] )
    , ( "shl",      0xB0, [ RegX, Imm4 4 ] )
    , ( "shr",      0xB1, [ RegX, Imm4 4 ] )
    , ( "sar",      0xB2, [ RegX, Imm4 4 ] )
    , ( "shl",      0xB3, [ RegX, RegY ] )
    , ( "shr",      0xB4, [ RegX, RegY ] )
    , ( "sar",      0xB5, [ RegX, RegY ] )
    , ( "push",     0xC0, [ RegX ] )
    , ( "pop",      0xC1, [ RegX ] )
    , ( "pushall",  0xC2, [ ] )
    , ( "popall",   0xC3, [ ] )
    , ( "pushf",    0xC4, [ ] )
    , ( "popf",     0xC5, [ ] )
    , ( "pal",      0xD0, [ Imm16 ] )
    , ( "pal",      0xD1, [ RegX ] )
    ]


getOperatorByName :: String -> [ Op ]
getOperatorByName name
  = filter (\( name', _, _ ) -> name == name') operators


getOperatorByCode :: Int -> Maybe Op
getOperatorByCode n
  = find (\( _, n', _ ) -> n == n' ) operators


getJumpByCode :: Int -> String
getJumpByCode n
  = fst . fromJust $ find (\( j, n' ) -> n' == n ) jump


getJumpByName :: String -> Maybe Int
getJumpByName j
  = snd <$> find (\( j', n ) -> j' == j ) jump
  
