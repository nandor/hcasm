--------------------------------------------------------------------------------
-- Chip16 Assembler written in Haskell
--------------------------------------------------------------------------------
module HC.Ops where


import Data.Bits
import Data.Int
import Data.List

data Parameter
  = Bit Int
  | Imm4 Int
  | Imm8 Int
  | Imm16
  | RegX
  | RegY
  | RegZ
  deriving (Eq, Show)


type Op
  = ( String, Int, [ Parameter ] )


operators :: [ Op ]
operators
  = [ ( "nop",      0x00, [ ] )
    , ( "cls",      0x01, [ ] )
    , ( "vlbnk",    0x02, [ ] )
    , ( "bgc",      0x03, [ Imm4 5 ] )
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
    , ( "j",        0x12, [ Imm16 ] ) -- special
    , ( "jme",      0x13, [] )
    , ( "call",     0x14, [] )
    , ( "ret",      0x15, [] )
    , ( "jmp",      0x16, [] )
    , ( "c",        0x17, [] ) -- special
    , ( "call",     0x18, [] )
    , ( "ldi",      0x20, [] )
    , ( "ldi",      0x21, [] )
    , ( "ldm",      0x22, [] )
    , ( "ldm",      0x23, [] )
    , ( "mov",      0x23, [] )
    , ( "stm",      0x30, [] )
    , ( "stm",      0x31, [] )
    , ( "addi",     0x40, [] )
    , ( "add",      0x41, [] )
    , ( "add",      0x42, [] )
    , ( "subi",     0x50, [] )
    , ( "sub",      0x51, [] )
    , ( "sub",      0x52, [] )
    , ( "cmpi",     0x53, [] )
    , ( "cmp",      0x54, [] )
    , ( "andi",     0x60, [] )
    , ( "and",      0x61, [] )
    , ( "and",      0x62, [] )
    , ( "tsti",     0x63, [] )
    , ( "tst",      0x64, [] )
    , ( "ori",      0x70, [] )
    , ( "or",       0x70, [] )
    , ( "or",       0x71, [] )
    , ( "or",       0x72, [] )
    , ( "xori",     0x80, [] )
    , ( "xor",      0x81, [] )
    , ( "xor",      0x82, [] )
    , ( "muli",     0x90, [] )
    , ( "mul",      0x91, [] )
    , ( "mul",      0x92, [] )
    , ( "divi",     0xA0, [] )
    , ( "div",      0xA1, [] )
    , ( "div",      0xA2, [] )
    , ( "shl",      0xB0, [] )
    , ( "shr",      0xB1, [] )
    , ( "sar",      0xB2, [] )
    , ( "shl",      0xB3, [] )
    , ( "shr",      0xB4, [] )
    , ( "sar",      0xB5, [] )
    , ( "push",     0xC0, [] )
    , ( "pop",      0xC1, [] )
    , ( "pushall",  0xC2, [] )
    , ( "popall",   0xC3, [] )
    , ( "pushf",    0xC4, [] )
    , ( "popf",     0xC5, [] )
    , ( "pal",      0xD0, [] )
    , ( "pal",      0xD1, [] )
    ]


getOperator :: Int -> Maybe Op
getOperator n
  = find (\( _, n', _ ) -> n == n' ) operators


getName :: Op -> String
getName ( name, _, _ )
  = name


emit :: Int8 -> [ String ] -> [ Parameter ] -> Int
emit op ss ps
  = 0
  
