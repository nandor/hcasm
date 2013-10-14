--------------------------------------------------------------------------------
-- Chip16 Assembler written in Haskell
--------------------------------------------------------------------------------


import Data.Char
import Control.Applicative
import Ops


uncomment :: String -> [ String ]
uncomment xs
  = filter (not . null) $ (fst . span (/= ';')) 
                       <$> (dropWhile isSpace) 
                       <$> lines xs


main :: IO ()
main = do
  input <- readFile "src/Herdle.asm"
  putStrLn $ unlines $ uncomment input


