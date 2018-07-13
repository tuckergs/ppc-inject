

import Control.Applicative
import Control.Monad
import Data.Word

import HexStuff
import InstructionSet
import Types

showWord32 :: Word32 -> String
showWord32 = pad8 . show . Hex
  where pad8 = reverse . take 8 . (++ cycle "0") . reverse

main = main' instLs1 lblTbl1

main' instLs lblTbl = 
  forM_ (zip [0..(fromIntegral (length instLs) - 1)] instLs) $
    \(ind,inst) -> 
      let
        cd = (+0x043133cc) $ (4*) $ ind 
        pc = (+0xa32cc) $ (4*) $ ind 
      in putStrLn $ showWord32 cd ++ " " ++ (showWord32 $ instructionToWord lblTbl pc inst)

instLs1 :: [Instruction]
instLs1 =
    Ib False "two"
  : Iadd False False 7 6 5 
  : Imullw False False 7 6 5
  : Isubf False False 7 6 5
  : Iand False 7 6 5
  : Ior False 7 6 5
  : Islw False 7 6 5
  : Israw False 7 6 5
  : Isrw False 7 6 5
  : Ib True "debugfunc"
  : Icmpz False 0 False 7 6
  : Ibc False Equal "three"
  : Icmpzi False 0 False 7 0x0708
  : Ibc False GreaterThanEqual "three"
  : Iaddis 3 0 0x8055
  : Iaddi 3 3 0x3970
  : Imulli 7 6 0x1C
  : Iandi 7 6 0xFF
  : Irlwinm False 3 3 2 0 29
  : Ilbz 7 6 0x0000
  : Ilhz 7 6 0x0004
  : Ilha 7 6 0x0008
  : Ilwz 7 6 0x000C
  : Istb 7 6 0x0011
  : Isth 7 6 0x001E
  : Istw 7 6 0x0020
  : Iori 0 0 0
  : Iori 0 0 0
  : Iori 0 0 0
  : []

lblTbl1 = [("two",0xa32d4),("three",0xa32d8),("debugfunc",0xa4208)]
