
module InstructionSet where

import Data.Bits
import Data.Word

import Helpers
import Types

-- Takes our label table, where the instruction is, the instruction and returns the machine code for it
-- The check for whether a label has been defined is not here
-- Note that b and bc, if you give it an address that is too far away 
-- (that is, the distance can't be captured by the instruction), 
-- then this will fail silently. Somewhere, I should put a check in
instructionToWord :: LabelTable -> Word32 -> Instruction -> Word32
-- b
instructionToWord tbl pc (Ib _LK _Lbl) = 
  opPart .|. addrPart .|. lkPart
  where
    opPart = (18 `shiftL` 26)
    addrPart = (.&. 0x03FFFFFC) $ (-) (brutalLookup _Lbl tbl) pc
    lkPart = enumToNum _LK
-- bc
instructionToWord tbl pc (Ibc _LK _CondOp _Lbl) =
  opPart .|. condPart .|. addrPart .|. lkPart
  where
    opPart = (16 `shiftL` 26)
    condPart = case _CondOp of
      GreaterThanEqual -> 0x00800000
      LessThanEqual -> 0x00810000
      NotEqual -> 0x00820000
      LessThan -> 0x01800000
      GreaterThan -> 0x01810000
      Equal -> 0x01820000
      Other bo bi -> (fromIntegral bo `shiftL` 21) .|. (fromIntegral bi `shiftL` 16)
    addrPart = (.&. 0x0000FFFC) $ (-) (brutalLookup _Lbl tbl) pc
    lkPart = enumToNum _LK
-- cmp / cmpl
instructionToWord _ _ (Icmpz _Unsigned _CrfD _L _RA _RB) =
  opPart .|. crfPart .|. lPart .|. rAPart .|. rBPart .|. unsignedPart
  where 
    opPart = 31 `shiftL` 26
    crfPart = fromIntegral _CrfD `shiftL` 23
    lPart = (`shiftL` 21) $ enumToNum _L
    rAPart = fromIntegral _RA `shiftL` 16
    rBPart = fromIntegral _RB `shiftL` 11
    unsignedPart = 
      if _Unsigned
        then 32 `shiftL` 1
        else 0
-- cmpi / cmpli
instructionToWord _ _ (Icmpzi _Unsigned _CrfD _L _RA _SIMM) =
  oplPart .|. crfPart .|. lPart .|. rAPart .|. simmPart
  where
    oplPart =
      if _Unsigned
        then 10 `shiftL` 26
        else 11 `shiftL` 26
    crfPart = fromIntegral _CrfD `shiftL` 23
    lPart = (`shiftL` 21) $ enumToNum _L
    rAPart = fromIntegral _RA `shiftL` 16
    simmPart = fromIntegral _SIMM
-- add
instructionToWord _ _ (Iadd _OE _Rc _RD _RA _RB) = 
  opPart .|. rDPart .|. rAPart .|. rBPart .|. oePart .|. opPart2 .|. rcPart
  where
    opPart = 31 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    rBPart = fromIntegral _RB `shiftL` 11
    oePart = enumToNum _OE `shiftL` 10
    opPart2 = 266 `shiftL` 1
    rcPart = enumToNum _Rc 
-- mullw
instructionToWord _ _ (Imullw _OE _Rc _RD _RA _RB) = 
  opPart .|. rDPart .|. rAPart .|. rBPart .|. oePart .|. opPart2 .|. rcPart
  where
    opPart = 31 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    rBPart = fromIntegral _RB `shiftL` 11
    oePart = enumToNum _OE `shiftL` 10
    opPart2 = 235 `shiftL` 1
    rcPart = enumToNum _Rc 
-- subf
instructionToWord _ _ (Isubf _OE _Rc _RD _RA _RB) = 
  opPart .|. rDPart .|. rAPart .|. rBPart .|. oePart .|. opPart2 .|. rcPart
  where
    opPart = 31 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    rBPart = fromIntegral _RB `shiftL` 11
    oePart = enumToNum _OE `shiftL` 10
    opPart2 = 40 `shiftL` 1
    rcPart = enumToNum _Rc 
-- and
instructionToWord _ _ (Iand _Rc _RA _RS _RB) = 
  opPart .|. rSPart .|. rAPart .|. rBPart .|. opPart2 .|. rcPart
  where
    opPart = 31 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    rBPart = fromIntegral _RB `shiftL` 11
    opPart2 = 28 `shiftL` 1
    rcPart = enumToNum _Rc 
-- or
instructionToWord _ _ (Ior _Rc _RA _RS _RB) = 
  opPart .|. rSPart .|. rAPart .|. rBPart .|. opPart2 .|. rcPart
  where
    opPart = 31 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    rBPart = fromIntegral _RB `shiftL` 11
    opPart2 = 444 `shiftL` 1
    rcPart = enumToNum _Rc 
-- slw
instructionToWord _ _ (Islw _Rc _RA _RS _RB) = 
  opPart .|. rSPart .|. rAPart .|. rBPart .|. opPart2 .|. rcPart
  where
    opPart = 31 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    rBPart = fromIntegral _RB `shiftL` 11
    opPart2 = 24 `shiftL` 1
    rcPart = enumToNum _Rc 
-- sraw
instructionToWord _ _ (Israw _Rc _RA _RS _RB) = 
  opPart .|. rSPart .|. rAPart .|. rBPart .|. opPart2 .|. rcPart
  where
    opPart = 31 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    rBPart = fromIntegral _RB `shiftL` 11
    opPart2 = 792 `shiftL` 1
    rcPart = enumToNum _Rc 
-- srw
instructionToWord _ _ (Isrw _Rc _RA _RS _RB) = 
  opPart .|. rSPart .|. rAPart .|. rBPart .|. opPart2 .|. rcPart
  where
    opPart = 31 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    rBPart = fromIntegral _RB `shiftL` 11
    opPart2 = 536 `shiftL` 1
    rcPart = enumToNum _Rc 
-- addi
instructionToWord _ _ (Iaddi _RD _RA _SIMM) =
  opPart .|. rDPart .|. rAPart .|. simmPart
  where
    opPart = 14 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    simmPart = fromIntegral _SIMM
-- addis
instructionToWord _ _ (Iaddis _RD _RA _SIMM) =
  opPart .|. rDPart .|. rAPart .|. simmPart
  where
    opPart = 15 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    simmPart = fromIntegral _SIMM
-- mulli
instructionToWord _ _ (Imulli _RD _RA _SIMM) =
  opPart .|. rDPart .|. rAPart .|. simmPart
  where
    opPart = 7 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    simmPart = fromIntegral _SIMM
-- andi
instructionToWord _ _ (Iandi _RA _RS _UIMM) =
  opPart .|. rSPart .|. rAPart .|. simmPart
  where
    opPart = 28 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    simmPart = fromIntegral _UIMM
-- ori
instructionToWord _ _ (Iori _RA _RS _UIMM) =
  opPart .|. rSPart .|. rAPart .|. simmPart
  where
    opPart = 24 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    simmPart = fromIntegral _UIMM
-- rlwinm
instructionToWord _ _ (Irlwinm _Rc _RA _RS _SH _MB _ME) =
  opPart .|. rSPart .|. rAPart .|. shPart .|. mbPart .|. mePart .|. rcPart
  where
    opPart = 21 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    shPart = fromIntegral _SH `shiftL` 11
    mbPart = fromIntegral _MB `shiftL` 6
    mePart = fromIntegral _ME `shiftL` 1
    rcPart = enumToNum _Rc
-- lbz
instructionToWord _ _ (Ilbz _RD _RA _D) =
  opPart .|. rDPart .|. rAPart .|. dPart
  where
    opPart = 34 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    dPart = fromIntegral _D
-- lhz
instructionToWord _ _ (Ilhz _RD _RA _D) =
  opPart .|. rDPart .|. rAPart .|. dPart
  where
    opPart = 40 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    dPart = fromIntegral _D
-- lha
instructionToWord _ _ (Ilha _RD _RA _D) =
  opPart .|. rDPart .|. rAPart .|. dPart
  where
    opPart = 42 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    dPart = fromIntegral _D
-- lwz
instructionToWord _ _ (Ilwz _RD _RA _D) =
  opPart .|. rDPart .|. rAPart .|. dPart
  where
    opPart = 32 `shiftL` 26
    rDPart = fromIntegral _RD `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    dPart = fromIntegral _D
-- stb
instructionToWord _ _ (Istb _RS _RA _D) =
  opPart .|. rSPart .|. rAPart .|. dPart
  where
    opPart = 38 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    dPart = fromIntegral _D
-- sth
instructionToWord _ _ (Isth _RS _RA _D) =
  opPart .|. rSPart .|. rAPart .|. dPart
  where
    opPart = 44 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    dPart = fromIntegral _D
-- stw
instructionToWord _ _ (Istw _RS _RA _D) =
  opPart .|. rSPart .|. rAPart .|. dPart
  where
    opPart = 36 `shiftL` 26
    rSPart = fromIntegral _RS `shiftL` 21
    rAPart = fromIntegral _RA `shiftL` 16
    dPart = fromIntegral _D
