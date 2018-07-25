
module InstructionParser (realInstructionParser) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word

import ParseHelpers
import ParseMonad
import Types

commaParser = ws >> token ',' >> ws >> return ()

crfDWithCommaParser = do
  tokens "cr" 
  n <- decNumberParser 
  commaParser
  return n

rWithoutCommaParser = token 'r' >> decNumberParser

rWithCommaParser = do
  token 'r'
  n <- decNumberParser
  commaParser
  return n

loadStoreArgParser :: Parse String (Word8,Word8,Word16)
loadStoreArgParser = do
  rX <- rWithoutCommaParser
  commaParser
  d <- signedNumberParser
  ws
  token '('
  ws
  rA <- rWithoutCommaParser
  ws
  token ')'
  isRegister rX
  isRegister rA
  return (rX,rA,d)

isRegister :: Word8 -> Parse String ()
isRegister rP = when ((/= 0) $ rP .&. 0xE0) $ none

isCrf :: Word8 -> Parse String ()
isCrf crfP = when ((/= 0) $ crfP .&. 0xFC) $ none


-- This is where the reading of the assembly instructions is done
-- Common mnemonics are handled here
realInstructionParser :: Parse String Instruction
realInstructionParser =
  -- b
  do 
    token 'b'
    ws1
    lbl <- anyLabelParser
    return $ Ib False lbl
  -- bl
  <|> do
    tokens "bl"
    ws1
    lbl <- anyLabelParser
    return $ Ib True lbl
  -- blr
  <|> do
    tokens "blr"
    return $ Ibclr False Always
  -- beq
  <|> do
    tokens "beq"
    ws1
    lbl <- anyLabelParser
    return $ Ibc False Equal lbl
  -- bne
  <|> do
    tokens "bne"
    ws1
    lbl <- anyLabelParser
    return $ Ibc False NotEqual lbl
  -- blt
  <|> do
    tokens "blt"
    ws1
    lbl <- anyLabelParser
    return $ Ibc False LessThan lbl
  -- ble
  <|> do
    tokens "ble"
    ws1
    lbl <- anyLabelParser
    return $ Ibc False LessThanEqual lbl
  -- bgt
  <|> do
    tokens "bgt"
    ws1
    lbl <- anyLabelParser
    return $ Ibc False GreaterThan lbl
  -- bge
  <|> do
    tokens "bge"
    ws1
    lbl <- anyLabelParser
    return $ Ibc False GreaterThanEqual lbl
  -- cmpw
  <|> do
    tokens "cmpw"
    ws1
    crfD <- return 0 <|> crfDWithCommaParser
    rA <- rWithCommaParser
    rB <- rWithoutCommaParser
    isCrf crfD
    isRegister rA
    isRegister rB
    return $ Icmpz False crfD False rA rB
  -- cmplw
  <|> do
    tokens "cmplw"
    ws1
    crfD <- return 0 <|> crfDWithCommaParser
    rA <- rWithCommaParser
    rB <- rWithoutCommaParser
    isCrf crfD
    isRegister rA
    isRegister rB
    return $ Icmpz True crfD False rA rB
  -- cmpwi
  <|> do
    tokens "cmpwi"
    ws1
    crfD <- return 0 <|> crfDWithCommaParser
    rA <- rWithCommaParser
    simm <- signedNumberParser
    isCrf crfD
    isRegister rA
    return $ Icmpzi False crfD False rA simm
  -- cmplwi
  <|> do
    tokens "cmplwi"
    ws1
    crfD <- return 0 <|> crfDWithCommaParser
    rA <- rWithCommaParser
    simm <- signedNumberParser
    isCrf crfD
    isRegister rA
    return $ Icmpzi True crfD False rA simm
  -- add
  <|> do
    tokens "add"
    ws1
    rD <- rWithCommaParser
    rA <- rWithCommaParser
    rB <- rWithoutCommaParser
    isRegister rD
    isRegister rA
    isRegister rB
    return $ Iadd False False rD rA rB
  -- mullw
  <|> do
    tokens "mullw"
    ws1
    rD <- rWithCommaParser
    rA <- rWithCommaParser
    rB <- rWithoutCommaParser
    isRegister rD
    isRegister rA
    isRegister rB
    return $ Imullw False False rD rA rB
  -- subf
  <|> do
    tokens "subf"
    ws1
    rD <- rWithCommaParser
    rA <- rWithCommaParser
    rB <- rWithoutCommaParser
    isRegister rD
    isRegister rA
    isRegister rB
    return $ Isubf False False rD rA rB
  -- sub
  <|> do
    tokens "sub"
    ws1
    rD <- rWithCommaParser
    rA <- rWithCommaParser
    rB <- rWithoutCommaParser
    isRegister rD
    isRegister rA
    isRegister rB
    return $ Isubf False False rD rB rA
  -- and
  <|> do
    tokens "and"
    ws1
    rA <- rWithCommaParser
    rS <- rWithCommaParser
    rB <- rWithoutCommaParser
    isRegister rA
    isRegister rS
    isRegister rB
    return $ Iand False rA rS rB
  -- or
  <|> do
    tokens "or"
    ws1
    rA <- rWithCommaParser
    rS <- rWithCommaParser
    rB <- rWithoutCommaParser
    isRegister rA
    isRegister rS
    isRegister rB
    return $ Ior False rA rS rB
  -- mr 
  <|> do
    tokens "mr"
    ws1
    rX <- rWithCommaParser
    rY <- rWithoutCommaParser
    isRegister rX
    isRegister rY
    return $ Ior False rX rY rY
  -- slw
  <|> do
    tokens "slw"
    ws1
    rA <- rWithCommaParser
    rS <- rWithCommaParser
    rB <- rWithoutCommaParser
    isRegister rA
    isRegister rS
    isRegister rB
    return $ Islw False rA rS rB
  -- sraw
  <|> do
    tokens "sraw"
    ws1
    rA <- rWithCommaParser
    rS <- rWithCommaParser
    rB <- rWithoutCommaParser
    isRegister rA
    isRegister rS
    isRegister rB
    return $ Israw False rA rS rB
  -- srw
  <|> do
    tokens "srw"
    ws1
    rA <- rWithCommaParser
    rS <- rWithCommaParser
    rB <- rWithoutCommaParser
    isRegister rA
    isRegister rS
    isRegister rB
    return $ Isrw False rA rS rB
  -- addi
  <|> do
    tokens "addi"
    ws1
    rD <- rWithCommaParser
    rA <- rWithCommaParser
    simm <- signedNumberParser
    isRegister rD
    isRegister rA
    return $ Iaddi rD rA simm
  -- addis
  <|> do
    tokens "addis"
    ws1
    rD <- rWithCommaParser
    rA <- rWithCommaParser
    simm <- signedNumberParser
    isRegister rD
    isRegister rA
    return $ Iaddis rD rA simm
  -- li
  <|> do
    tokens "li"
    ws1
    rX <- rWithCommaParser
    simm <- signedNumberParser
    isRegister rX
    return $ Iaddi rX 0 simm
  -- lis
  <|> do
    tokens "lis"
    ws1
    rX <- rWithCommaParser
    simm <- signedNumberParser
    isRegister rX
    return $ Iaddis rX 0 simm
  -- subi
  <|> do
    tokens "subi"
    ws1
    rD <- rWithCommaParser
    rA <- rWithCommaParser
    simm <- signedNumberParser
    isRegister rD
    isRegister rA
    return $ Iaddi rD rA $ negate simm
  -- subis
  <|> do
    tokens "subis"
    ws1
    rD <- rWithCommaParser
    rA <- rWithCommaParser
    simm <- signedNumberParser
    isRegister rD
    isRegister rA
    return $ Iaddis rD rA $ negate simm
  -- mulli
  <|> do
    tokens "mulli"
    ws1
    rD <- rWithCommaParser
    rA <- rWithCommaParser
    simm <- signedNumberParser
    isRegister rD
    isRegister rA
    return $ Imulli rD rA simm
  -- andi
  <|> do
    tokens "andi" <|> tokens "andi."
    ws1
    rA <- rWithCommaParser
    rS <- rWithCommaParser
    uimm <- signedNumberParser
    isRegister rA
    isRegister rS
    return $ Iandi rA rS uimm
  -- ori
  <|> do
    tokens "ori"
    ws1
    rA <- rWithCommaParser
    rS <- rWithCommaParser
    uimm <- signedNumberParser
    isRegister rA
    isRegister rS
    return $ Iori rA rS uimm
  -- nop
  <|> do
    tokens "nop"
    return $ Iori 0 0 0
  -- rlwinm
  <|> do
    tokens "rlwinm"
    ws1
    rA <- rWithCommaParser
    rS <- rWithCommaParser
    sh <- numberParser
    commaParser
    mb <- numberParser
    commaParser
    me <- numberParser
    let malformed = (/= 5) $ length $ flip filter [rA,rS,sh,mb,me] $ flip elem [0..31] 
    when malformed $ none
    return $ Irlwinm False rA rS sh mb me
  -- lbz
  <|> do
    tokens "lbz"
    ws1
    (rD,rA,d) <- loadStoreArgParser
    return $ Ilbz rD rA d
  -- lhz
  <|> do
    tokens "lhz"
    ws1
    (rD,rA,d) <- loadStoreArgParser
    return $ Ilhz rD rA d
  -- lha
  <|> do
    tokens "lha"
    ws1
    (rD,rA,d) <- loadStoreArgParser
    return $ Ilha rD rA d
  -- lwz
  <|> do
    tokens "lwz"
    ws1
    (rD,rA,d) <- loadStoreArgParser
    return $ Ilwz rD rA d
  -- stb
  <|> do
    tokens "stb"
    ws1
    (rS,rA,d) <- loadStoreArgParser
    return $ Istb rS rA d
  -- sth
  <|> do
    tokens "sth"
    ws1
    (rS,rA,d) <- loadStoreArgParser
    return $ Isth rS rA d
  -- stw
  <|> do
    tokens "stw"
    ws1
    (rS,rA,d) <- loadStoreArgParser
    return $ Istw rS rA d
  -- other
  <|> do
    inst <- numberParser
    return $ Iother inst
