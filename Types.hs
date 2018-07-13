
module Types where

import Data.Word

data Instruction =
  Ib { bLK :: Bool , bAddrLabel :: Label } -- Address is the address where you want to go to
  | Ibc { bcLK :: Bool , bcCondOp :: CondOp , bcAddrLabel :: Label }
  | Icmpz { cmpzUnsigned :: Bool , cmpzCrfD :: Word8 , cmpzL :: Bool , cmpzRA :: Word8 , cmpzRB :: Word8 } -- cmp / cmpl
  | Icmpzi { cmpziUnsigned :: Bool , cmpziCrfD :: Word8 , cmpziL :: Bool , cmpziRA :: Word8 , cmpziSIMM :: Word16 } -- cmpl / cmpli
  | Iadd { addOE :: Bool , addRc :: Bool , addRD :: Word8 , addRA :: Word8 , addRB :: Word8 }
  | Imullw { mullwOE :: Bool , mullwRc :: Bool , mullwRD :: Word8 , mullwRA :: Word8 , mullwRB :: Word8 }
  | Isubf { subfOE :: Bool , subfRc :: Bool , subfRD :: Word8 , subfRA :: Word8 , subfRB :: Word8 }
  | Iand { andRc :: Bool , andRA :: Word8 , andRS :: Word8 , andRB :: Word8 }
  | Ior { orRc :: Bool , orRA :: Word8 , orRS :: Word8 , orRB :: Word8 }
  | Islw { slwRc :: Bool , slwRA :: Word8 , slwRS :: Word8 , slwRB :: Word8 }
  | Israw { srawRc :: Bool , srawRA :: Word8 , srawRS :: Word8 , srawRB :: Word8 }
  | Isrw { srwRc :: Bool , srwRA :: Word8 , srwRS :: Word8 , srwRB :: Word8 }
  | Iaddi { addiRD :: Word8 , addiRA :: Word8 , addiSIMM :: Word16 }
  | Iaddis { addisRD :: Word8 , addisRA :: Word8 , addisSIMM :: Word16 }
  | Imulli { mulliRD :: Word8 , mulliRA :: Word8 , mulliSIMM :: Word16 }
  | Iandi { andiRA :: Word8 , andiRS :: Word8 , andiUIMM :: Word16 }
  | Iori { oriRA :: Word8 , oriRS :: Word8 , oriUIMM :: Word16 }
  | Irlwinm { rlwinmRc :: Bool , rlwinmRA :: Word8 , rlwinmRS :: Word8 , rlwinmSH :: Word8 , rlwinmMB :: Word8 , rlwinmME :: Word8 }
  | Ilbz { lbzRD :: Word8 , lbzRA :: Word8 , lbzD :: Word8 }
  | Ilhz { lhzRD :: Word8 , lhzRA :: Word8 , lhzD :: Word8 }
  | Ilha { lhaRD :: Word8 , lhaRA :: Word8 , lhaD :: Word8 }
  | Ilwz { lwzRD :: Word8 , lwzRA :: Word8 , lwzD :: Word8 }
  | Istb { stbRS :: Word8 , stbRA :: Word8 , stbD :: Word8 }
  | Isth { sthRS :: Word8 , sthRA :: Word8 , sthD :: Word8 }
  | Istw { stwRS :: Word8 , stwRA :: Word8 , stwD :: Word8 }


data CondOp = LessThan | GreaterThan | LessThanEqual | GreaterThanEqual | Equal | NotEqual | Other Word8 Word8
-- Other represents other BO, BI values for bc

data UnresolvedOffset = AfterFunction Label | Address Word32
  deriving Eq

data Function = NoFunction | Function [Instruction] LabelTable

type UnresolvedFunctionTable = [(UnresolvedOffset,Function)]

type ResolvedFunctionTable = [(Word32,Function)]

type Label = String

type LabelTable = [(Label,Word32)]
