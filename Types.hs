
module Types where

import Data.Word

data Instruction =
  Ib { bLK :: Bool , bAddrLabel :: Label } -- Address is the address where you want to go to
  | Ibc { bcLK :: Bool , bcCondOp :: CondOp , bcAddrLabel :: Label }
  | Ibclr { bcLK :: Bool , bcCondOp :: CondOp }
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
  | Iextsb { extsbRc :: Bool , extsbRA :: Word8 , extsbRS :: Word8 }
  | Iextsh { extshRc :: Bool , extshRA :: Word8 , extshRS :: Word8 }
  | Iaddi { addiRD :: Word8 , addiRA :: Word8 , addiSIMM :: Word16 }
  | Iaddis { addisRD :: Word8 , addisRA :: Word8 , addisSIMM :: Word16 }
  | Imulli { mulliRD :: Word8 , mulliRA :: Word8 , mulliSIMM :: Word16 }
  | Iandi { andiRA :: Word8 , andiRS :: Word8 , andiUIMM :: Word16 }
  | Iori { oriRA :: Word8 , oriRS :: Word8 , oriUIMM :: Word16 }
  | Irlwinm { rlwinmRc :: Bool , rlwinmRA :: Word8 , rlwinmRS :: Word8 , rlwinmSH :: Word8 , rlwinmMB :: Word8 , rlwinmME :: Word8 }
  | Ilbz { lbzRD :: Word8 , lbzRA :: Word8 , lbzD :: Word16 }
  | Ilhz { lhzRD :: Word8 , lhzRA :: Word8 , lhzD :: Word16 }
  | Ilha { lhaRD :: Word8 , lhaRA :: Word8 , lhaD :: Word16 }
  | Ilwz { lwzRD :: Word8 , lwzRA :: Word8 , lwzD :: Word16 }
  | Istb { stbRS :: Word8 , stbRA :: Word8 , stbD :: Word16 }
  | Isth { sthRS :: Word8 , sthRA :: Word8 , sthD :: Word16 }
  | Istw { stwRS :: Word8 , stwRA :: Word8 , stwD :: Word16 }
  | Istwu { stwuRS :: Word8 , stwuRA :: Word8 , stwuD :: Word16 }
  | Imtspr { mtsprSPR :: SpecialRegister , mtsprRS :: Word8 }
  | Imfspr { mfsprRS :: Word8 , mfsprSPR :: SpecialRegister }
  | Iother Word32
  deriving Show


data CondOp = LessThan | GreaterThan | LessThanEqual | GreaterThanEqual | Equal | NotEqual | Always | Other Word8 Word8
  deriving Show
-- Other represents other BO, BI values for bc

data SpecialRegister = XER | LR | CTR
  deriving Show

data UnresolvedOffset = After Label | Address Word32
  deriving Eq
type ResolvedOffset = Word32

data Function = Function { getLabel :: Label , getOffset :: UnresolvedOffset , getMaxSize :: Maybe Word32 , getInstructions :: [Instruction] , getLabelTable :: LabelTable }
type FunctionTable = [Function]

-- data Function o = Function { getLabel :: Label , getOffset :: o , getEnsureOffset :: Word32 , getInstructions :: [Instruction] , getLabelTable :: LabelTable }
-- type UnresolvedFunction = Function UnresolvedOffset
-- type ResolvedFunction = Function ResolvedOffset

-- type FunctionTable o = [Function o]
-- type UnresolvedFunctionTable = FunctionTable UnresolvedOffset
-- type ResolvedFunctionTable = FunctionTable ResolvedOffset

type Label = String

type LabelTable = [(Label,Word32)]
