
{-# LANGUAGE LambdaCase #-}

module LabelResolver (resolveLabelsInFunctionTable) where

import Control.Monad
import Control.Monad.Fix
import Data.Word
import System.Exit
import System.IO

import Types


-- This takes a function table and finds out where they should be put
-- This checks whether there isn't a loop in the graph of label referral
-- This also checks if an after-offset label points to something that exists
-- Semantic errors for after-offsets are found and handled here
resolveLabelsInFunctionTable :: FunctionTable -> IO LabelTable
resolveLabelsInFunctionTable fnTbl = do

  -- Check if two functions have same id
  flip fix (map getLabel fnTbl) $ \loop -> \case
    [] -> return ()
    (lbl:lbls) -> do
      when (elem lbl lbls) $
        die $ "Function \"" ++ lbl ++ "\" is defined more than once"
      loop lbls
      
  -- Resolution let statement
  let
    isResolvedOffset = \case
      (Function _ (Address _) _ _ _) -> True
      _ -> False
    fnWithAddrToLblData (Function lbl (Address addr) _ insts _) = ((lbl,addr),(4*) $ fromIntegral $ length insts)
    initialLabelData = map fnWithAddrToLblData $ filter isResolvedOffset fnTbl
    initialLabelTable = map fst initialLabelData
  
  -- Check if two initially resolved functions would be placed in the same position
  flip fix initialLabelTable $ \loop -> \case
    [] -> return ()
    ((lbl,offset):rest) -> do
      when (elem offset $ map snd rest) $
        die $ "Function \"" ++ lbl ++ "\" would be placed in the same place as another function"
      loop rest

  -- Resolution
  lblTbl <- flip fix (initialLabelTable,initialLabelData) $ \loop (fullLblTbl,stepLblData) -> do
    let
      nextStepForElement ((lblForEle,offForEle),szForEle) = do
        let 
          isFnThatIsAfterEle = \case
            (Function _ (After lbl) _ _ _) -> lbl == lblForEle
            _ -> False
          fnWithAfterOffsetToLblData (Function lbl (After _) _ insts _) = ((lbl,offForEle + szForEle), (4*) $ fromIntegral $ length insts)
          -- dieIfLabelWasAlreadyHandled lblData@((lbl,_),_) = case lookup lbl fullLblTbl of
            -- Just _ -> die $ "Your after-offsets are unresolvable because there is a loop in the graph of after-offsets (vertices are labels, edges are the after-offset declarations). Look at function \"" ++ lbl ++ "\""
            -- Nothing -> return $ lblData
          dieIfMoreThanOneElement ls = 
            if length ls > 1
              then die $ "There is more than one function that has as an after offset \"" ++ lblForEle ++ "\""
              else return ls
        -- (=<<) dieIfMoreThanOneElement $ mapM dieIfLabelWasAlreadyHandled $ map fnWithAfterOffsetToLblData $ filter isFnThatIsAfterEle fnTbl
        dieIfMoreThanOneElement $ map fnWithAfterOffsetToLblData $ filter isFnThatIsAfterEle fnTbl
    nextStepLblData <- fmap concat $ mapM nextStepForElement stepLblData
    if null nextStepLblData
      then return fullLblTbl
      else loop $ (,) (fmap fst nextStepLblData ++ fullLblTbl) nextStepLblData
  
  -- Check if there is an after label that doesn't exist
  let
    labelsUnresolved = flip filter fnTbl $ \(Function lbl _ _ _ _) -> not $ elem lbl $ map fst lblTbl
  when (not $ null labelsUnresolved) $ do
    hPutStrLn stderr $ "There is at least one function whose after-offset doesn\'t resolve. Here they are:"
    forM_ labelsUnresolved $ \(Function lbl (After afterLabel) _ _ _) -> 
      hPutStrLn stderr $ "  Function \"" ++ lbl ++ "\" placed after function \"" ++ afterLabel ++ "\""
    exitFailure

  -- Done! :3
  return lblTbl

    

------ TEST STUFF ------

nops = take 4 $ repeat $ Iori 0 0 0
  
ft1 = [fn1,fn2,fn3,fn4,fn5]
  where
    fn1 = Function "fn1" (Address 0xa32cc) Nothing nops []
    fn2 = Function "fn2" (After "fn1") Nothing nops []
    fn3 = Function "fn3" (After "fn2") Nothing nops []
    fn4 = Function "fn4" (Address 0xa4208) Nothing nops []
    fn5 = Function "fn5" (After "fn4") Nothing nops []
ft2 = [fnF1,fnF2]
  where
    fnF1 = Function "fnF1" (After "fnF2") Nothing nops []
    fnF2 = Function "fnF2" (After "fnF1") Nothing nops []
ft3 = fnF3:[]
  where
    fnF3 = Function "fnF3" (After "fnF3") Nothing nops []
ft4 = fn1:fn2:fn3:[]
  where
    fn1 = Function "fn1" (Address 0xa32cc) Nothing nops []
    fn2 = Function "fn2" (After "fn1") Nothing nops []
    fn3 = Function "fn3" (After "fn1") Nothing nops []
ft5 = fn1:fn2:[]
  where
    fn1 = Function "fn1" (Address 0xa32cc) Nothing nops []
    fn2 = Function "fn1" (Address 0xa4208) Nothing nops []
ft6 = fn1:fn2:[]
  where
    fn1 = Function "fn1" (After "bagel") Nothing nops []
    fn2 = Function "fn2" (After "bagel") Nothing nops []
ft7 = fn1:fn2:[]
  where
    fn1 = Function "fn1" (Address 0xa32cc) Nothing nops []
    fn2 = Function "fn2" (Address 0xa32cc) Nothing nops []
