
{-# LANGUAGE LambdaCase #-}


module Main where

import Control.Monad
import qualified Data.ByteString.Builder as BB
import Data.List
import System.Environment
import System.Exit
import System.IO


import Helpers
import FileParser
import FileStuff
import InstructionSet
import LabelResolver
import Types

main = do
  args <- getArgs
  when (length args < 3) $
    die $ "Usage: ./PPCInject [in file] [out file] [one or more assembly files]"
  let (inFileName:outFileName:asmFiles) = args

  let 
    handleFile curFunctionTable fileName = do
      hPutStrLn stderr $ "Reading file " ++ fileName
      fnTbl <- parseFile fileName
      let fnLblsAlreadyDefined = flip filter (map getLabel fnTbl) $ flip elem $ map getLabel curFunctionTable
      when (not $ null fnLblsAlreadyDefined) $ do
        hPutStrLn stderr $ "These functions have already been defined in another file:"
        forM_ fnLblsAlreadyDefined $ putStrLn . ("  "++)
        exitFailure
      return $ fnTbl ++ curFunctionTable
  -- Read all files and get all functions
  functionTable <- foldM handleFile [] asmFiles
  -- Resolve after-offsets
  globalLabelTable <- resolveLabelsInFunctionTable functionTable
  -- Make the local labels absolute
  fixedFunctionTable <- forM functionTable $ \fn@(Function lbl _ _ _ lblTbl) -> do
    case lookup lbl globalLabelTable of
      Nothing -> die $ "The offset for function \"" ++ lbl ++ "\" has not been resolved"
      Just off -> do
        let
          absLblTbl = flip map lblTbl $ fmap $ (+) off
        return $ fn { getLabelTable = absLblTbl }
  let 
    sortedLabelTable = sortBy (\(_,off1) (_,off2) -> compare off1 off2) globalLabelTable
    pairedFunctionTable = zip (map getLabel fixedFunctionTable) fixedFunctionTable
    finalFunctionTable = flip map sortedLabelTable $ \(lbl,off) -> 
      let (Function _ _ _ insts lblTbl) = brutalLookup lbl pairedFunctionTable
      in (lbl,off,insts,lblTbl)
    -- Each entry in this table pairs the end offset of a function to the beginning of the next, along with the respective labels
    endBeginTable = zip 
      (map (\(lbl,off,insts,_) -> (,) lbl $ (+) off $ (4*) $ fromIntegral $ length insts) finalFunctionTable) 
      (map (\(lbl,off,_,_) -> (lbl,off)) $ tail finalFunctionTable)
  
  -- Make sure that any two functions don't overlap
  forM_ endBeginTable $ \((lbl1,endOfFn1),(lbl2,beginOfFn2)) ->
    when (endOfFn1 > beginOfFn2) $
      die $ "Functions \"" ++ lbl1 ++ "\" and \"" ++ lbl2 ++ "\" overlap"

  -- Get machine code from instructions
  machineCodeTable <- 
    flip mapM finalFunctionTable $ \(lbl,off,insts,localLabelTable) -> do
      let thisLabelTable = localLabelTable ++ globalLabelTable
      fmap ((,) off) $ forM (zip insts $ map ((+off).(4*)) [0..]) $ 
        \(inst,pc) -> instructionToWord thisLabelTable pc inst
      

  doFileIO inFileName outFileName $ \inFile outFile -> do
    let cpBytes = copyBytes inFile outFile
        cpUntilEnd = copyUntilEnd inFile outFile
    case finalFunctionTable of
      [] -> do
        hPutStrLn stderr $ "No functions to write. Copying input file..." 
        cpUntilEnd
      _ -> do
        let (fstOff,fstMc) = head machineCodeTable
        cpBytes 1 fstOff
        let
          writeInstruction instMc = do
            BB.hPutBuilder outFile $ BB.word32BE instMc
            hSeek inFile RelativeSeek 4
          writeFunction = mapM_ writeInstruction 
        writeFunction fstMc
        forM_ (tail machineCodeTable) $ \(off,mc) -> do
          curPos <- fmap fromIntegral $ hTell inFile
          cpBytes curPos (off-1)
          writeFunction mc
        curPos <- fmap fromIntegral $ hTell inFile
        inFileSize <- fmap fromIntegral $ hFileSize inFile
        when (curPos < inFileSize) $ cpUntilEnd

          


