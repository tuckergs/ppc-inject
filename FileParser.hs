
module FileParser (parseFile) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Word
import System.Exit
import System.IO

import HexStuff
import InstructionParser
import ParseHelpers
import ParseMonad
import Types


------ TODO: Make ParseFileRecord keep track of line number


data ParseFileRecord = PFR { getLineNum :: Int , 
                              getLines :: [String] , 
                              getFunctionTable :: FunctionTable , 
                              getFunctionMaybe :: Maybe Function , 
                              getOffsetTable :: LabelTable 
                            }

data ParserLine = BeginFunctionLine (Label,UnresolvedOffset,(Maybe Word32)) 
                  | LabelLine String 
                  | InstructionLine Instruction
                  | OffsetLine String Word32
                  | Comment

-- Note that the local labels will have a offset relative to the beginning of its function
-- This will be fixed on write-out
parseFile :: String -> IO (FunctionTable,LabelTable)
parseFile inFileName = do
  inFile <- openFile inFileName ReadMode
  allLns <- fmap lines $ hGetContents inFile
  let initRecord = PFR { getLineNum = 1 , getLines = allLns , getFunctionTable = [] , getFunctionMaybe = Nothing , getOffsetTable = [] }
  pair <- 
    flip fix initRecord $ 
      \loop curRecord -> do
        let
          curLineNum = getLineNum curRecord
          curLines = getLines curRecord
          curFunctionTable = getFunctionTable curRecord
          curFunctionMaybe = getFunctionMaybe curRecord
          curOffsetTable = getOffsetTable curRecord
          updateFunctionTable fn@(Function lbl _ maxSizeMaybe insts _) = do
            case maxSizeMaybe of
              Nothing -> return ()
              Just maxSize -> do
                when ((> maxSize) $ (4*) $ fromIntegral $ length insts) $ 
                  die $ "The size of function \"" ++ lbl ++ "\" exceeds the max size that you specified, 0x" ++ (show $ Hex maxSize) ++ " bytes"
            return $ (fn { getInstructions = reverse $ getInstructions fn }):curFunctionTable
          startFunction (lbl,off,maxSizeMaybe) = do
            when (not $ null $ flip filter curFunctionTable $ (== lbl) . getLabel) $
              die $ "Function \"" ++ lbl ++ "\" has already been defined"
            return $ Function lbl off maxSizeMaybe [] []
        case curLines of
          [] -> 
            case curFunctionMaybe of
              Nothing -> return (curFunctionTable,curOffsetTable)
              Just fn -> do
                theFunctionTable <- updateFunctionTable fn
                return (theFunctionTable,curOffsetTable)
          (ln:lns) -> do
            let 
              nextRecord = curRecord { getLineNum = curLineNum + 1 , getLines = lns }
              parsedLine = parse lineParser ln 
            case (curFunctionMaybe,parsedLine) of
              (_,Left _) -> die $ "In file " ++ inFileName ++ ", line " ++ (show curLineNum) ++ ", this line couldn\'t be parsed"
              (_,Right Comment) -> loop nextRecord
              (Nothing,Right (BeginFunctionLine fnData)) -> do
                fn <- startFunction fnData
                loop $ nextRecord { getFunctionMaybe = Just fn }
              (Nothing,_) ->
                die $ "You haven\'t started to define a snippet of code. Add a code declaration line"
              (Just curFn,Right (BeginFunctionLine newFnData)) -> do
                newFunctionTable <- updateFunctionTable curFn
                newFn <- startFunction newFnData
                loop $ nextRecord { getFunctionTable = newFunctionTable , getFunctionMaybe = Just newFn }
              (Just curFn,Right (LabelLine lbl)) -> do
                let
                  curLblTbl = getLabelTable curFn
                  curOffsetInFn = (4*) $ fromIntegral $ length $ getInstructions curFn
                case lookup lbl curLblTbl of
                  Nothing -> return ()
                  Just _ -> die $ "You defined label \"" ++ lbl ++ "\" twice"
                loop $ nextRecord { getFunctionMaybe = Just $ curFn { getLabelTable = (lbl,curOffsetInFn):curLblTbl } }
              (Just curFn, Right (OffsetLine lbl off)) -> do
                case lookup lbl curOffsetTable of
                  Nothing -> return ()
                  Just _ -> die $ "You defined offset \"" ++ lbl ++ "\" twice"
                loop $ nextRecord { getOffsetTable = (lbl,off):curOffsetTable }
              (Just curFn,Right (InstructionLine inst)) -> do
                let curInsts = getInstructions curFn
                loop $ nextRecord { getFunctionMaybe = Just $ curFn { getInstructions = inst:curInsts } }
  hClose inFile
  return pair


lineParser :: Parse String ParserLine
lineParser = comment <|> beginFunctionParser <|> localLabelLineParser <|> offsetParser <|> instructionParser

comment = do
  ws 
  return () <|> (token '%' >> list item >> return ()) 
  return Comment

beginFunctionParser = do
  ws
  tokens "#function"
  ws1
  lbl <- functionLabelParser
  ws1
  off <- 
    (numberParser >>= (return . Address)) 
    <|> (tokens "after" >> ws1 >> fmap After functionLabelParser)
  maxSizeMaybe <- 
    return Nothing <|> (ws1 >> tokens "maxSize" >> ws1 >> fmap Just numberParser) 
  comment
  return $ BeginFunctionLine (lbl,off,maxSizeMaybe)

localLabelLineParser = ws >> fmap LabelLine localLabelParser

offsetParser = do
  ws
  tokens "#offset"
  ws1
  lbl <- offsetLabelParser
  ws1
  off <- numberParser
  comment
  return $ OffsetLine lbl off

instructionParser = do
  ws
  inst <- realInstructionParser -- Defined in ParseInstruction.hs
  comment
  return $ InstructionLine inst

