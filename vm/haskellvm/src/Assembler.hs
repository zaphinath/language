module Assembler
  ( assemble
  ) where

import Data.Int
import Data.Word
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as SM
import InstructionProcessing
import Parsers

addLabelIfNotDup :: String -> Int32 -> SM.Map String Int32 -> Maybe (SM.Map String Int32)
addLabelIfNotDup label address symbolTable =
  case SM.member label symbolTable of
    True -> Nothing
    False -> Just $ SM.insert label address symbolTable

dupLabelError :: Int32 -> String -> String
dupLabelError lineNum label = "Line " ++ (show lineNum) ++ ": Duplicate label: " ++ label

makeTable :: [(Int32, String)] -> Int32 -> Int32 -> SM.Map String Int32 -> SM.Map String Int32 -> Either String (SM.Map String Int32, Int32)
makeTable [] iOffset _ iLabelMap dLabelMap = do
  let duplicateLabels = SM.intersection iLabelMap dLabelMap
      in if SM.null duplicateLabels
         then Right (SM.union iLabelMap (SM.map (\o -> o + iOffset) dLabelMap), iOffset)
         else Left ("Duplicate labels found\n" ++ (SM.foldlWithKey (\output key _ -> output ++ key ++ "\n") "" duplicateLabels))
makeTable ((lineNum, line):asm) iOffset dOffset iLabelMap dLabelMap =
  let instruction = readP_to_S instructionParser line
      byt = readP_to_S bytParser line
      int = readP_to_S intParser line
      in case (instruction, byt, int) of
           ([((l, _, _, _), "")], [], []) ->
             if null l
             then makeTable asm (iOffset + instructionOffset) dOffset iLabelMap dLabelMap
             else case addLabelIfNotDup l iOffset iLabelMap of
                    (Just m) -> makeTable asm (iOffset + instructionOffset) dOffset m dLabelMap
                    Nothing -> Left $ dupLabelError lineNum l
           ([], [((l, _, _, _), "")], []) ->
             if null l
             then makeTable asm iOffset (dOffset + bytSize) iLabelMap dLabelMap
             else case addLabelIfNotDup l dOffset dLabelMap of
                    (Just m) -> makeTable asm iOffset (dOffset + bytSize) iLabelMap m
                    Nothing -> Left $ dupLabelError lineNum l
           ([], [], [((l, _, _), "")]) ->
             if null l
             then makeTable asm iOffset (dOffset + intSize) iLabelMap dLabelMap
             else case addLabelIfNotDup l dOffset dLabelMap of
                    (Just m) -> makeTable asm iOffset (dOffset + intSize) iLabelMap m
                    Nothing -> Left $ dupLabelError lineNum l
           _ -> Left $ "Line " ++ (show lineNum) ++ ": Invalid instruction or directive"

makeSymbolTable :: [(Int32, String)] -> Either String (SM.Map String Int32, Int32)
makeSymbolTable assembly = makeTable assembly 0 0 SM.empty SM.empty

getErrStr :: Int32 -> String -> String
getErrStr lineNum err = "line " ++ (show lineNum) ++ ": " ++ err

generate :: [(Int32, String)] -> [Word8] -> [Word8] -> Int32 -> SM.Map String Int32 -> Either String [Word8]
generate [] byteCode directives _ _ = Right $ byteCode ++ directives
generate ((lineNum, line):assembly) byteCode directives iOffset symbolTable =
  let instruction = readP_to_S instructionParser line
      byt = readP_to_S bytParser line
      int = readP_to_S intParser line
      in case (instruction, byt, int) of
           ([(parseResult, "")], [], []) ->
             case assembleInstruction iOffset symbolTable parseResult of
               (Left errStr) -> Left $ getErrStr lineNum errStr
               (Right bytes) -> generate assembly (byteCode ++ bytes) directives iOffset symbolTable
           ([], [((_, _, val, bytType), "")], []) ->
             let bits = assembleDirectiveByt val bytType
                 in generate assembly byteCode (directives ++ bits) iOffset symbolTable
           ([], [], [((_, _, val), "")]) ->
             let bits = assembleDirectiveInt val
                 in generate assembly byteCode (directives ++ bits) iOffset symbolTable
           _ -> Left $ getErrStr lineNum "Invalid instruction or directive"

generateByteCode :: [(Int32, String)] -> Int32 -> SM.Map String Int32 -> Either String [Word8]
generateByteCode assembly iOffset symbolTable = generate assembly [] [] iOffset symbolTable

groom :: Int32 -> [String] -> [(Int32, String)]
groom _ [] = []
groom index (line:rest) =
  let uncommentedLine = removeComment line
      in if isWhiteSpace uncommentedLine
         then groom (index + 1) rest
         else (index, uncommentedLine):(groom (index + 1) rest)

assemble :: [String] -> Either String [Word8]
assemble assembly =
  let groomedAsm = groom 1 assembly 
      in case makeSymbolTable groomedAsm of
           (Left err) -> Left err
           (Right (symbolTable, iOffset)) ->
             case generateByteCode groomedAsm iOffset symbolTable of
               (Left err) -> Left err
               (Right byteCode) -> Right byteCode
