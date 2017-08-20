module InstructionProcessing
  ( Instruction(..)
  , Directive(..)
  , getInstructionFromWord
  , removeComment
  , intSize
  , bytSize
  , assembleDirectiveByt
  , assembleDirectiveInt
  , assembleInstruction
  , instructionOffset
  , isWhiteSpace
  , getInstructionValue
  , instructionParser
  ) where

import Data.Word
import Data.Int
import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as SM
import Text.Read
import Utils
import Parsers
import Instructions

instructionParser = makeParserCombinator
  [ (JMP, "jmp", Label, None)
  , (JMR, "jmr", Register, None)
  , (BNZ, "bnz", Register, Label)
  , (BGT, "bgt", Register, Label)
  , (BLT, "blt", Register, Label)
  , (BRZ, "brz", Register, Label)
  , (MOV, "mov", Register, Register)
  , (MOV, "mov", Register, ReadOnlyRegisters)
  , (LDA, "lda", Register, Label)
  , (STRrl, "str", Register, Label)
  , (LDRrl, "ldr", Register, Label)
  , (STBrl, "stb", Register, Label)
  , (LDBrl, "ldb", Register, Label)
  , (ADD, "add", Register, Register)
  , (ADI, "adi", Register, IMM)
  , (SUB, "sub", Register, Register)
  , (MUL, "mul", Register, Register)
  , (DIV, "div", Register, Register)
  , (AND, "and", Register, Register)
  , (OR, "or", Register, Register)
  , (CMP, "cmp", Register, Register)
  , (TRP, "trp", IMM, None)
  , (STRrr, "str", Register, Register)
  , (LDRrr, "ldr", Register, Register)
  , (STBrr, "stb", Register, Register)
  , (LDBrr, "ldb", Register, Register)
  , (RUN, "run", Register, Label)
  , (END, "end", None, None)
  , (BLK, "blk", None, None)
  , (LCK, "lck", Label, None)
  , (ULK, "ulk", Label, None)
  ]

isWhiteSpace :: String -> Bool
isWhiteSpace str = all (\c -> isSpace c) str

instructionOffset :: Int32
instructionOffset = 12

labelDoesNotExist :: String -> String
labelDoesNotExist label = "Label " ++ label ++ " does not exist"

invalidRegister :: String -> String
invalidRegister register = "Invalid Register " ++ register

getInstructionLabelOp :: Instruction -> SM.Map String Int32 -> String -> Either String [Word8]
getInstructionLabelOp instruction symbolTable label =
  case SM.lookup label symbolTable of
    (Just labelAddress) -> Right $ (get4ByteValue instruction) ++
                                   (int32ToWord8 labelAddress) ++
                                   emptyInt32
    Nothing -> Left $ labelDoesNotExist label

getInstructionRegisterOp :: Instruction -> String -> [Word8]
getInstructionRegisterOp instruction registerStr =
  (get4ByteValue instruction) ++
  (getRegisterValue registerStr) ++
  emptyInt32

getInstructionRegisterLabelOp :: Instruction -> SM.Map String Int32 -> String -> String -> Either String [Word8]
getInstructionRegisterLabelOp instruction symbolTable registerStr label =
  case SM.lookup label symbolTable of
    (Just labelAddress) ->  Right $ (get4ByteValue instruction) ++
                                    (getRegisterValue registerStr) ++
                                    (int32ToWord8 labelAddress)
    Nothing -> Left $ labelDoesNotExist label

getInstructionRegisterRegisterOp :: Instruction -> String -> String -> [Word8]
getInstructionRegisterRegisterOp instruction registerStr1 registerStr2 =
  (get4ByteValue instruction) ++
  (getRegisterValue registerStr1) ++
  (getRegisterValue registerStr2)

assembleInstruction :: Int32 -> SM.Map String Int32 -> InstructionParseResult -> Either String [Word8]
assembleInstruction iOffset symbolTable (_, instruction, op1, op2) =
  case (instruction, op1, op2) of
    (JMP, l, _) -> getInstructionLabelOp JMP symbolTable l
    (JMR, r, _) -> Right $ getInstructionRegisterOp JMR r
    (BNZ, r, l) -> getInstructionRegisterLabelOp BNZ symbolTable r l 
    (BGT, r, l) -> getInstructionRegisterLabelOp BGT symbolTable r l
    (BLT, r, l) -> getInstructionRegisterLabelOp BLT symbolTable r l
    (BRZ, r, l) -> getInstructionRegisterLabelOp BRZ symbolTable r l
    (MOV, r1, r2) -> Right $ getInstructionRegisterRegisterOp MOV r1 r2
    (LDA, r, l) ->
      case SM.lookup l symbolTable of
        (Just labelAddress) -> if labelAddress >= iOffset
                               then getInstructionRegisterLabelOp LDA symbolTable r l
                               else Left $ l ++ " is an instruction address, it must be a directive"
        Nothing -> Left $ labelDoesNotExist l
    (STRrl, r, l) -> getInstructionRegisterLabelOp STRrl symbolTable r l
    (LDRrl, r, l) -> getInstructionRegisterLabelOp LDRrl symbolTable r l
    (STBrl, r, l) -> getInstructionRegisterLabelOp STBrl symbolTable r l
    (LDBrl, r, l) -> getInstructionRegisterLabelOp LDBrl symbolTable r l
    (ADD, r1, r2) -> Right $ getInstructionRegisterRegisterOp ADD r1 r2
    (ADI, r, i) -> Right $ (get4ByteValue ADI) ++
                             (getRegisterValue r) ++
                             (int32ToWord8 (read i))
    (SUB, r1, r2) -> Right $ getInstructionRegisterRegisterOp SUB r1 r2
    (MUL, r1, r2) -> Right $ getInstructionRegisterRegisterOp MUL r1 r2
    (DIV, r1, r2) -> Right $ getInstructionRegisterRegisterOp DIV r1 r2
    (AND, r1, r2) -> Right $ getInstructionRegisterRegisterOp AND r1 r2
    (OR, r1, r2) -> Right $ getInstructionRegisterRegisterOp OR r1 r2 
    (CMP, r1, r2) -> Right $ getInstructionRegisterRegisterOp CMP r1 r2
    (TRP, trpCode, _) -> Right $ (get4ByteValue TRP) ++
                                   (int32ToWord8 (read trpCode)) ++
                                   emptyInt32
    (STRrr, r1, r2) -> Right $ getInstructionRegisterRegisterOp STRrr r1 r2
    (LDRrr, r1, r2) -> Right $ getInstructionRegisterRegisterOp LDRrr r1 r2
    (STBrr, r1, r2) -> Right $ getInstructionRegisterRegisterOp STBrr r1 r2
    (LDBrr, r1, r2) -> Right $ getInstructionRegisterRegisterOp LDBrr r1 r2
    (RUN, r, l) -> getInstructionRegisterLabelOp RUN symbolTable r l
    (END, _, _) -> Right $ (get4ByteValue END) ++ emptyInt32 ++ emptyInt32
    (BLK, _, _) -> Right $ (get4ByteValue BLK) ++ emptyInt32 ++ emptyInt32
    (LCK, l, _) -> getInstructionLabelOp LCK symbolTable l
    (ULK, l, _) -> getInstructionLabelOp ULK symbolTable l

getZeroDirective :: Directive -> [Word8]
getZeroDirective INT = emptyInt32
getZeroDirective BYT = [0]

emptyInt32 :: [Word8]
emptyInt32 = [0, 0, 0, 0]

assembleDirectiveByt :: String -> BytType -> [Word8]
assembleDirectiveByt bytCode Code =
  let word = (head . int32ToWord8 . fromIntegral . ord . read) bytCode
      in [word]
assembleDirectiveByt (bytChar:[]) Character =
  let char = (head . int32ToWord8 . fromIntegral . ord) bytChar
      in [char]
assembleDirectiveByt _ Empty = getZeroDirective BYT

assembleDirectiveInt :: String -> [Word8]
assembleDirectiveInt [] = getZeroDirective INT
assembleDirectiveInt value = int32ToWord8 $ read value

getRegisterValue :: String -> [Word8]
getRegisterValue registerStr =
  int32ToWord8 $ case registerStr of
    "R0" -> 0
    "R1" -> 1
    "R2" -> 2
    "R3" -> 3
    "R4" -> 4
    "R5" -> 5
    "R6" -> 6
    "R7" -> 7
    "PC" -> 8
    "SL" -> 9
    "SP" -> 10
    "FP" -> 11
    "SB" -> 12

removeComment :: String -> String
removeComment instruction = head $ splitOn "--" instruction

isValidLabel :: String -> Bool
isValidLabel (f:rest) = (isAlpha f) && all (\c -> isAlphaNum c || c == '_') rest
isValidLabel _ = False

bytSize :: Int32
bytSize = 1

intSize :: Int32
intSize = 4

get4ByteValue :: Instruction -> [Word8]
get4ByteValue instruction = (int32ToWord8 . getInstructionValue) instruction
