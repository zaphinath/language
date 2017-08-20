module Parsers
  ( Operand(..)
  , BytType(..)
  , InstructionParseResult
  , makeParserCombinator
  , intParser
  , bytParser
  , makeParsers
  , generateInstructionParser
  ) where

import Data.Char
import qualified Data.Map.Strict as SM
import Text.ParserCombinators.ReadP
import Instructions

-- single instruction operators can be compiled as labels but you get ambiguity if you try
-- to use them because the assembler doesn't know if the label is an operand or if the operator
-- is a label to the instruction.
-- ex.
-- jmp end (ambigous)
-- end trp 0

data Operand
  = Register
  | ReadOnlyRegisters
  | Label
  | IMM
  | None

data BytType
  = Empty
  | Code
  | Character

type InstructionParseResult = (String, Instruction, String, String)

toLowerStr :: String -> String
toLowerStr str = map (\c -> toLower c) str

makeParserCombinator :: [(Instruction, String, Operand, Operand)] -> ReadP InstructionParseResult
makeParserCombinator settings = choice $ makeParsers settings

makeParsers :: [(Instruction, String, Operand, Operand)] -> [ReadP InstructionParseResult]
makeParsers settings = fmap (tuncurry generateInstructionParser) settings

tuncurry :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
tuncurry f (p1, p2, p3, p4) = f p1 p2 p3 p4

isValidLabel :: String -> Bool
isValidLabel [] = False
isValidLabel str =
  (isAlpha (head str)) &&
  all (\c -> isAlphaNum c || c == '_') (tail str) &&
  not (satisfiesParser regParser str) &&
  not (satisfiesParser readOnlyParser str)

generateInstructionParser :: Instruction -> String -> Operand -> Operand -> ReadP InstructionParseResult
generateInstructionParser instruction instructionStr opType1 opType2 = do
  skipSpaces
  label <- labelParser
  skipSpaces
  parseCaseInsensitiveString instructionStr
  skipSpaces
  op1 <- selectParser opType1
  skipSpaces
  op2 <- selectParser opType2
  skipSpaces
  eof
  return (label, instruction, op1, op2)

intParser = do
  skipSpaces
  label <- labelParser
  skipSpaces
  directive <- parseCaseInsensitiveString ".int"
  skipSpaces
  value <- option "" $ do
    num <- intValueParser
    return num
  skipSpaces
  eof
  return (label, directive, value)

bytValueParser = choice
  [ do
      char '\''
      byt <- readS_to_P readLitChar
      char '\''
      return ([byt], Character)
  , do
      num <- munch1 (\c -> isDigit c)
      return (num, Code)
  , return ("", Empty)
  ]

bytParser = do
  skipSpaces
  label <- labelParser
  skipSpaces
  directive <- parseCaseInsensitiveString ".byt"
  skipSpaces
  (value, bytType) <- bytValueParser
  skipSpaces
  eof
  return (label, directive, value, bytType)

selectParser opType =
  case opType of
    Register -> regParser
    ReadOnlyRegisters -> readOnlyParser
    Label -> labelParser
    IMM -> intValueParser
    None -> return ""

parseCaseInsensitiveString str = do
  parseStr <- count (length str) get
  if (toLowerStr parseStr) == str then return str else pfail

labelParser = option "" $ do
  label <- munch (\c -> not (isSpace c))
  if isValidLabel label then return label else pfail

regParser = choice
  [ do
      reg <- char 'R'
      regNum <- satisfy (\c -> elem c "01234567")
      return (reg:regNum:[])
  , string "FP"
  , string "SP"
  ]

readOnlyParser = choice
  [ string "PC"
  , string "SB"
  , string "SL"
  ]

intValueParser = do
  sign <- option "" (string "-")
  num <- munch1 (\c -> isDigit c)
  return (sign ++ num)

satisfiesParser parser str = length (readP_to_S parser str) == 1
