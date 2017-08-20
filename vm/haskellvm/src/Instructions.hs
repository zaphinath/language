module Instructions
  ( Instruction(..)
  , Directive(..)
  , getInstructionFromWord
  , getInstructionValue
  ) where

import Data.Int 

data Instruction
  = JMP
  | JMR
  | BNZ
  | BGT
  | BLT
  | BRZ
  | MOV
  | LDA
  | STRrl
  | LDRrl
  | STBrl
  | LDBrl
  | ADD
  | ADI
  | SUB
  | MUL
  | DIV
  | AND
  | OR
  | CMP
  | TRP
  | STRrr
  | LDRrr
  | STBrr
  | LDBrr
  | RUN
  | END
  | BLK
  | LCK
  | ULK
  deriving (Show, Eq)

data Directive
  = INT
  | BYT

getInstructionValue :: Instruction -> Int32
getInstructionValue instruction =
  case instruction of
    JMP -> 1
    JMR -> 2
    BNZ -> 3
    BGT -> 4
    BLT -> 5
    BRZ -> 6
    MOV -> 7
    LDA -> 8
    STRrl -> 9
    LDRrl -> 10
    STBrl -> 11
    LDBrl -> 12
    ADD -> 13
    ADI -> 14
    SUB -> 15
    MUL -> 16
    DIV -> 17
    AND -> 18
    OR -> 19
    CMP -> 20
    TRP -> 21
    STRrr -> 22
    LDRrr -> 23
    STBrr -> 24
    LDBrr -> 25
    RUN -> 26
    END -> 27
    BLK -> 28
    LCK -> 29
    ULK -> 30

getInstructionFromWord :: Int32 -> Maybe Instruction
getInstructionFromWord word =
  case word of
    1 -> Just JMP
    2 -> Just JMR
    3 -> Just BNZ
    4 -> Just BGT
    5 -> Just BLT
    6 -> Just BRZ
    7 -> Just MOV
    8 -> Just LDA
    9 -> Just STRrl
    10 -> Just LDRrl
    11 -> Just STBrl
    12 -> Just LDBrl
    13 -> Just ADD
    14 -> Just ADI
    15 -> Just SUB
    16 -> Just MUL
    17 -> Just DIV
    18 -> Just AND
    19 -> Just OR
    20 -> Just CMP
    21 -> Just TRP
    22 -> Just STRrr
    23 -> Just LDRrr
    24 -> Just STBrr
    25 -> Just LDBrr
    26 -> Just RUN
    27 -> Just END
    28 -> Just BLK
    29 -> Just LCK
    30 -> Just ULK
    _ -> Nothing
