module Machine
  ( Machine
  , makeMachine
  , runMachine
  ) where

import Data.Word
import Data.Array.IO
import Data.Array.MArray
import Data.Char
import Data.Array
import Utils
import InstructionProcessing
import Data.Int
import Control.Exception
import Data.Typeable.Internal

data Machine = Machine
  { memory :: IOArray Int32 Word8
  , r0 :: Int32
  , r1 :: Int32
  , r2 :: Int32
  , r3 :: Int32
  , r4 :: Int32
  , r5 :: Int32
  , r6 :: Int32
  , r7 :: Int32
  , pc :: Int32
  , hl :: Int32
  , hr :: Int32
  , sl :: Int32
  , sp :: Int32
  , fp :: Int32
  , sb :: Int32
  , tl :: Int32
  , threadQueue :: [Int32]
  }

memSize :: Int32
memSize = 100000000

threadStackSize :: Int32
threadStackSize = 1000000

heapSize :: Int32
heapSize = 1000000

makeMachine :: [Word8] -> IO Machine
makeMachine mem = do
  let stackStart = memSize - 56
      stackLimit = memSize - threadStackSize
      programSize = (fromIntegral . length) mem 
      threadLimit = (memSize - heapSize - programSize) `div` threadStackSize
  newMem <- newArray (0, memSize) 0
  writeMem newMem (zip [0..memSize] mem)
  let machine = Machine
                  { memory = newMem
                  , r0 = 0
                  , r1 = 0
                  , r2 = 0
                  , r3 = 0
                  , r4 = 0
                  , r5 = 0
                  , r6 = 0
                  , r7 = 0
                  , pc = 0
                  , hl = programSize
                  , hr = programSize
                  , sl = stackLimit
                  , sp = stackStart
                  , fp = stackStart
                  , sb = stackStart
                  , tl = threadLimit
                  , threadQueue = [0]
                  }
  initializeHeap machine
  storeContext machine
  return machine

writeMem :: IOArray Int32 Word8 -> [(Int32, Word8)] -> IO ()
writeMem m [] = return ()
writeMem m ((i, v):nm) = do
  writeArray m i v
  writeMem m nm

initializeHeap :: Machine -> IO ()
initializeHeap m = do
  let size = zip [(hl m)..((hl m) + 4)] (int32ToWord8 heapSize)
      next = zip [((hl m) + 4)..((hl m) + 8)] (int32ToWord8 0)
  writeMem (memory m) size
  writeMem (memory m) next
  return ()
  

storeContext :: Machine -> IO ()
storeContext m = do
  let current:_ = threadQueue m
      start = memSize - current * threadStackSize
      ul = getLargeUpdateList
        [ (r0 m, start - 4)
        , (r1 m, start - 8)
        , (r2 m, start - 12)
        , (r3 m, start - 16)
        , (r4 m, start - 20)
        , (r5 m, start - 24)
        , (r6 m, start - 28)
        , (r7 m, start - 32)
        , (pc m, start - 36)
        , (sb m, start - 40)
        , (fp m, start - 44)
        , (sp m, start - 48)
        , (sl m, start - 52)
        ]
  writeMem (memory m) ul
  return ()

loadContext :: Machine -> IO Machine
loadContext m = do
  let current = head (threadQueue m)
      start = memSize - current * threadStackSize
  r0 <- getInt32FromMem m (start - 4)
  r1 <- getInt32FromMem m (start - 8)
  r2 <- getInt32FromMem m (start - 12)
  r3 <- getInt32FromMem m (start - 16)
  r4 <- getInt32FromMem m (start - 20)
  r5 <- getInt32FromMem m (start - 24)
  r6 <- getInt32FromMem m (start - 28)
  r7 <- getInt32FromMem m (start - 32)
  pc <- getInt32FromMem m (start - 36)
  sb <- getInt32FromMem m (start - 40)
  fp <- getInt32FromMem m (start - 44)
  sp <- getInt32FromMem m (start - 48)
  sl <- getInt32FromMem m (start - 52)
  return m
    { r0 = r0
    , r1 = r1
    , r2 = r2
    , r3 = r3
    , r4 = r4
    , r5 = r5
    , r6 = r6
    , r7 = r7
    , pc = pc
    , sb = sb
    , fp = fp
    , sp = sp
    , sl = sl
    }

data ThreadOverflowException = TooManyThreads String deriving (Show, Typeable)
instance Exception ThreadOverflowException

defaultThreadOverflowException = TooManyThreads "Cannot allocate more threads"

newThread :: Machine -> Int32 -> Int32 -> IO Machine
newThread m reg address = do
  let findEmptySpot cur m =
        if cur > tl m
        then throw defaultThreadOverflowException
        else if elem cur (threadQueue m)
             then findEmptySpot (cur + 1) m
             else cur
      newThreadID = findEmptySpot 0 m
      base = memSize - newThreadID * threadStackSize
      stackStart = base - 56
      updatedMachine = updateRegister (m { threadQueue = (threadQueue m) ++ [newThreadID] }) reg newThreadID
      ul = getLargeUpdateList
        [ (r0 updatedMachine, base - 4)
        , (r1 updatedMachine, base - 8)
        , (r2 updatedMachine, base - 12)
        , (r3 updatedMachine, base - 16)
        , (r4 updatedMachine, base - 20)
        , (r5 updatedMachine, base - 24)
        , (r6 updatedMachine, base - 28)
        , (r7 updatedMachine, base - 32)
        , (address, base - 36)
        , (stackStart, base - 40)
        , (stackStart, base - 44)
        , (stackStart, base - 48)
        , (base - threadStackSize, base - 52)
        ]
  writeMem (memory updatedMachine) ul
  return updatedMachine

updateFirstByte :: Word8 -> Int32
updateFirstByte byte = word8ToInt32 byte 0 0 0

updateMemoryWord8 :: Machine -> Int32 -> Word8 -> IO ()
updateMemoryWord8 m address value = do
  writeMem (memory m) [(address, value)]
  return ()

getUpdateList :: (Int32, Int32) -> [(Int32, Word8)]
getUpdateList (value, address) = zip [address..(address + 4)] (int32ToWord8 value)

getLargeUpdateList :: [(Int32, Int32)] -> [(Int32, Word8)]
getLargeUpdateList l = foldr (\x xs -> x ++ xs) [] (fmap getUpdateList l)

updateMemoryInt32 :: Machine -> Int32 -> Int32 -> IO ()
updateMemoryInt32 m address value = do
  writeMem (memory m) (getUpdateList (value, address))
  return ()

getInt32FromMem :: Machine -> Int32 -> IO Int32
getInt32FromMem m addr = do
  w0 <- readArray (memory m) addr
  w1 <- readArray (memory m) (addr + 1)
  w2 <- readArray (memory m) (addr + 2)
  w3 <- readArray (memory m) (addr + 3)
  return $ word8ToInt32 w0 w1 w2 w3

getInt32FromMemAtPC :: Machine -> IO Int32
getInt32FromMemAtPC m = getInt32FromMem m (pc m)

getRegisterValue :: Machine -> Int32 -> Int32
getRegisterValue m r =
  case r of
    0 -> r0 m
    1 -> r1 m
    2 -> r2 m
    3 -> r3 m
    4 -> r4 m
    5 -> r5 m
    6 -> r6 m
    7 -> r7 m
    8 -> pc m
    9 -> sl m
    10 -> sp m
    11 -> fp m
    12 -> sb m
      
updateRegister :: Machine -> Int32 -> Int32 -> Machine
updateRegister m register newVal =
  case register of
    0 -> m { r0 = newVal }
    1 -> m { r1 = newVal }
    2 -> m { r2 = newVal }
    3 -> m { r3 = newVal }
    4 -> m { r4 = newVal }
    5 -> m { r5 = newVal }
    6 -> m { r6 = newVal }
    7 -> m { r7 = newVal }
    9 -> m { sl = newVal }
    10 -> m { sp = newVal }
    11 -> m { fp = newVal }

updateRegisterWord8 :: Machine -> Int32 -> Word8 -> Machine
updateRegisterWord8 m register byte = updateRegister m register (word8ToInt32 byte 0 0 0)

fetchInstruction :: Machine -> IO (Int32, Int32, Int32, Machine)
fetchInstruction m = do
  mWithNewContext <- loadContext m
  let programCounter = pc mWithNewContext
  operator <- getInt32FromMemAtPC mWithNewContext
  op1 <- getInt32FromMem mWithNewContext (programCounter + 4)
  op2 <- getInt32FromMem mWithNewContext (programCounter + 8)
  return (operator, op1, op2, mWithNewContext { pc = (programCounter + 12) })

rotateQueue :: Machine -> Machine
rotateQueue m = 
  let current:queue = threadQueue m
      in m { threadQueue = queue ++ [current] }

runMachine :: Machine -> IO ()
runMachine m = do
  (operator, op1, op2, advancedM) <- fetchInstruction m
  let instruction = getInstructionFromWord operator
      srr machine = do
        storeContext machine
        runMachine $ rotateQueue machine
  case instruction of
    (Just TRP) ->
      case op1 of
        0 -> return ()
        1 -> do
          putStr $ show (r3 advancedM)
          srr advancedM
        2 -> do
          line <- getLine
          let newInt = (read . head . words) line
          srr $ advancedM { r3 = newInt }
        3 -> do
          let char:_ = int32ToWord8 $ r3 advancedM
          (putChar . chr . fromIntegral) char
          srr advancedM
        4 -> do
          c <- getChar
          let newR3 = updateFirstByte ((fromIntegral . ord) c)
          srr $ advancedM { r3 = newR3 }
        _ -> putStrLn "Bad trp instruction "
    Nothing -> putStrLn $ "Invalid instruction: " ++ (show operator)
    _ -> do
      newMachine <- 
            case instruction of
              (Just JMP) -> return advancedM { pc = op1 }
              (Just JMR) -> 
                let nPC = getRegisterValue advancedM op1
                    in return advancedM { pc = nPC }
              (Just BNZ) ->
                let regValue = getRegisterValue advancedM op1 
                    in return $
                       case regValue of
                         0 -> advancedM
                         _ -> advancedM { pc = op2 }
              (Just BGT) -> 
                let regValue = getRegisterValue advancedM op1
                    in return $
                       case regValue > 0 of
                         True -> advancedM { pc = op2 }
                         False -> advancedM
              (Just BLT) ->
                let regValue = getRegisterValue advancedM op1 
                    in return $ 
                       case regValue < 0 of
                         True -> advancedM { pc = op2 }
                         False -> advancedM
              (Just BRZ) -> 
                let regValue = getRegisterValue advancedM op1 
                    in return $
                       case regValue of
                         0 -> advancedM { pc = op2 }
                         _ -> advancedM
              (Just MOV) -> 
                let regValue = getRegisterValue advancedM op2
                    in return $ updateRegister advancedM op1 regValue
              (Just LDA) -> return $ updateRegister advancedM op1 op2
              (Just STRrl) -> do
                let regValue = getRegisterValue advancedM op1
                updateMemoryInt32 advancedM op2 regValue
                return advancedM
              (Just LDRrl) -> do
                regValue <- getInt32FromMem advancedM op2
                return $ updateRegister advancedM op1 regValue
              (Just STBrl) -> do
                let regValue = getRegisterValue advancedM op1
                updateMemoryWord8 advancedM op2 ((head . int32ToWord8) regValue)
                return advancedM
              (Just LDBrl) -> do
                regValue <- getInt32FromMem advancedM op2
                return $ updateRegisterWord8 advancedM op1 ((head . int32ToWord8) regValue)
              (Just ADD) -> 
                let v1 = getRegisterValue advancedM op1
                    v2 = getRegisterValue advancedM op2
                    in return $ updateRegister advancedM op1 (v1 + v2)
              (Just ADI) ->
                let regValue = getRegisterValue advancedM op1
                    in return $ updateRegister advancedM op1 (regValue + op2)
              (Just SUB) ->
                let v1 = getRegisterValue advancedM op1
                    v2 = getRegisterValue advancedM op2
                    in return $ updateRegister advancedM op1 (v1 - v2)
              (Just MUL) ->
                let v1 = getRegisterValue advancedM op1
                    v2 = getRegisterValue advancedM op2
                    in return $ updateRegister advancedM op1 (v1 * v2)
              (Just DIV) -> 
                let v1 = getRegisterValue advancedM op1
                    v2 = getRegisterValue advancedM op2
                    in return $ updateRegister advancedM op1 (v1 `div` v2)
              (Just AND) -> 
                let v1 = getRegisterValue advancedM op1
                    v2 = getRegisterValue advancedM op2
                    in return $ updateRegister advancedM op1 $ if v1 == 1 && v2 == 1
                                                               then 1
                                                               else 0
              (Just OR) -> 
                let v1 = getRegisterValue advancedM op1
                    v2 = getRegisterValue advancedM op2
                    in return $ updateRegister advancedM op1 $ if v1 == 1 || v2 == 1
                                                               then 1
                                                               else 0
              (Just CMP) -> 
                let v1 = getRegisterValue advancedM op1
                    v2 = getRegisterValue advancedM op2
                    in return $ updateRegister advancedM op1 $ if v1 == v2
                                                               then 0
                                                               else if v1 > v2
                                                                    then 1
                                                                    else (-1)
              (Just STRrr) -> do
                let regVal1 = getRegisterValue advancedM op1
                    regVal2 = getRegisterValue advancedM op2
                updateMemoryInt32 advancedM regVal2 regVal1
                return advancedM
              (Just LDRrr) -> do
                let regVal = getRegisterValue advancedM op2
                memVal <- getInt32FromMem advancedM regVal
                return $ updateRegister advancedM op1 memVal
              (Just STBrr) -> do
                let regVal1 = getRegisterValue advancedM op1
                    regVal2 = getRegisterValue advancedM op2
                updateMemoryWord8 advancedM regVal2 ((head . int32ToWord8) regVal1)
                return advancedM
              (Just LDBrr) -> do
                let regVal = getRegisterValue advancedM op2
                memVal <- getInt32FromMem advancedM regVal
                return $ updateRegisterWord8 advancedM op1 ((head . int32ToWord8) memVal)
              (Just RUN) -> newThread advancedM op1 op2
              (Just END) -> do 
                let current:queue = threadQueue advancedM
                if current == 0
                then do
                   storeContext advancedM
                   return $ rotateQueue advancedM
                else return advancedM { threadQueue = queue }
              (Just BLK) -> 
                let current:queue = threadQueue advancedM
                    in if current == 0 && (not . null) queue
                       then return advancedM { pc = (pc advancedM) - 12 }
                       else return advancedM
              (Just LCK) -> do
                let current:_ = threadQueue advancedM
                lockVal <- getInt32FromMem advancedM op1
                if (lockVal == current) || lockVal < 0
                then do
                   updateMemoryInt32 advancedM op1 current
                   return advancedM
                else return advancedM { pc = (pc advancedM) - 12 }
              (Just ULK) -> do
                let current:_ = threadQueue advancedM
                lockVal <- getInt32FromMem advancedM op1
                if lockVal == current 
                then do
                   updateMemoryInt32 advancedM op1 (-1)
                   return advancedM
                else return advancedM
      if instruction == (Just END)
      then runMachine newMachine
      else do
        storeContext newMachine
        runMachine $ rotateQueue newMachine
