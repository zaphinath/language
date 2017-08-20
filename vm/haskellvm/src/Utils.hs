module Utils
  ( int32ToWord8
  , word8ToInt32
  ) where

import Data.Bits
import Data.Word
import Data.Int

int32ToWord8 :: Int32 -> [Word8]
int32ToWord8 w =
  let w0 = w .&. 0xFF
      w1 = (shiftR w 8) .&. 0xFF
      w2 = (shiftR w 16) .&. 0xFF
      w3 = (shiftR w 24) .&. 0xFF      
      in map fromIntegral [w0, w1, w2, w3]

word8ToInt32 :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
word8ToInt32 w80 w81 w82 w83 =
  let w0 = shiftL (fromIntegral w83) 24
      w1 = shiftL (fromIntegral w82) 16
      w2 = shiftL (fromIntegral w81) 8
      w3 = fromIntegral w80
      in w0 .|. w1 .|. w2 .|. w3
