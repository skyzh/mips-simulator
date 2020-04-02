module Memory where
import           Prelude                 hiding ( replicate )
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Data.Vector                    ( replicate
                                                , Vector
                                                , (!)
                                                )
data Memory = Memory (Vector Word8)
  deriving(Show)

defaultMemSize = 2 ^ 16

-- 64KiB of memory
bootMem :: Memory
bootMem = Memory (replicate defaultMemSize 0)

convertMemSize :: (Integral a) => (Integral b) => a -> b
convertMemSize = fromIntegral

-- assume little endian memory
readMem :: Memory -> Word32 -> Int -> Word32
readMem (Memory mem) addr 8  = convertMemSize $ mem ! (fromIntegral addr)
readMem (Memory mem) addr 16 = (b 1) * 2 ^ 8 + (b 0) where
  b :: Word32 -> Word32
  b p = convertMemSize $ mem ! fromIntegral (addr + p)
readMem (Memory mem) addr 32 =
  (b 3) * 2 ^ 24 + (b 2) * 2 ^ 16 + (b 1) * 2 ^ 8 + (b 0) where
  b :: Word32 -> Word32
  b p = convertMemSize $ mem ! fromIntegral (addr + p)
