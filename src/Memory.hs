module Memory where
import           Prelude                 hiding ( replicate )
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Data.Int                       ( Int8 )
import           Data.Bits
import           Data.Vector                    ( replicate
                                                , Vector
                                                , (!)
                                                , (//)
                                                )
import           Debug.Trace

data Memory = Memory (Vector Word8)
  deriving(Show)

defaultMemSize = 2 ^ 16

memoryMode :: Word32 -> Int
-- lb, sb
memoryMode 0x20 = 8
memoryMode 0x28 = 8
-- lw, sw
memoryMode 0x23 = 32
memoryMode 0x2B = 32
memoryMode _    = 0

memoryLoad :: Word32 -> Bool
-- lb
memoryLoad 0x20 = True
-- lw
memoryLoad 0x23 = True
memoryLoad _    = False

memoryStore :: Word32 -> Bool
-- sb
memoryStore 0x28 = True
-- sw
memoryStore 0x2B = True
memoryStore _    = False

isMemoryOp :: Word32 -> Bool
isMemoryOp x = memoryStore x || memoryLoad x

-- 64KiB of memory
bootMem :: Memory
bootMem = Memory (replicate defaultMemSize 0)

convertMemSize :: (Integral a) => (Integral b) => a -> b
convertMemSize = fromIntegral

-- assume little endian memory
readMem :: Memory -> Word32 -> Int -> Word32
readMem (Memory mem) addr 8 = fromIntegral signed where
  signed :: Int8
  signed = fromIntegral $ mem ! (fromIntegral addr)

readMem (Memory mem) addr 16 = (b 1) * 2 ^ 8 + (b 0) where
  b :: Word32 -> Word32
  b p = convertMemSize $ mem ! fromIntegral (addr + p)
readMem (Memory mem) addr 32 =
  (b 3) * 2 ^ 24 + (b 2) * 2 ^ 16 + (b 1) * 2 ^ 8 + (b 0) where
  b :: Word32 -> Word32
  b p = convertMemSize $ mem ! fromIntegral (addr + p)
readMem _ _ sz = error $ "mem: unsupported word size " ++ show sz

updateMem :: Memory -> Word32 -> Word32 -> Int -> Memory
updateMem (Memory mem) addr mem_data 8 =
  Memory $ mem // [(fromIntegral addr, fromIntegral mem_data)]
updateMem (Memory mem) addr mem_data 32 =
  Memory
    $  mem
    // [ (fromIntegral addr      , b 0)
       , (fromIntegral $ addr + 1, b 1)
       , (fromIntegral $ addr + 2, b 2)
       , (fromIntegral $ addr + 3, b 3)
       ] where
  b :: Int -> Word8
  b p = fromIntegral $ mem_data `shiftR` (fromIntegral p * 8)
updateMem _ _ _ sz = error $ "mem: unsupported word size " ++ show sz
