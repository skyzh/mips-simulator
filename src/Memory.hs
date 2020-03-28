module Memory where
import           Prelude                 hiding ( replicate )
import           Data.Word                      ( Word8 )
import           Data.Vector                    ( replicate
                                                , Vector
                                                )
data Memory = Memory (Vector Word8)
  deriving(Show)

-- 64KiB of memory
bootMem :: Memory
bootMem = Memory (replicate 65536 0)
