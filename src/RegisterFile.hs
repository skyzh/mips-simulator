module RegisterFile where
import           Prelude                 hiding ( replicate )
import           Data.Word                      ( Word32 )
import           Data.Vector
data RegisterFile = RegisterFile (Vector Word32)
  deriving(Show)

-- 32 registers
bootRF :: RegisterFile
bootRF = RegisterFile (replicate 32 0)

readRF :: RegisterFile -> Word32 -> Word32
readRF (RegisterFile rf) 0 = 0
readRF (RegisterFile rf) rid = rf ! (fromIntegral rid)

updateRF :: RegisterFile -> Word32 -> Word32 -> RegisterFile
updateRF rf 0 _ = rf
updateRF (RegisterFile rf) rid rdata = RegisterFile $ rf // [(fromIntegral rid, rdata)]
