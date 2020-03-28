module RegisterFile where
import           Prelude                 hiding ( replicate )
import           Data.Word                      ( Word32 )
import           Data.Vector
data RegisterFile = RegisterFile (Vector Word32)
  deriving(Show)

-- 32 registers
bootRF :: RegisterFile
bootRF = RegisterFile (replicate 32 0)

readRF :: RegisterFile -> Int -> Word32
readRF (RegisterFile rf) rid = rf ! rid

updateRF :: RegisterFile -> Int -> Word32 -> RegisterFile
updateRF (RegisterFile rf) rid rdata = RegisterFile $ rf // [(rid, rdata)]