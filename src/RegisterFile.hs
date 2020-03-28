module RegisterFile where
import           Prelude                 hiding ( replicate )
import           Data.Word                      ( Word32 )
import           Data.Vector                    ( replicate
                                                , Vector
                                                )
data RegisterFile = RegisterFile (Vector Word32)
  deriving(Show)

-- 32 registers
bootRF :: RegisterFile
bootRF = RegisterFile (replicate 32 0)
