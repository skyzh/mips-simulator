module ALU where
import           Data.Word                      ( Word16
                                                , Word32
                                                )
import           Data.Int                       ( Int16 )
import           Debug.Trace

signExt :: Word16 -> Word32
signExt x = fromIntegral signed where
  signed :: Int16
  signed = fromIntegral x

zeroExt :: Word16 -> Word32
zeroExt x = fromIntegral x

-- extension mode
-- True if sign extension, otherwise False
extMode :: Word32 -> Bool
extMode 0x0C = False
extMode 0x0D = False
extMode 0x0E = False
extMode 0x24 = False
extMode 0x25 = False
extMode _    = True

aluRead :: Word32 -> Word32 -> Word32 -> Word32
-- add
aluRead 0x20 x y = x + y
-- addu
aluRead 0x21 x y = x + y
-- addi
aluRead 0x8  x y = x + y
aluRead 0x9  x y = x + y
aluRead op   _ _ = trace ("unknown opcode " ++ show op) 0
