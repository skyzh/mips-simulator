module ALU where

import           Prelude                 hiding ( xor )
import           Data.Word                      ( Word16
                                                , Word32
                                                )
import           Data.Int                       ( Int16 )
import           Debug.Trace
import           Data.Bits

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
-- sub
aluRead 0x22 x y = x - y
-- subu
aluRead 0x23 x y = x - y
-- and
aluRead 0x24 x y = x .&. y
-- andi
aluRead 0x0C x y = x .&. y
-- div
aluRead 0x1A x y = error "div"
-- divu
aluRead 0x1B x y = error "divu"
-- mult
aluRead 0x18 x y = error "mult"
-- multu
aluRead 0x19 x y = error "multu"
-- nor
aluRead 0x27 x y = complement (x .|. y)
-- or
aluRead 0x25 x y = x .|. y
-- ori
aluRead 0x0D x y = x .|. y
-- xor
aluRead 0x26 x y = x `xor` y
-- xori
aluRead 0x0E x y = x `xor` y
--- lui
aluRead 0x0F x y = y `shiftL` 16 + x
--- other op
aluRead 0    _ _ = 0
aluRead op _ _ =
  trace ("\x1b[32m" ++ "unknown opcode " ++ "\x1b[0m" ++ show op) 0
