module ALU where

import           Prelude                 hiding ( xor )
import           Data.Word                      ( Word16
                                                , Word32
                                                )
import           Data.Int                       ( Int16
                                                , Int32
                                                )
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

--- read output value from ALU
aluRead :: Word32 -> Word32 -> Word32 -> Word32
-- add
aluRead 0x20 x y = x + y
-- addu
aluRead 0x21 x y = x + y
-- addi
aluRead 0x08  x y = x + y
aluRead 0x09  x y = x + y
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
--- sll
aluRead 0x00 x y = x `shiftL` (maskShift y)
--- sllv
aluRead 0x04 x y = x `shiftL` (maskShift y)
--- sra
aluRead 0x03 x y = fromIntegral out where
  x' :: Int32
  x' = fromIntegral x
  out = x' `shiftR` (maskShift y)
--- srav
aluRead 0x07 x y = fromIntegral out where
  x' :: Int32
  x' = fromIntegral x
  out = x' `shiftR` (maskShift y)
--- srl
aluRead 0x02 x y = x `shiftR` (maskShift y)
--- srlv
aluRead 0x06 x y = x `shiftR` (maskShift y)
--- slt
aluRead 0x2A x y = if x' < y' then 1 else 0 where
  x' :: Int32
  y' :: Int32
  x' = fromIntegral x
  y' = fromIntegral y
--- sltu
aluRead 0x29 x y = if x < y then 1 else 0
--- slti
aluRead 0x0A x y = if x' < y' then 1 else 0 where
  x' :: Int32
  y' :: Int32
  x' = fromIntegral x
  y' = fromIntegral y
--- sltiu
aluRead 0x0B x y = if x < y then 1 else 0

--- other op
aluRead op _ _ =
  trace ("\x1b[32m" ++ "unknown opcode " ++ "\x1b[0m" ++ show op) 0

maskShift y = fromIntegral (y .&. 0x1f)