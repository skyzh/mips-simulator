module ALU
  ( signExt
  , zeroExt
  , extMode
  , aluRead
  , isShift
  , mapALUOp
  , isArithmeticOp
  )
where

import           Prelude                 hiding ( xor )
import           Data.Word                      ( Word16
                                                , Word32
                                                )
import           Data.Int                       ( Int16
                                                , Int32
                                                )
import           Debug.Trace
import           Data.Bits

-- map opcode to ALU operation
mapALUOp :: Word32 -> Word32
-- branch instructions
-- beq, bne = sub
mapALUOp 0x4 = 0x22
mapALUOp 0x5 = 0x22
-- bgez, bltz = slt
mapALUOp 0x1 = 0x2A
-- bgtz, blez = slt
mapALUOp 0x6 = 0x2A
mapALUOp 0x7 = 0x2A
-- lb, lw, sb, sw = add
mapALUOp 0x20 = 0x20
mapALUOp 0x23 = 0x20
mapALUOp 0x28 = 0x20
mapALUOp 0x2B = 0x20
-- other op as it is
mapALUOp op  = op

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

-- is shift
-- True if reads shift amount field
isShift :: Word32 -> Bool
isShift 0x02 = True
isShift 0x03 = True
isShift 0x00 = True
isShift _    = False

isArithmeticOp :: Word32 -> Bool
isArithmeticOp 0x0 = True
isArithmeticOp 0x08 = True
isArithmeticOp 0x09 = True
isArithmeticOp 0x0C = True
isArithmeticOp 0x0D = True
isArithmeticOp 0x0E = True
isArithmeticOp 0x0F = True
isArithmeticOp 0x0A = True
isArithmeticOp 0x0B = True
isArithmeticOp _ = False

--- read output value from ALU
aluRead :: Word32 -> Word32 -> Word32 -> Word32
-- add
aluRead 0x20 x y = x + y
-- addu
aluRead 0x21 x y = x + y
-- addi
aluRead 0x08 x y = x + y
-- addiu
aluRead 0x09 x y = x + y
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
aluRead 0x00 x y = y `shiftL` (maskShift x)
--- sllv
aluRead 0x04 x y = y `shiftL` (maskShift x)
--- sra
aluRead 0x03 x y = fromIntegral out where
  y' :: Int32
  y'  = fromIntegral y
  out = y' `shiftR` (maskShift x)
--- srav
aluRead 0x07 x y = fromIntegral out where
  y' :: Int32
  y'  = fromIntegral y
  out = y' `shiftR` (maskShift x)
--- srl
aluRead 0x02 x y = y `shiftR` (maskShift x)
--- srlv
aluRead 0x06 x y = y `shiftR` (maskShift x)
--- slt
aluRead 0x2A x y = slt x y
--- sltu
aluRead 0x2B x y = if x < y then 1 else 0
--- slti
aluRead 0x0A x y = slt x y
--- sltiu
aluRead 0x0B x y = if x < y then 1 else 0
--- other op
aluRead op _ _ =
  trace ("\x1b[32m" ++ "unknown opcode " ++ "\x1b[0m" ++ show op) 0

maskShift :: Word32 -> Int
maskShift y = fromIntegral (y .&. 0x1f)

--- slt utility
slt x y = if x' < y' then 1 else 0 where
  x' :: Int32
  y' :: Int32
  x' = fromIntegral x
  y' = fromIntegral y