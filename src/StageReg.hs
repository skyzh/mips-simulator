module StageReg where

import           Data.Word                      ( Word32 )

data IF_ID_Reg = IF_ID_Reg {
    -- instruction
    if_instruction :: Word32
    -- program counter 
  , if_pc :: Word32
  } deriving (Show)

defaultIFIDReg :: IF_ID_Reg
defaultIFIDReg = IF_ID_Reg 0 0

data ID_EX_Reg = ID_EX_Reg {
    id_alu_op :: Word32,
    id_alu_src1 :: Word32,
    id_alu_src2::Word32,
    id_opcode::Word32,
    id_pc::Word32,
    id_alu_branch_mask::Bool
  } deriving (Show)
