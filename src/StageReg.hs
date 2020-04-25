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
    id_alu_src2 :: Word32,
    id_opcode :: Word32,
    id_pc :: Word32,
    id_alu_branch_mask :: Bool,
    id_branch_pc :: Word32,
    id_next_pc :: Word32,
    id_rf_dest :: Word32,
    id_mem_data :: Word32
  } deriving (Show)

defaultIDEXReg = ID_EX_Reg 0 0 0 0 0 False 0 0 0 0

data EX_MEM_Reg = EX_MEM_Reg {
    ex_alu_out :: Word32,
    ex_opcode :: Word32,
    ex_pc :: Word32,
    ex_rf_dest :: Word32,
    ex_mem_data :: Word32
  } deriving (Show)

defaultEXMEMReg = EX_MEM_Reg 0 0 0 0 0

data MEM_WB_Reg = MEM_WB_Reg {
    mem_pc :: Word32,
    mem_out :: Word32,
    mem_alu_out :: Word32,
    mem_opcode :: Word32,
    mem_rf_dest :: Word32
  } deriving (Show)

defaultMEMWBReg = MEM_WB_Reg 0 0 0 0 0