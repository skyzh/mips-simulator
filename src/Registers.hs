module Registers where

import           Data.Word                      ( Word32 )
import           Memory
import           RegisterFile
import           StageReg

data Registers = Registers {
    -- register file
    rf :: RegisterFile
    -- multiplication hi and lo
  , hi :: Word32
  , lo :: Word32
    -- instruction memory
  , imem :: Memory
    -- data memory
  , dmem :: Memory
    -- program counter 
  , pc :: Word32
  , if_id :: IF_ID_Reg
  , id_ex :: ID_EX_Reg
  , ex_mem :: EX_MEM_Reg
  , mem_wb :: MEM_WB_Reg
  } deriving (Show)
