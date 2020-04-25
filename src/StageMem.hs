module StageMem
  ( stageMem
  )
where

import           StageReg
import           Memory

stageMem :: EX_MEM_Reg -> Memory -> (MEM_WB_Reg, Memory)
stageMem ex_mem_reg dmem' = (mem_wb_reg, new_dmem) where
  rf_dest   = ex_rf_dest ex_mem_reg
  alu_out   = ex_alu_out ex_mem_reg
  opcode    = ex_opcode ex_mem_reg
  mem_data  = ex_mem_data ex_mem_reg
  mem_addr  = alu_out
  mem_write = memoryStore opcode
  mem_mode  = memoryMode opcode
  mem_out   = readMem dmem' mem_addr mem_mode
  new_dmem =
    if mem_write then updateMem dmem' mem_addr mem_data mem_mode else dmem'
  mem_wb_reg = MEM_WB_Reg (ex_pc ex_mem_reg) mem_out alu_out opcode rf_dest
