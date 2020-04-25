module StageExecute
  ( stageExecute
  )
where
import           Data.Bits
import           StageReg
import           Memory
import           ALU
import           Branch
import           RegisterFile

stageExecute :: ID_EX_Reg -> EX_MEM_Reg
stageExecute id_ex_reg = ex_mem_reg where
  -- MODULE: ALU
  alu_op          = id_alu_op id_ex_reg
  alu_src1        = id_alu_src1 id_ex_reg
  alu_src2        = id_alu_src2 id_ex_reg
  opcode          = id_opcode id_ex_reg
  pc'             = id_pc id_ex_reg
  alu_branch_mask = id_alu_branch_mask id_ex_reg
  branch_pc       = id_branch_pc id_ex_reg
  next_pc         = id_next_pc id_ex_reg
  rf_dest         = id_rf_dest id_ex_reg
  mem_data        = id_mem_data id_ex_reg

  alu_out         = aluRead alu_op alu_src1 alu_src2

  -- MODULE: Branch
  is_branch       = isBranchOp opcode
  take_branch     = is_branch && ((alu_out == 0) `xor` alu_branch_mask)
  pc''            = if take_branch then branch_pc else next_pc

  ex_mem_reg      = EX_MEM_Reg alu_out
                               opcode
                               pc''
                               (id_rf_dest id_ex_reg)
                               (id_mem_data id_ex_reg)
