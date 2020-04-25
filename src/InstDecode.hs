module InstDecode
  ( stageInstDecode
  )
where

import           Data.Bits
import           StageReg
import           Memory
import           ALU
import           Branch
import           RegisterFile

stageInstDecode :: IF_ID_Reg -> RegisterFile -> ID_EX_Reg
stageInstDecode if_id_reg rf = id_ex_reg where
  instruction  = if_instruction if_id_reg
  pc'          = if_pc if_id_reg

  opcode       = instruction `shiftR` 26
  rs           = instruction `shiftR` (26 - 5) .&. 0x1f
  rt           = instruction `shiftR` (26 - 10) .&. 0x1f
  rd           = instruction `shiftR` (26 - 15) .&. 0x1f
  shamt        = instruction `shiftR` (26 - 20) .&. 0x1f
  funct        = instruction .&. 0x3f
  imm          = fromIntegral instruction .&. 0xffff
  imm_sign_ext = signExt imm
  imm_zero_ext = zeroExt imm
  typeR        = opcode == 0
  use_shamt    = (isShift funct) && typeR
  jump_target =
    ((instruction `shiftL` 2) .&. 0x0fffffff) .|. (pc' .&. 0xf0000000)

  -- MODULE: Register file
  rf_src1 = rs
  rf_src2 = if typeR || is_branch || is_memory then rt else 0
  rf_dest | typeR       = rd
          | opcode == 3 = 31
          | otherwise   = rt
  rf_out1    = readRF rf rf_src1
  rf_out2    = readRF rf rf_src2

  -- MODULE: Branch
  imm_offset = imm_sign_ext `shiftL` 2
  branch_pc  = pc' + 4 + imm_offset
  next_pc | opcode == 2 || opcode == 3  = jump_target
          | opcode == 0 && funct == 0x8 = rf_out1
          | otherwise                   = pc' + 4
  branch_alu_rt_val = branchRtVal opcode rf_out2
  is_branch         = isBranchOp opcode
  alu_branch_mask   = branchOut opcode rt

  -- MODULE: Memory
  mapped_op         = mapALUOp opcode
  is_memory         = isMemoryOp opcode
  is_mem_load       = memoryLoad opcode

  -- MODULE: ALU
  ext_mode          = extMode alu_op
  alu_op            = if typeR then funct else mapped_op
  alu_imm           = if ext_mode then imm_sign_ext else imm_zero_ext
  alu_src1          = if use_shamt then shamt else rf_out1
  alu_src2 | typeR     = rf_out2
           | is_branch = branch_alu_rt_val
           | otherwise = alu_imm

  -- MODULE: Memory
  mem_data  = rf_out2

  id_ex_reg = ID_EX_Reg alu_op
                        alu_src1
                        alu_src2
                        opcode
                        pc'
                        alu_branch_mask
                        branch_pc
                        next_pc
                        rf_dest
                        mem_data
