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
import           Forward                        ( forward
                                                , ForwardInfo
                                                )
import           Debug.Trace                    ( trace )

stageInstDecode :: IF_ID_Reg -> RegisterFile -> ForwardInfo -> (ID_EX_Reg, Bool)
stageInstDecode if_id_reg rf forward_info = (id_ex_reg, stall) where
  inst         = if_instruction if_id_reg
  pc'          = if_pc if_id_reg

  opcode       = inst `shiftR` 26
  rs           = inst `shiftR` (26 - 5) .&. 0x1f
  rt           = inst `shiftR` (26 - 10) .&. 0x1f
  rd           = inst `shiftR` (26 - 15) .&. 0x1f
  shamt        = inst `shiftR` (26 - 20) .&. 0x1f
  funct        = inst .&. 0x3f
  imm          = fromIntegral inst .&. 0xffff
  imm_sign_ext = signExt imm
  imm_zero_ext = zeroExt imm
  typeR        = opcode == 0
  use_shamt    = (isShift funct) && typeR
  jump_target  = ((inst `shiftL` 2) .&. 0x0fffffff) .|. (pc' .&. 0xf0000000)

  -- MODULE: Register file
  rf_src1      = rs
  rf_src2      = if typeR || is_branch || is_mem_store then rt else 0
  rf_dest | typeR       = rd
          | opcode == 3 = 31
          | otherwise   = rt
  rf_out1_prev = readRF rf rf_src1
  rf_out2_prev = readRF rf rf_src2
  rf_out1 | forward_depends forward_op1 = forward_result forward_op1
          | otherwise                   = rf_out1_prev
  rf_out2 | override_rt                 = branch_alu_rt_val
          | forward_depends forward_op2 = forward_result forward_op2
          | otherwise                   = rf_out2_prev

  -- MODULE: Branch
  imm_offset = imm_sign_ext `shiftL` 2
  branch_pc  = pc' + 4 + imm_offset
  next_pc | opcode == 2 || opcode == 3  = jump_target
          | opcode == 0 && funct == 0x8 = rf_out1
          | otherwise                   = pc' + 4
  override_rt       = overrideRt opcode
  branch_alu_rt_val = branchRtVal opcode
  is_branch         = isBranchOp opcode
  alu_branch_mask   = branchOut opcode rt
  force_jump = opcode == 2 || opcode == 3 || (opcode == 0 && funct == 0x8)

  -- MODULE: Memory
  mapped_op         = mapALUOp opcode
  is_memory         = isMemoryOp opcode
  is_mem_load       = memoryLoad opcode
  is_mem_store      = memoryStore opcode

  -- MODULE: ALU
  ext_mode          = extMode alu_op
  alu_op            = if typeR then funct else mapped_op
  alu_imm           = if ext_mode then imm_sign_ext else imm_zero_ext

  -- MODULE: Forward
  forward_op1       = forward forward_info rf_src1
  forward_op2       = forward forward_info rf_src2
  forward_result (x, _, _) = x
  forward_depends (_, x, _) = x
  forward_stall (_, _, x) = x
  stall =
    (alu_use_rf_out_1 && forward_stall forward_op1)
      || (alu_use_rf_out_2 && forward_stall forward_op2)
      || (is_mem_store && forward_stall forward_op2)

  debug_info =
    "forward_op1 = "
      ++ show forward_op1
      ++ " forward op2 = "
      ++ show forward_op2

  alu_use_rf_out_1 = not use_shamt && opcode /= 3
  alu_use_rf_out_2 = typeR || is_branch

  alu_src1 | use_shamt   = shamt
           | opcode == 3 = pc'
           | otherwise   = rf_out1
  alu_src2 | alu_use_rf_out_2 = rf_out2
           | opcode == 3      = 4
           | otherwise        = alu_imm

  -- MODULE: Memory
  mem_data | forward_depends forward_op2 = forward_result forward_op2
           | otherwise                   = rf_out2

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
                        (if_branch_taken if_id_reg)
                        force_jump
