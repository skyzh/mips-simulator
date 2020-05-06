module CPU
  ( cycles
  , boot
  )
where

import           Data.Word                      ( Word16
                                                , Word32
                                                )
import           Data.Vector                    ( (//) )
import           Debug.Trace
import           Data.Bits
import           Utils
import           ALU
import           Branch
import           Registers
import           RegisterFile
import           Memory
import           InstFetch                      ( stageInstFetch )
import           InstDecode                     ( stageInstDecode )
import           StageExecute                   ( stageExecute )
import           StageMem                       ( stageMem )
import           StageReg
import           Forward                        ( ForwardInfo(..) )

-- initial register set
boot :: Memory -> Registers
boot imem = Registers bootRF
                      0
                      0
                      imem
                      bootMem
                      0
                      defaultIFIDReg
                      defaultIDEXReg
                      defaultEXMEMReg
                      defaultMEMWBReg

-- debug current cycle information
debug_cycle regs = do
  return ()
  {-putStrLn $ "PC = " ++ show (pc regs)
  putStrLn $ show (rf regs)-}

-- run n cycles
cycles :: Registers -> Int -> IO Registers
cycles regs 0 = do
  debug_cycle regs
  return regs
cycles regs times = do
  debug_cycle regs
  cycles (cpu_cycle regs) (times - 1)

-- run one cycle
cpu_cycle :: Registers -> Registers
cpu_cycle regs = next_regs where
  imem'                     = imem regs
  dmem'                     = dmem regs

  -- STAGE: Instruction Fetch
  curr_if_id_reg            = if_id regs
  correct_branch_prediction = fst branch_resolve_result
  branch_jump_target        = snd branch_resolve_result
  if_pc | not correct_branch_prediction = branch_jump_target
        | otherwise                     = pc regs
  stage_if_out = stageInstFetch if_pc imem'
  next_if_id_reg | stage_id_stall = curr_if_id_reg
                 | otherwise      = fst stage_if_out
  if_next_pc | stage_id_stall = if_pc
             | otherwise      = snd stage_if_out

  -- STAGE: Decode
  rf'            = rf regs
  curr_id_ex_reg = id_ex regs
  stage_id_out   = stageInstDecode curr_if_id_reg rf' forward_info
  stage_id_stall = snd stage_id_out
  stage_id_regs  = fst stage_id_out
  next_id_ex_reg
    | not correct_branch_prediction || stage_id_stall = defaultIDEXReg
    | otherwise = stage_id_regs
  forward_info = ForwardInfo (ex_opcode next_ex_mem_reg)
                             (ex_rf_dest next_ex_mem_reg)
                             (ex_alu_out next_ex_mem_reg)
                             (mem_opcode next_mem_wb_reg)
                             (mem_rf_dest next_mem_wb_reg)
                             (mem_alu_out next_mem_wb_reg)
                             (mem_out next_mem_wb_reg)
                             opcode
                             rf_dest
                             rf_data


  -- STAGE: Execute
  -- REQUIRE: alu_op, alu_src1, alu_src2, opcode, pc, alu_branch_mask
  curr_ex_mem_reg       = ex_mem regs
  stage_ex_out          = stageExecute curr_id_ex_reg
  next_ex_mem_reg       = fst stage_ex_out
  branch_resolve_result = snd stage_ex_out

  -- STAGE: Memory
  curr_mem_wb_reg       = mem_wb regs
  stage_mem_out         = stageMem curr_ex_mem_reg dmem'
  next_mem_wb_reg       = fst stage_mem_out
  new_dmem              = snd stage_mem_out

  -- STAGE: Write Back
  -- REQUIRE: mem_out, alu_out, opcode, pc
  opcode                = mem_opcode curr_mem_wb_reg
  is_branch             = isBranchOp opcode
  pc'                   = mem_pc curr_mem_wb_reg
  alu_out               = mem_alu_out curr_mem_wb_reg
  mem_write             = memoryStore opcode
  rf_dest               = mem_rf_dest curr_mem_wb_reg
  rf_write              = not is_branch && not mem_write && opcode /= 2
  is_mem_load           = memoryLoad opcode
  mem_mem_out           = mem_out curr_mem_wb_reg

  rf_data | is_mem_load = mem_mem_out
          | otherwise   = alu_out

  -- STEP: update register value
  -- CONSTRAINT: these values shouldn't be read above 
  --             (even if I do, there'll be dead loop)
  -- CONSTRAINT: units can only be used once (e.g. aluRead)
  new_rf = if rf_write then new_rf' else rf'
    where new_rf' = updateRF rf' rf_dest rf_data
  new_pc    = if_next_pc
  new_hi    = hi regs
  new_lo    = lo regs

  next_regs = Registers new_rf
                        new_hi
                        new_lo
                        imem'
                        new_dmem
                        new_pc
                        next_if_id_reg
                        next_id_ex_reg
                        next_ex_mem_reg
                        next_mem_wb_reg

  -- STEP: debug info
  debug_info =
    show next_if_id_reg
      ++ "\n"
      ++ show next_id_ex_reg
      ++ "\n"
      ++ show next_ex_mem_reg
      ++ "\n"
      ++ show next_mem_wb_reg
      ++ "\n"
      ++ show branch_resolve_result
      ++ "\n"
      ++ "id_stall=" ++ show stage_id_stall ++ "\n"
      ++ "branch=" ++ show branch_resolve_result ++ "\n"
