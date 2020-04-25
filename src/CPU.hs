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
cpu_cycle regs = trace debug_info next_regs where
  imem'           = imem regs
  dmem'           = dmem regs

  -- STAGE: Instruction Fetch
  curr_if_id_reg  = if_id regs
  next_if_id_reg  = stageInstFetch (pc regs) imem'

  -- STAGE: Decode
  rf'             = rf regs
  curr_id_ex_reg  = id_ex regs
  next_id_ex_reg  = stageInstDecode curr_if_id_reg rf'

  -- STAGE: Execute
  -- REQUIRE: alu_op, alu_src1, alu_src2, opcode, pc, alu_branch_mask
  curr_ex_mem_reg = ex_mem regs
  next_ex_mem_reg = stageExecute curr_id_ex_reg

  -- STAGE: Memory
  curr_mem_wb_reg = mem_wb regs
  stage_mem_out   = stageMem curr_ex_mem_reg dmem'
  next_mem_wb_reg = fst stage_mem_out
  new_dmem        = snd stage_mem_out

  -- STAGE: Write Back
  -- REQUIRE: mem_out, alu_out, opcode, pc
  opcode          = mem_opcode curr_mem_wb_reg
  is_branch       = isBranchOp opcode
  pc'             = mem_pc curr_mem_wb_reg
  alu_out         = mem_alu_out curr_mem_wb_reg
  mem_write       = memoryStore opcode
  rf_dest         = mem_rf_dest curr_mem_wb_reg
  rf_write        = not is_branch && not mem_write && opcode /= 2
  is_mem_load     = memoryLoad opcode
  mem_mem_out     = mem_out curr_mem_wb_reg
  
  rf_data | is_mem_load = mem_mem_out
          | opcode == 3 = pc' + 4
          | otherwise   = alu_out

  -- STEP: update register value
  -- CONSTRAINT: these values shouldn't be read above 
  --             (even if I do, there'll be dead loop)
  -- CONSTRAINT: units can only be used once (e.g. aluRead)
  new_rf = if rf_write then new_rf' else rf'
    where new_rf' = updateRF rf' rf_dest rf_data
  new_pc    = (pc regs) + 4
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
    show curr_if_id_reg
      ++ "\n"
      ++ show curr_id_ex_reg
      ++ "\n"
      ++ show curr_ex_mem_reg
      ++ "\n"
