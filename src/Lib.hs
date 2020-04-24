module Lib
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

-- initial register set
boot :: Memory -> Registers
boot imem = Registers bootRF 0 0 imem bootMem 0

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
cpu_cycle regs = next_regs where
  imem'          = imem regs
  dmem'          = dmem regs

  -- STAGE: Instruction Fetch
  pc'            = pc regs
  imem_addr      = pc'
  imem_read_size = 32
  instruction    = readMem imem' imem_addr imem_read_size

  -- STAGE: Decode
  opcode         = instruction `shiftR` 26
  rs             = instruction `shiftR` (26 - 5) .&. 0x1f
  rt             = instruction `shiftR` (26 - 10) .&. 0x1f
  rd             = instruction `shiftR` (26 - 15) .&. 0x1f
  shamt          = instruction `shiftR` (26 - 20) .&. 0x1f
  funct          = instruction .&. 0x3f
  imm            = fromIntegral instruction .&. 0xffff
  imm_sign_ext   = signExt imm
  imm_zero_ext   = zeroExt imm
  typeR          = opcode == 0
  use_shamt      = (isShift funct) && typeR
  jump_target =
    ((instruction `shiftL` 2) .&. 0x0fffffff) .|. (pc' .&. 0xf0000000)

  -- MODULE: Register file
  rf'               = rf regs
  rf_src1           = rs
  rf_src2           = if typeR || is_branch || is_memory then rt else 0
  rf_dest           | typeR = rd
                    | opcode == 3 = 31 
                    | otherwise = rt
  rf_out1           = readRF rf' rf_src1
  rf_out2           = readRF rf' rf_src2

  -- MODULE: Branch
  imm_offset        = imm_sign_ext `shiftL` 2
  branch_pc         = pc' + 4 + imm_offset
  next_pc           = pc' + 4
  branch_alu_rt_val = branchRtVal opcode rf_out2
  is_branch         = isBranchOp opcode

  -- MODULE: Memory
  mapped_op         = mapALUOp opcode
  is_memory         = isMemoryOp opcode

  -- STAGE: Execute
  -- MODULE: ALU
  ext_mode          = extMode alu_op
  alu_op            = if typeR then funct else mapped_op
  alu_imm           = if ext_mode then imm_sign_ext else imm_zero_ext
  alu_src1          = if use_shamt then shamt else rf_out1
  alu_src2 | typeR     = rf_out2
           | is_branch = branch_alu_rt_val
           | otherwise = alu_imm
  alu_out     = aluRead alu_op alu_src1 alu_src2

  -- MODULE: Branch
  take_branch = takeBranch opcode rt alu_out

  pc''        | take_branch = branch_pc 
              | opcode == 2 || opcode == 3 = jump_target
              | opcode == 0 && funct == 0x8 = rf_out1
              | otherwise =  next_pc

  -- STAGE: Memory
  mem_write   = memoryStore opcode
  mem_addr    = alu_out
  mem_data    = rf_out2
  mem_mode    = memoryMode opcode
  mem_out     = readMem dmem' mem_addr mem_mode

  -- STAGE: Write Back
  is_mem_load = memoryLoad opcode
  rf_write    = not is_branch && not mem_write && opcode /= 2
  rf_data | is_mem_load = mem_out
          | opcode == 3 = pc' + 4
          | otherwise   = alu_out

  -- STEP: type annotation
  imm :: Word16

  -- STEP: update register value
  -- CONSTRAINT: these values shouldn't be read above 
  --             (even if I do, there'll be dead loop)
  -- CONSTRAINT: units can only be used once (e.g. aluRead)
  new_rf = if rf_write then new_rf' else rf'
    where new_rf' = updateRF rf' rf_dest rf_data
  new_pc = pc''
  new_hi = hi regs
  new_lo = lo regs
  new_dmem =
    if mem_write then updateMem dmem' mem_addr mem_data mem_mode else dmem'
  next_regs  = Registers new_rf new_hi new_lo imem' new_dmem new_pc

  -- STEP: debug info
  debug_info = "pc=" ++ show pc'
