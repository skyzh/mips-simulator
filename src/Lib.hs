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
  {-
  putStrLn $ "PC = " ++ show (pc regs)
  putStrLn $ show (rf regs) -}

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
  new_dmem       = dmem regs

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

  -- MODULE: Register file
  rf'            = rf regs
  rf_src1        = rs
  rf_src2        = if typeR || is_branch then rt else 0
  rf_dest        = if typeR then rd else rt
  rf_data        = alu_out
  rf_write       = True && not is_branch
  rf_out1        = readRF rf' rf_src1
  rf_out2        = readRF rf' rf_src2

  -- MODULE: Branch
  imm_offset     = imm_sign_ext `shiftL` 2
  branch_map_op  = mapBranchOp opcode
  branch_pc      = pc' + 4 + imm_offset
  next_pc        = pc' + 4
  alu_rt_val     = branchRtVal opcode rf_out2
  is_branch      = isBranchOp opcode

  -- STAGE: Execute
  -- MODULE: ALU
  ext_mode       = extMode alu_op
  alu_op         = if typeR then funct else branch_map_op
  alu_imm        = if ext_mode then imm_sign_ext else imm_zero_ext
  alu_src1       = if use_shamt then shamt else rf_out1
  alu_src2 | typeR     = rf_out2
           | is_branch = alu_rt_val
           | otherwise = alu_imm
  alu_out     = aluRead alu_op alu_src1 alu_src2

  -- MODULE: Branch
  take_branch = takeBranch opcode rt alu_out
  pc''        = if take_branch then branch_pc else next_pc

  -- STAGE: Memory

  -- STAGE: Write Back

  -- STEP: type annotation
  imm :: Word16

  -- STEP: update register value
  -- CONSTRAINT: these values shouldn't be read above 
  --             (even if I do, there'll be dead loop)
  -- CONSTRAINT: units can only be used once (e.g. aluRead)
  new_rf = if rf_write then new_rf' else rf'
    where new_rf' = updateRF rf' rf_dest rf_data
  new_pc    = pc''
  new_hi    = hi regs
  new_lo    = lo regs
  next_regs = Registers new_rf new_hi new_lo imem' new_dmem new_pc

  -- STEP: debug info
  debug_info =
    "opcode "
      ++ show opcode
      ++ " rs "
      ++ show rs
      ++ " rt "
      ++ show rt
      ++ " rd "
      ++ show rd
      ++ " funct "
      ++ show funct
      ++ " imm "
      ++ show imm
