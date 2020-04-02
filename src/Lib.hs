module Lib
  ( cycles
  , boot
  )
where

import           Data.Word                      ( Word16
                                                , Word32
                                                )
import           RegisterFile
import           Memory
import           Data.Vector                    ( (//) )
import           Debug.Trace
import           Data.Bits
import           Utils
import           ALU

data Registers = Registers {
    rf :: RegisterFile -- register file
  , imem :: Memory    -- instruction memory
  , dmem :: Memory    -- data memory
  , pc :: Word32      -- program counter
  } deriving (Show)

-- initial register set
boot :: Memory -> Registers
boot imem = Registers bootRF imem bootMem 0

-- debug current cycle information
debug_cycle regs = do
  putStrLn $ "PC = " ++ show (pc regs)
  putStrLn $ show (rf regs)

-- run n cycles
cycles :: Registers -> Int -> IO ()
cycles regs 0     = debug_cycle regs
cycles regs times = do
  debug_cycle regs
  cycles (cpu_cycle regs) (times - 1)

-- mux
mux :: Bool -> a -> a -> a
mux False a b = a
mux True  a b = b

-- run one cycle
cpu_cycle regs = trace debug_info (Registers new_rf imem' new_dmem new_pc) where
  imem'          = imem regs
  new_dmem       = dmem regs

  -- instruction memory and instruction fetch
  pc'            = pc regs
  imem_addr      = pc'
  imem_read_size = 32
  instruction    = readMem imem' imem_addr imem_read_size

  -- decode
  opcode         = instruction `shiftR` 26
  rs             = instruction `shiftR` (26 - 5) .&. 0x1f
  rt             = instruction `shiftR` (26 - 10) .&. 0x1f
  rd             = instruction `shiftR` (26 - 15) .&. 0x1f
  funct          = instruction .&. 0x3f
  imm :: Word16
  imm        = fromIntegral instruction .&. 0xffff
  typeR      = opcode == 0

  imm_sign_ext = signExt imm
  imm_zero_ext = zeroExt imm

  debug_info =
    "opcode"
      ++ show opcode
      ++ "rs"
      ++ show rs
      ++ "rt"
      ++ show rt
      ++ "rd"
      ++ show rd
      ++ "funct"
      ++ show funct
      ++ "imm"
      ++ show imm

  -- register file operations
  rf'      = rf regs
  rf_src1  = rs
  rf_src2  = mux typeR 0 rt
  rf_dest  = mux typeR rt rd
  rf_data  = alu_out
  rf_write = True
  rf_out1  = readRF rf' rf_src1
  rf_out2  = readRF rf' rf_src2

  -- execute
  alu_op = mux typeR opcode funct
  ext_mode = extMode alu_op
  alu_imm = mux ext_mode imm_zero_ext imm_sign_ext
  alu_src1 = rf_out1
  alu_src2 = mux typeR alu_imm rf_out2
  alu_out  = aluRead alu_op alu_src1 alu_src2

  -- memory

  -- write back

  -- these values shouldn't be read
  new_rf   = mux rf_write rf' new_rf'
    where new_rf' = updateRF rf' rf_dest rf_data
  new_pc = pc' + 4
