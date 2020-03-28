module Lib
  ( cycles
  , boot
  )
where

import           Data.Word                      ( Word32 )
import           RegisterFile
import           Memory
import           Data.Vector                    ( (//) )
import           Debug.Trace
import           Data.Bits

data Registers = Registers {
    rf :: RegisterFile -- register file
  , imem :: Memory    -- instruction memory
  , dmem :: Memory    -- data memory
  , pc :: Word32      -- program counter
  } deriving (Show)

-- initial register set
boot :: Registers
boot = Registers bootRF bootMem bootMem 0

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
cpu_cycle regs = trace ("opcode" ++ show decode_opcode) Registers new_rf imem' new_dmem new_pc where
  imem'          = imem regs
  new_dmem       = dmem regs
  -- instruction memory and instruction fetch
  pc' = pc regs
  imem_addr      = pc'
  imem_read_size = 32
  imem_data      = readMem imem' imem_addr imem_read_size
  -- decode
  decode_opcode  = imem_data `shiftR` 26
  
  -- register file operations
  rf'            = rf regs
  rf_rs1         = 0
  rf_rs2         = 0
  rf_rt          = 2
  rf_data        = 2333
  rf_write       = True
  rf_out1        = readRF rf' rf_rs1
  rf_out2        = readRF rf' rf_rs2

  -- these values shouldn't be read
  new_rf = mux rf_write rf' new_rf' where new_rf' = updateRF rf' rf_rt rf_data
  new_pc         = pc' + 4
