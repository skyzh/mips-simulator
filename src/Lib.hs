module Lib
  ( cycles
  , boot
  )
where

import           Data.Word                      ( Word32 )
import           RegisterFile
import           Memory

data Registers = Registers {
    rf:: RegisterFile
  , mem :: Memory
  , pc :: Word32
  } deriving (Show)


cpu_cycle regs = Registers new_rf new_mem new_pc where
  new_pc  = (pc regs) + 4
  new_rf  = (rf regs)
  new_mem = (mem regs)

-- initial register set
boot :: Registers
boot = Registers bootRF bootMem 0

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
