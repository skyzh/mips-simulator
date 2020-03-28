module Lib
    ( cycles,
      boot
    ) where

import Data.Word (Word32)

data Registers = Registers { pc :: Word32 }
    deriving (Read, Show, Eq)
cpu_cycle regs =  Registers new_pc where
    new_pc = (pc regs) + 4

boot :: Registers
boot = Registers 0

cycles :: Registers -> Int -> IO ()
cycles regs 0 = putStrLn "Done."
cycles regs times = do 
    let new_regs = cpu_cycle regs
    putStrLn $ show  new_regs
    cycles new_regs (times - 1)
