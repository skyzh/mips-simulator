module Registers where

import           Data.Word                      ( Word32 )
import           Memory
import           RegisterFile

data Registers = Registers {
    rf :: RegisterFile -- register file
  , imem :: Memory    -- instruction memory
  , dmem :: Memory    -- data memory
  , pc :: Word32      -- program counter
  } deriving (Show)
