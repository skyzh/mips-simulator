module Registers where

import           Data.Word                      ( Word32 )
import           Memory
import           RegisterFile

data Registers = Registers {
    -- register file
    rf :: RegisterFile
    -- multiplication hi and lo
  , hi :: Word32
  , lo :: Word32
    -- instruction memory
  , imem :: Memory
    -- data memory
  , dmem :: Memory
    -- program counter 
  , pc :: Word32
  } deriving (Show)
