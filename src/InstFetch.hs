module InstFetch
  ( stageInstFetch
  )
where

import           StageReg
import           Data.Word                      ( Word32 )
import           Memory                         ( Memory
                                                , readMem
                                                )

stageInstFetch :: Word32 -> Memory -> IF_ID_Reg
stageInstFetch pc imem = if_id_reg where
  imem_addr      = pc
  imem_read_size = 32
  instruction    = readMem imem imem_addr imem_read_size
  if_id_reg      = IF_ID_Reg instruction pc
