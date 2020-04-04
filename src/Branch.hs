module Branch
  ( isBranchOp
  , branchRtVal
  , takeBranch
  )
where
import           Data.Word                      ( Word32 )

-- check if we should replace rt
isBranchOp :: Word32 -> Bool
-- beq
isBranchOp 0x4 = True
-- bne
isBranchOp 0x5 = True
-- bgez, bltz
isBranchOp 0x1 = True
-- bgtz
isBranchOp 0x7 = True
-- blez
isBranchOp 0x6 = True

isBranchOp _   = False

-- for bgez, bltz, bgtz, blez, set rt to 0 or 1
branchRtVal :: Word32 -> Word32 -> Word32
-- blez = slt rs 1
branchRtVal 0x6 _ = 1
-- bgtz = !(slt rs 1)
branchRtVal 0x7 _ = 1
-- bgez = !(slt rs 0)
-- bltz = slt rs 0
branchRtVal 0x1 _ = 0
branchRtVal _   rt = rt

takeBranch :: Word32 -> Word32 -> Word32 -> Bool
-- beq
takeBranch 0x4 _   alu_out = alu_out == 0
-- bne
takeBranch 0x5 _   alu_out = alu_out /= 0
-- bgez
takeBranch 0x1 0x1 alu_out = alu_out == 0
-- bltz
takeBranch 0x1 0x0 alu_out = alu_out == 1
-- bgtz
takeBranch 0x7 0x0 alu_out = alu_out == 0
-- blez
takeBranch 0x6 0x0 alu_out = alu_out == 1
-- other op
takeBranch _   _   _       = False