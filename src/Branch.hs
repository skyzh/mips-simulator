module Branch
  ( isBranchOp
  , branchRtVal
  , takeBranch
  , branchOut
  , isLinkOp
  , overrideRt
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

isLinkOp :: Word32 -> Bool
isLinkOp 0x3 = True
isLinkOp _   = False

-- for bgez, bltz, bgtz, blez, set rt to 0 or 1
branchRtVal :: Word32 -> Word32
-- blez = slt rs 1
branchRtVal 0x6 = 1
-- bgtz = !(slt rs 1)
branchRtVal 0x7 = 1
-- bgez = !(slt rs 0)
-- bltz = slt rs 0
branchRtVal 0x1 = 0

overrideRt :: Word32 -> Bool
-- blez
overrideRt 0x6 = True
-- bgtz
overrideRt 0x7 = True
-- bgez bltz
overrideRt 0x1 = True
overrideRt _   = False

branchOut :: Word32 -> Word32 -> Bool
-- beq
branchOut 0x4 _   = False
-- bne
branchOut 0x5 _   = True
-- bgez
branchOut 0x1 0x1 = False
-- bltz
branchOut 0x1 0x0 = True
-- bgtz
branchOut 0x7 0x0 = False
-- blez
branchOut 0x6 0x0 = True
branchOut _   _   = False

takeBranch :: Word32 -> Word32 -> Word32 -> Bool
-- beq
takeBranch 0x4 _   alu_out = alu_out == 0
-- bne
takeBranch 0x5 _   alu_out = alu_out /= 0
-- bgez
takeBranch 0x1 0x1 alu_out = alu_out == 0
-- bltz
takeBranch 0x1 0x0 alu_out = alu_out /= 0
-- bgtz
takeBranch 0x7 0x0 alu_out = alu_out == 0
-- blez
takeBranch 0x6 0x0 alu_out = alu_out /= 0
-- other op
takeBranch _   _   _       = False
