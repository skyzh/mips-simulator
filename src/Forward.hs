module Forward
  ( forward
  , ForwardInfo(..)
  )
where
import           Data.Word                      ( Word32 )
import           Branch                         ( isBranchOp
                                                , isLinkOp
                                                )
import           Memory                         ( memoryLoad
                                                , memoryStore
                                                )
import           ALU                            ( isArithmeticOp )

data ForwardInfo = ForwardInfo Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32

forward :: ForwardInfo -> Word32 -> (Word32, Bool, Bool)
forward (ForwardInfo ex_opcode ex_dest ex_val mem_opcode mem_dest mem_alu_val mem_val wb_opcode wb_dest wb_val) src
  | src == 0
  = (0, False, False)
  | ex_dest == src && isArithmeticOp ex_opcode
  = (ex_val, True, False)
  | ex_dest == src && memoryLoad ex_opcode -- use after load, stall
  = (0, True, True)
  | ex_dest == src && isLinkOp ex_opcode
  = (ex_val, True, False)
  | mem_dest == src && isArithmeticOp mem_opcode
  = (mem_alu_val, True, False)
  | mem_dest == src && memoryLoad mem_opcode
  = (mem_val, True, False)
  | mem_dest == src && isLinkOp mem_opcode
  = (mem_alu_val, True, False)
  | wb_dest
    == src
    && (isArithmeticOp wb_opcode || memoryLoad wb_opcode || isLinkOp wb_opcode)
  = (wb_val, True, False)
  | otherwise
  = (0, False, False)
