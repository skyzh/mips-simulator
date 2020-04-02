import           Test.HUnit
import           System.IO
import           Prelude                 hiding ( take )
import           Data.Vector
import           Data.Word
import           Lib
import           Memory
import           Utils
import           TestUtils
import           RegisterFile
import           Registers

main :: IO ()
main = do
  runTestTT tests
  return ()

testLoadIMem = TestCase $ do
  mem <- loadIMem "test/naive-tests/0-imem.hex"
  let Memory content = mem
  assertEqual "0 byte" (content ! 0) 0xef
  assertEqual "1 byte" (content ! 1) 0xbe
  assertEqual "2 byte" (content ! 2) 0xad
  assertEqual "3 byte" (content ! 3) 0xde

testAdd = TestCase $ do
  mem  <- loadIMem "test/naive-tests/1-test-add.hex"
  regs <- cycles (boot mem) 10
  assertEqual "result" (a2 regs) 300

testArithmetic = TestCase $ do
  mem  <- loadIMem "test/naive-tests/2-basic-arithmetic.hex"
  regs <- cycles (boot mem) 50
  let (RegisterFile rf') = rf regs
  assertEqual
    "registers"
    (take 22 rf')
    (fromList
      [ 0
      , 0xffff
      , 0
      , 0
      , 0xfffe0000
      , 0xfffe0000
      , 0xffff01d2
      , 0xffff0000
      , 0x00e90000
      , 0xffff0000
      , 0xffff0000
      , 0x00e9fffe
      , 0x00e90000
      , 0
      , 0xff170000
      , 0xff170000
      , 0
      , 0xffff0001
      , 0xffff0001
      , 0xff160000
      , 0x00e9ffff
      , 0x0000ffff
      ]
    )

tests = TestList
  [ TestLabel "test load instruction memory" testLoadIMem
  , TestLabel "test add"                     testAdd
  , TestLabel "test arithmetic"              testArithmetic
  ]
