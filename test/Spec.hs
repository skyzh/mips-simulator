import           System.IO
import           Prelude                 hiding ( take )
import           Data.Vector
import           Data.Word
import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Lib
import           Memory
import           Utils
import           TestUtils
import           RegisterFile
import           Registers

main :: IO ()
main = hspec $ do
  describe "unit test" $ do
    it "load instruction memory" testLoadIMem

  describe "assembly test" $ do
    it "add" $ testAdd
    it "simple arithmetic operation"  testArithmetic
    it "compare arithmetic operation" testCompare

testLoadIMem = do
  mem <- loadIMem "test/naive-tests/0-imem.hex"
  let Memory content = mem
  shouldBe (content ! 0) 0xef
  shouldBe (content ! 1) 0xbe
  shouldBe (content ! 2) 0xad
  shouldBe (content ! 3) 0xde

testAdd = do
  mem  <- loadIMem "test/naive-tests/1-test-add.hex"
  regs <- cycles (boot mem) 10
  shouldBe (a2 regs) 300

testArithmetic = do
  mem  <- loadIMem "test/naive-tests/2-basic-arithmetic.hex"
  regs <- cycles (boot mem) 50
  let (RegisterFile rf') = rf regs
  shouldBe
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

testCompare = do
  mem  <- loadIMem "test/naive-tests/3-basic-compare.hex"
  regs <- cycles (boot mem) 50
  let (RegisterFile rf') = rf regs
  shouldBe
    (take 17 rf')
    (fromList
      [ 0
      , 0xfffe0000
      , 0
      , 0
      , 0x000f4240
      , 0x001e8480
      , 0x00000014
      , 0xfffe7960
      , 0x3d090000
      , 0x24000000
      , 0x000003d0
      , 0x0
      , 0x000003d0
      , 0
      , 0x1
      , 0
      , 0x1
      ]
    )
