import           Test.HUnit
import           System.IO
import           Lib
import           Memory
import           Utils
import           Prelude                 hiding ( take )
import           Data.Vector
import           Data.Word

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

tests = TestList [TestLabel "test load instruction memory" testLoadIMem]
