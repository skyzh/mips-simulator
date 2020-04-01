import           Test.HUnit
import           System.IO
import           Lib
import           Memory
import           Utils
import           Prelude                 hiding ( take )
import           Data.Vector

main :: IO ()
main = do
  mem <- loadIMem "test/naive-tests/1-test-add.hex"
  let Memory content = mem
  putStrLn $ show (take 32 content)
