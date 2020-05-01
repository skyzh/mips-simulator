import           System.IO
import           CPU
import           Memory
import           Registers
import           Utils
import           Criterion.Main

setupEnv :: IO (Int -> IO Registers)
setupEnv = do
  mem <- loadIMem "test/naive-tests/8-loop.hex"
  let cycle_with_mem = cycles (boot mem)
  return cycle_with_mem

main = defaultMain
  [
   -- notice the lazy pattern match here!
    env setupEnv $ \ ~cycle_with_mem ->
      bgroup "mem" [bench "1000000 (1MHz) cycles" $ whnfIO (cycle_with_mem 1000000)]
  ]
