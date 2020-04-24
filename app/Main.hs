module Main where

import           CPU
import           Utils

main :: IO ()
main = do
  imem <- loadIMem "test/naive-tests/1-test-add.hex"
  cycles (boot imem) 10
  return ()
