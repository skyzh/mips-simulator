module Main where

import           Lib
import           Utils

main :: IO ()
main = do
  imem <- loadIMem "test/naive-tests/1-test-add.hex"
  cycles (boot imem) 10
  return ()
