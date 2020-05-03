module Main where

import           CPU
import           Utils

main :: IO ()
main = do
  imem <- loadIMem "test/naive-tests/8-loop.hex"
  cycles (boot imem) 10000000
  return ()
