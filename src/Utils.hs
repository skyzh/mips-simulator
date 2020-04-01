module Utils where

import           Prelude                 hiding ( replicate )
import           Memory
import           Data.Vector
import           Data.Word                      ( Word8
                                                , Word32
                                                )
import           Numeric                        ( readHex )
import           Data.Bits

loadIMem :: String -> IO Memory
loadIMem filename = do
  content <- readFile filename
  return $ Memory (parseMem content)

parseMem :: String -> Vector Word8
parseMem content = parse (lines content) (replicate defaultMemSize 0) 0 where
  parse :: [String] -> Vector Word8 -> Int -> Vector Word8
  parse (x : xs) mem addr = parse xs (update' x mem addr) (addr + 4)
  parse _        mem _    = mem
  update' :: String -> Vector Word8 -> Int -> Vector Word8
  update' hex mem addr =
    mem
      // [ (addr    , byte 3)
         , (addr + 1, byte 2)
         , (addr + 2, byte 1)
         , (addr + 3, byte 0)
         ]   where
    datax :: Int
    datax = let [(x, _)] = readHex hex in x
    byte :: Int -> Word8
    byte x = convertMemSize $ datax `shiftR` (x * 8)
