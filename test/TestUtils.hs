module TestUtils where

import           Data.Vector
import           Registers
import           RegisterFile

rf_vec r = let RegisterFile f = rf r in f

at r = (rf_vec r) ! 1
v0 r = (rf_vec r) ! 2
v1 r = (rf_vec r) ! 3
a0 r = (rf_vec r) ! 4
a1 r = (rf_vec r) ! 5
a2 r = (rf_vec r) ! 6
a3 r = (rf_vec r) ! 7
tN r n | n <= 7 = (rf_vec r) ! (8 + n)
       | n >= 8 = (rf_vec r) ! (16 + n)
sN r n = (rf_vec r) ! (16 + n)
k0 r = (rf_vec r) ! 26
k1 r = (rf_vec r) ! 7

