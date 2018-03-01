import RMTypes

import qualified Data.Map as Map

arg1 :: Integer -> RegisterState
arg1 = Map.singleton 1

args :: [Integer] -> RegisterState
args = Map.fromList . zip [1..]

adder :: RegisterMachine
adder = [DEC 1 1 2,
         INC 0 0,
         DEC 2 3 4,
         INC 0 2,
         HALT]

