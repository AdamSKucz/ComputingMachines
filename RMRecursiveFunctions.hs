module RMRecursiveFunctions (
) where

import RMTypes
import RMMachines

projF :: Natural -> Natural -> RegisterMachine
projF n i = copyReg i 0

zeroF :: Natural -> RegisterMachine
zeroF = const [HALT]

succF :: RegisterMachine
succF = 