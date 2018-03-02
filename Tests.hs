module Tests where

import Data.Map ((!))
import qualified Data.Map as Map

import Text.Printf
import System.CPUTime

import RMTypes
import Interpreter
import Packer
import Machines
import Interface

simple1 :: RegisterMachine
simple1 = copyReg 1 2 3

simple2 :: RegisterMachine
simple2 = assembleOneHaltMachine machines "copy" "halt" wiring
  where machines = Map.fromList [("copy", simple1), ("add", adder)]
        wiring   = Map.fromList [("copy", oneOutput "add"),("add", oneOutput "halt")]

testUniversal :: RegisterMachine -> [Natural] -> Maybe (Natural, Natural)
testUniversal m args = if correct == univ then Nothing else Just (correct, univ)
  where correct = compute m args
        mCode = progToNat m
        argsCode = listToNat args
        univ = compute universalInstance [mCode, argsCode]

showUniversalState :: ComputationState -> Location -> String
showUniversalState (l, i, rs) j = newBlock ++ "\t("++show l++"->"++show j++"),\tstate " ++ registerState
  where newBlock = case j of
                    0   -> "push0toA"
                    105 -> "setTtoP"
                    73  -> "popTtoN"
                    8   -> "decPC"
                    40  -> "popNtoC"
                    29  -> "popAtoR0"
                    112 -> "halt"
                    18  -> "popAtoR"
                    2   -> "decC_1"
                    5   -> "decC_2"
                    91  -> "pushRtoS"
                    16  -> "incR"
                    14  -> "incN"
                    51  -> "popNtoPC"
                    11  -> "decR"
                    98  -> "setPCtoN"
                    84  -> "pushRtoA"
                    62  -> "popStoR"
        registerState = "PC = " ++ show (getRegister 3 rs) ++ 
                        ", instruction: " ++ show (natToInstr $ getRegister 4 rs) ++
                        ", simulated RS: " ++ show (natToList $ getRegister 2 rs) 
 
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v