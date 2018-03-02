module Interface (
  arg1, args,
  compute, 
  traceComputation, printComputation,
  customTraceJumpsInComputation, customPrintJumpsInComputation, 
  printMachine
) where

import qualified Data.Map as Map

import Control.Monad.Writer (runWriter)
import Control.Monad.State (evalState, evalStateT)

import Data.Maybe (fromJust, isJust)

import RMTypes
import Interpreter
import Packer

arg1 :: Natural -> RegisterState
arg1 = Map.singleton 1

args :: [Natural] -> RegisterState
args = Map.fromList . zip [1..]
         
compute :: RegisterMachine -> [Natural] -> Natural
compute rm = evalState (run rm) . args

showState :: ComputationState -> String
showState (l, HALT,           rs) = 
    show l ++ ": " ++ show HALT
showState (l, i@(INC r _),    rs) = 
    show l ++ ": " ++ show i ++ "   with R" ++ show r ++ " = " ++ show (getRegister r rs)
showState (l, i@(DEC r _ _),  rs) = 
    show l ++ ": " ++ show i ++ " with R" ++ show r ++ " = " ++ show (getRegister r rs)

traceComputation :: RegisterMachine -> [Natural] -> (Natural, [String])
traceComputation rm = runWriter . evalStateT (runAndLog showState rm) . args

printComputation :: RegisterMachine -> [Natural] -> IO Natural
printComputation rm args = sequence_ (fmap putStrLn log) >> return n
  where (n, log) = traceComputation rm args

customShowCodeBlockTransitions :: 
  Natural -> (ComputationState -> Location -> String) -> ComputationState -> Maybe String
customShowCodeBlockTransitions t f a@(l, HALT,           rs) = Just $ f a 0
customShowCodeBlockTransitions t f a@(l, i@(INC r j),    rs) = 
  if abs (fromIntegral l - fromIntegral j) < fromIntegral t 
    then Nothing 
    else Just $ f a j
customShowCodeBlockTransitions t f a@(l, i@(DEC r j1 j2),  rs) = 
  if abs (fromIntegral l - fromIntegral j) < fromIntegral t 
    then Nothing
    else Just $ f a j
  where n = getRegister r rs
        j = if n > 0 then j1 else j2

customTraceJumpsInComputation :: 
  Natural -> (ComputationState -> Location -> String) -> RegisterMachine -> [Natural] -> (Natural, Int, [String])
customTraceJumpsInComputation t f rm = 
      getPresentOnly . 
      runWriter . 
      evalStateT (runAndLog (customShowCodeBlockTransitions t f) rm) . 
      args
  where getPresentOnly (n, ls) = (n, length ls, fmap fromJust $ filter isJust ls)

customPrintJumpsInComputation :: 
  Natural -> (ComputationState -> Location -> String) -> RegisterMachine -> [Natural] -> IO Natural
customPrintJumpsInComputation t f rm args = do
    sequence_ (fmap putStrLn log)
    putStrLn (show nJumps ++ " instructions in total")
    return n
  where (n, nJumps, log) = customTraceJumpsInComputation t f rm args

printMachine :: RegisterMachine -> IO ()
printMachine = sequence_ . fmap (\(i,a) -> putStrLn $ show i ++ ": " ++ show a) . zip [0..]