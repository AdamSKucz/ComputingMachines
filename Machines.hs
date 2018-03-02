module Machines (
  assembleOneHaltMachine, assembleTwoHaltMachine,
  simplifyHalts, normalizeHalts,
  copyReg, push, pop, zeroReg, dec, inc,
  oneOutput, twoOutput,
  universal, universalInstance,
  adder, multiplier, proj, constant
) where

import Data.Foldable (fold)

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.List ((\\))
import qualified Data.List as List

import RMTypes

import Debug.Trace (trace)

mapInstrLoc :: (Location -> Location) -> Instruction -> Instruction
mapInstrLoc f HALT          = HALT
mapInstrLoc f (INC r j)     = INC r (f j)
mapInstrLoc f (DEC r j1 j2) = DEC r (f j1) (f j2)

mapLoc :: (Location -> Location) -> RegisterMachine -> RegisterMachine
mapLoc = fmap . mapInstrLoc

mapInstrReg :: (Register -> Register) -> Instruction -> Instruction
mapInstrReg f HALT          = HALT
mapInstrReg f (INC r j)     = INC (f r) j
mapInstrReg f (DEC r j1 j2) = DEC (f r) j1 j2

mapReg :: (Register -> Register) -> RegisterMachine -> RegisterMachine
mapReg = fmap . mapInstrReg

type MachineAssembler k = Map k Location -> Natural -> RegisterMachine -> RegisterMachine

assembleOneHaltMachine :: (Show k, Ord k) => Map k RegisterMachine -> k -> k -> Map k (MachineAssembler k) -> RegisterMachine
assembleOneHaltMachine machines startMachine haltName plugMap = fold assembled ++ [HALT]
  where names = startMachine : List.delete startMachine (Map.keys machines)
        pos = reverse . foldr (\name ls@(x:_) -> x + fromIntegral (length $ machines!name) : ls) [0] $ reverse names 
        posMap = Map.fromList $ zip (names ++ [haltName]) pos
        assembled = fmap (\name -> (plugMap!name) posMap (posMap!name) (machines!name)) names

assembleTwoHaltMachine :: Ord k => Map k RegisterMachine -> k -> k -> k -> Map k (MachineAssembler k) -> RegisterMachine
assembleTwoHaltMachine machines startMachine haltName exitName plugMap = fold assembled ++ [HALT, HALT]
  where names = startMachine : List.delete startMachine (Map.keys machines)
        pos' = reverse $ foldr (\name ls@(x:_) -> x + fromIntegral (length $ machines!name) : ls) [0] names
        pos = pos' ++ [last pos' + 1]
        posMap = Map.fromList $ zip (names ++ [haltName, exitName]) pos
        assembled = fmap (\name -> (plugMap!name) posMap (posMap!name) (machines!name)) names

shiftCode :: Natural -> RegisterMachine -> RegisterMachine
shiftCode n = mapLoc (+n)

simplifyHalts :: RegisterMachine -> RegisterMachine
simplifyHalts rm = ((fmap shiftIndices $ zip [0..] rm) \\ replicate nHalts HALT) ++ [HALT]
  where is = fmap fromIntegral $ List.elemIndices HALT rm
        nHalts = length is
        halt = fromIntegral $ length rm - nHalts
        shiftIndices (i, instr) = 
            mapInstrLoc (\loc -> if loc `elem` is then halt else adjust loc) instr
          where adjust i = i - (fromIntegral . length $ List.takeWhile (<i) is)

normalizeHalts :: RegisterMachine -> RegisterMachine
normalizeHalts rm = mapLoc (min halt) rm ++ [HALT]
  where halt = fromIntegral $ length rm

errHalt :: Location
errHalt = 10000

copyReg :: Register -> Register -> Register -> RegisterMachine
copyReg src dst aux = normalizeHalts
  [ DEC dst 0 1,
    DEC src 2 4,
    INC aux 3,
    INC dst 1,
    DEC aux 5 errHalt,
    INC src 4]

push :: Register -> Register -> Register -> RegisterMachine
push src dst aux = normalizeHalts
  [ INC aux 1,
    DEC dst 2 3,
    INC aux 0,
    DEC aux 4 5,
    INC dst 3,
    DEC src 1 errHalt]

pop :: Register -> Register -> Register -> RegisterMachine
pop src dst aux = ls
  where ls = [  DEC dst 0 1,
                DEC src 2 exit,
                INC src 3,
                DEC src 4 5,
                INC aux 3,
                DEC aux 7 6,
                INC dst 3,
                DEC aux 8 halt,
                INC src 5,
                HALT,
                HALT]
        halt = fromIntegral $ length ls - 2
        exit = fromIntegral $ length ls - 1

zeroReg :: Register -> RegisterMachine
zeroReg r = normalizeHalts
  [ DEC r 0 errHalt]

dec :: Register -> RegisterMachine
dec r = [DEC r 1 2, HALT, HALT]

inc :: Register -> RegisterMachine
inc r = [INC r 1, HALT]

plugOneAndShift :: Location -> Natural -> RegisterMachine -> RegisterMachine
plugOneAndShift l shift m 
  | last m == HALT  = mapLoc (\loc -> if loc == halt then l else loc + shift) m
  | otherwise       = undefined
  where halt = fromIntegral $ length m - 1

plugTwoAndShift :: Location -> Location -> Natural -> RegisterMachine -> RegisterMachine
plugTwoAndShift l1 l2 shift m 
  | last m == HALT  = mapLoc (\loc -> if loc == halt then l1 else if loc == exit then l2 else loc + shift) m
  | otherwise       = undefined
  where halt = fromIntegral $ length m - 2
        exit = fromIntegral $ length m - 1

oneOutput :: Ord k => k -> MachineAssembler k
oneOutput halt m = plugOneAndShift (m!halt)

twoOutput :: Ord k => k -> k -> MachineAssembler k
twoOutput halt exit m = plugTwoAndShift (m!halt) (m!exit)

--simplifyHalts $
universal :: [Register] -> RegisterMachine
universal [dst, p, a, pc, n, c, r, s, t, z] = assembleOneHaltMachine machines "push0toA" "halt" wiring
  where machines = Map.fromList
          [ ("push0toA", zeroReg a),
            ("setTtoP", copyReg p t z),
            ("popTtoN", pop t n z),
            ("decPC", dec pc),
            ("popNtoC", pop n c z),
            ("popAtoR0", pop a dst z),
            ("popAtoR", pop a r z),
            ("decC_1", dec c),
            ("decC_2", dec c),
            ("pushRtoS", push r s z),
            ("incR", inc r),
            ("incN", inc n),
            ("popNtoPC", pop n pc z),
            ("decR", dec r),
            ("setPCtoN", copyReg n pc z),
            ("pushRtoA", push r a z),
            ("popStoR", pop s r z)]
        wiring = Map.fromList
          [ ("push0toA",  oneOutput "setTtoP"), 
            ("setTtoP",   oneOutput "popTtoN"), 
            ("popTtoN",   twoOutput "decPC"     "popAtoR0"),
            ("decPC",     twoOutput "popTtoN"   "popNtoC"),
            ("popNtoC",   twoOutput "popAtoR"   "popAtoR0"),
            ("popAtoR0",  twoOutput "halt"      "halt"),
            ("popAtoR",   twoOutput "decC_1"    "decC_1"),
            ("decC_1",    twoOutput "decC_2"    "incR"),
            ("decC_2",    twoOutput "pushRtoS"  "incN"),
            ("pushRtoS",  oneOutput "popAtoR"),
            ("incR",      oneOutput "setPCtoN"),
            ("incN",      oneOutput "popNtoPC"),
            ("popNtoPC",  twoOutput "decR"      "decR"),
            ("decR",      twoOutput "pushRtoA"  "setPCtoN"),
            ("setPCtoN",  oneOutput "pushRtoA"),
            ("pushRtoA",  oneOutput "popStoR"),
            ("popStoR",   twoOutput "pushRtoA"  "setTtoP")]

universalInstance :: RegisterMachine
universalInstance = universal [0..9]

adder :: RegisterMachine
adder = normalizeHalts
  [ DEC 1 1 2,
    INC 0 0,
    DEC 2 3 errHalt,
    INC 0 2]

multiplier :: RegisterMachine
multiplier = normalizeHalts
  [ DEC 1 1 errHalt,
    DEC 2 2 4,
    INC 0 3,
    INC 3 1,
    DEC 3 5 0,
    INC 2 4]

proj :: RegisterMachine
proj = normalizeHalts
  [ DEC 1 1 errHalt,
    INC 0 0]
    
constant :: Natural -> RegisterMachine
constant 0 = [HALT]
constant n = INC 0 1 : shiftCode 1 (constant $ n - 1)