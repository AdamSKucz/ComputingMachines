import Text.Printf
import System.CPUTime

import RMTypes

pairToPosInt :: (Integer, Integer) -> Integer
pairToPosInt (n1, n2) = 2 ^ n1 * (2*n2 + 1)

posIntToPair :: Integer -> (Integer, Integer)
posIntToPair n = case n `divMod` 2 of
  (n', 0) -> let (x, y) = posIntToPair n' in (x+1,y)
  (n', 1) -> (0, n')

pairToNat :: (Integer, Integer) -> Integer
pairToNat = (subtract 1) . pairToPosInt

natToPair :: Integer -> (Integer, Integer)
natToPair = posIntToPair . (+1)

listToNat :: [Integer] -> Integer
listToNat [] = 0
listToNat (n:ns) = pairToPosInt (n, listToNat ns)

natToList :: Integer -> [Integer]
natToList 0 = []
natToList n = let (x, n') = posIntToPair n in x : natToList n'

instrToNat :: Instruction -> Integer
instrToNat HALT = 0
instrToNat (INC r j) = pairToPosInt (2*r, j)
instrToNat (DEC r j1 j2) = pairToPosInt (2*r+1, pairToNat (j1, j2))

natToInstr :: Integer -> Instruction
natToInstr 0 = HALT
natToInstr n = case flag of
    0 -> INC x' y
    1 -> let (j1, j2) = natToPair y in DEC x' j1 j2
  where (x,y) = posIntToPair n
        (x',flag) = x `divMod` 2

progToNat :: [Instruction] -> Integer
progToNat = listToNat . fmap instrToNat

natToProg :: Integer -> [Instruction]
natToProg = fmap natToInstr . natToList

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v