module RMTypes (
    Natural,
    Register,
    Location,
    Instruction(..),
    RegisterMachine,
    RegisterState,
    ComputationState,
    getRegister
) where

import Data.Natural (Natural)
import Data.Map (Map, findWithDefault)

type Register = Natural
type Location = Natural
data Instruction = 
    HALT
  | INC Register Location
  | DEC Register Location Location
  deriving (Read, Show, Eq)
type RegisterMachine = [Instruction]
type RegisterState = Map Register Natural
type ComputationState = (Location, Instruction, RegisterState)

getRegister :: Register -> RegisterState -> Natural
getRegister = findWithDefault 0