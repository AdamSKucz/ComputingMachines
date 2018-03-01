module RMTypes where

import Data.Map (Map)

data Instruction = 
    HALT
  | INC Integer Integer
  | DEC Integer Integer Integer
  deriving (Read, Show, Eq)
type RegisterMachine = [Instruction]
type RegisterState = Map Integer Integer