{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State.Class (MonadState, modify, gets)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Reader (runReaderT)

import Data.Map (adjust, alter, findWithDefault)

import RMTypes

step :: (MonadState RegisterState m, MonadReader RegisterMachine m) => Integer -> m (Maybe Integer)
step i = do
  instructions <- ask
  if fromIntegral i >= length instructions
    then return Nothing
    else case instructions !! fromIntegral i of
            HALT          -> return Nothing
            (INC r j)     -> modify (alter (Just . maybe 1 (+1)) r) >> return (Just j)
            (DEC r j1 j2) -> do a <- gets (findWithDefault 0 r)
                                case a of
                                  0 -> return (Just j2)
                                  n -> modify (adjust (subtract 1) r) >> return (Just j1)

runWithRegisters :: MonadState RegisterState m => RegisterMachine -> m Integer
runWithRegisters rm = runWithRM 0 >> gets (findWithDefault 0 0)
  where stepWithRM :: MonadState RegisterState n => Integer -> n (Maybe Integer)
        stepWithRM = flip runReaderT rm . step
        runWithRM :: MonadState RegisterState n => Integer -> n ()
        runWithRM n = stepWithRM n >>= maybe (return ()) runWithRM