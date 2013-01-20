{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Rand where

import System.Random
import System.IO
import Control.Monad
import Control.Monad.State

class (Monad m) => MonadRandom m where
  getRandom :: (Random a) => m a
  
instance MonadRandom IO where
  getRandom = randomIO
  
class RandomState rs where
  getRandomFromState :: rs -> StdGen
  setRandomToState :: rs -> StdGen -> rs
  
instance RandomState StdGen where
  getRandomFromState = id
  setRandomToState = flip const

instance (RandomState rs) => MonadRandom (State rs) where
  getRandom = do
    st <- get
    let (a,g) = random $ getRandomFromState st
    put $ setRandomToState st g
    return a
    
