{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module ZMod2 where

import Prelude hiding ((+),(-),(*),negate,fromInteger,sum,product)
import AlgebraicStructure
import Utils
import Data.Array.Unboxed
import System.Random

newtype ZMod2 = ZMod2 Bool deriving (IArray UArray, Eq, Ord)

instance Show ZMod2 where
  show (ZMod2 b) = if b then "1" else "0"

instance AbelianGroup ZMod2 where
  (+) = ZMod2 .-. (xor `on` (==one))
  negate = id
  zero = ZMod2 False
  
instance Monoid ZMod2 where
  (*) = ZMod2 .-. ((&&) `on` (==one))
  one = ZMod2 True
  
instance Ring ZMod2 where
  fromInteger = ZMod2 . odd

instance Field ZMod2 where
  x / y = if y == one then x else error "Z/2 Divide by Zero"
  
instance Random ZMod2 where
  random g = let (r,g') = random g in (ZMod2 r,g')
  randomR (ZMod2 x,ZMod2 y) g = let (r,g') = randomR (x,y) g in (ZMod2 r, g')
