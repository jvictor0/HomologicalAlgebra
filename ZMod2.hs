{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, NoImplicitPrelude #-}
module ZMod2 where

import NumericPrelude
import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import qualified Algebra.Field as Field
import Utils
import Data.Array.Unboxed
import System.Random

newtype ZMod2 = ZMod2 Bool deriving (IArray UArray, Eq, Ord)

instance Show ZMod2 where
  show (ZMod2 b) = if b then "1" else "0"

instance Additive.C ZMod2 where
  (+) = ZMod2 .-. (xor `on` (==one))
  negate = id
  zero = ZMod2 False
  
instance Ring.C ZMod2 where
  (*) = ZMod2 .-. ((&&) `on` (==one))
  one = ZMod2 True
  fromInteger n = ZMod2 $ odd n

instance Field.C ZMod2 where
  x / y = if y == one then x else error "Z/2 Divide by Zero"
  
instance Random ZMod2 where
  random g = let (r,g') = random g in (ZMod2 r,g')
  randomR (ZMod2 x,ZMod2 y) g = let (r,g') = randomR (x,y) g in (ZMod2 r, g')
