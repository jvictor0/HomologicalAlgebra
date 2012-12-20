module Instances where

import qualified Prelude as Prelude ((+),(-),(*),negate,fromInteger)
import Prelude hiding ((+),(-),(*),negate,fromInteger,sum,product,Num)
import AlgebraicStructure
import Utils
import FreeModule

-- for convinience, Ints need be Rings
instance AbelianGroup Int where
  (+) = (Prelude.+)
  (-) = (Prelude.-)
  zero = 0
  
instance Monoid Int where
  one = 1
  (*) = (Prelude.*)
  
instance Ring Int where
  fromInteger = Prelude.fromInteger



-- yaaay polynomials!!!
newtype Monomial = X Int deriving (Eq,Ord)

instance Show Monomial where
  show (X i) = "x^" ++ (texShow i)
  
type Polynomial = FreeModule Monomial