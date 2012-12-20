{- 
Algebraic Structures!!!! Yaaaay!!!
We don't enforce the algebraic laws, just enjoying
-} 
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module AlgebraicStructure where

import qualified Prelude as Prelude ((+),(-),(*),negate,fromInteger)
import Prelude hiding ((+),(-),(*),negate,fromInteger,sum,product,Num)


-- (+) is commutative and associative
-- a-a=zero
-- a + 0 = a
class AbelianGroup g where
  (+) :: g -> g -> g
  (-) :: g -> g -> g
  a - b = a + (negate b)
  negate :: g -> g
  negate a = zero - a
  zero :: g
  sum :: [g] -> g
  sum = foldr (+) zero

 
-- (*) is associative, not nessesarily commutative
-- 1 is a unit on both sides
class Monoid m where
  (*) :: m -> m -> m
  one :: m
  product :: [m] -> m
  product = foldr (*) one
  
-- monoid with inverse is group 
class (Monoid g) => NonAbelianGroup g where
  inv :: g -> g
  
-- if (*) distributes over (+), we have a ring
class (AbelianGroup r,Monoid r) => Ring r where
  fromInteger :: Integer -> r
  fromInteger 0 = zero
  fromInteger 1 = one
  fromInteger n 
   | n < 0    = negate $ fromInteger $ negate n 
   | n > 0    = let no2 = fromInteger $ n `div` 2 
                in no2 + no2 + (fromInteger $ n`mod`2)


-- A module over a ring
class (Monoid r,AbelianGroup m) => Module m r where
  (*>) :: r -> m -> m
 
-- needs no extra structure, just is what it is
class (Monoid r,Ring m, Module m r) => Algebra m r

class (Ring f) => Field f where
  (/) :: f -> f -> f
  
-- a Ring is a Module over itself
-- I can't believe this compiles!!!
instance (Ring r) => Module r r where
  (*>) = (*)


------------------------
--Integers are things --
------------------------
instance AbelianGroup Integer where
  (+) = (Prelude.+)
  (-) = (Prelude.-)
  zero = 0
  
instance Monoid Integer where
  one = 1
  (*) = (Prelude.*)
  
instance Ring Integer where
  fromInteger = Prelude.fromInteger


