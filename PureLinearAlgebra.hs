{-# LANGUAGE FlexibleInstances, FlexibleContexts,  MultiParamTypeClasses #-}
module PureLinearAlgebra where

import Prelude hiding ((+),(-),(*),negate,fromInteger,sum,product)
import AlgebraicStructure
import FreeModule
import qualified Data.Map as Map
import Utils 
import Instances
import Data.List hiding (sum)
import GradedObject
import Data.Array.Unboxed
import Data.Tuple.HT

--we use matlab conventions for indexing!
data Matrix u r = Matrix (u (Int,Int) r)
data Vector u r = Vector (u Int r)
--type Basis b = Array Int b

instance (IArray u r,Show r) => Show (Vector u r) where
  show (Vector v) = "[" ++ (cim " " show $ elems v) ++ "]"

instance (IArray u r,Show r) => Show (Matrix u r) where
  show (Matrix v) = cim "\n" (\i -> "[" ++(cim " "  (\j -> show $ v!(i,j)) [a..c]) ++ "]") [b..d]
    where ((a,b),(c,d)) = bounds v
  
-- this is a bit sloppy, but might work if pure vectors are only considered internally
instance (IArray u r, AbelianGroup r) => (AbelianGroup (Vector u r)) where
  (Vector v1) + (Vector v2)
   | bounds v1 == bounds v2 = Vector $ listArray (bounds v1) $ zipWith (+) (elems v1) (elems v2)
   | otherwise = error $ "cannot add vectors of different sizes. "
  zero = error "no zero for arbitrary sized vector"


instance (IArray u r, AbelianGroup r) => (AbelianGroup (Matrix u r)) where
  (Matrix v1) + (Matrix v2)
   | bounds v1 == bounds v2 = Matrix $ array (bounds v1) $ zipWith (\(i,x) (_,y) -> (i,x+y)) (assocs v1) (assocs v2)
   | otherwise = error $ "cannot add vectors of different sizes."
  zero = error "no zero for arbitrary sized matrix"

        
instance (IArray u r,Ring r) => Monoid (Matrix u r) where
  (Matrix m1) * (Matrix m2) = if n==n2 
                              then Matrix $ array ((1,1),(m,r)) 
                                   [((i,j),sum $ map (\k-> (m1!(i,k))*(m2!(k,j))) [1..n]) 
                                   | i <- [1..m], j <- [1..r]]
                              else error "matrix multiplication size mismatch"
    where ((1,1),(m,n)) = bounds m1
          ((1,1),(n2,r)) = bounds m2
  one = error "arbitrary dimension matrix has no unit"
  
instance (IArray u r,Ring r) => Ring (Matrix u r)

instance (IArray u r,Ring r) => Module (Vector u r) (Matrix u r) where
  (Matrix mat) *> (Vector v) = if n == n2 
                             then Vector $ array (1,m)    
                                  [(i,sum $ map (\k-> (v!k)*(mat!(i,k))) [1..n])
                                   | i <- [1..m]]
                             else error "Matrix-vector product dimension mismatch"
    where ((1,1),(m,n)) = bounds mat
          (1,n2)        = bounds v

-- We're going to prefix things with "v"s and "m"s for now, because... I don't know why...
vDot   :: (Monoid g, AbelianGroup g, IArray a g, IArray a1 g) =>
          Vector a g -> Vector a1 g -> g
vDot (Vector v1) (Vector v2)
  | bounds v1 == bounds v2 = sum $ zipWith (*) (elems v1) (elems v2)
  | otherwise = error "cannot dot vectors of different sizes"
    
mTranspose :: (IArray a1 e, IArray a e) => Matrix a1 e -> a (Int, Int) e
mTranspose (Matrix m) = array ((b,a),(d,c)) $ [((j,i),x) | ((i,j),x) <- assocs m]
  where ((a,b),(c,d)) = bounds m

mAt (Matrix m) (i,j) = m!(i,j)
vAt (Vector v) i = v!i

mSize (Matrix m) = snd $ bounds m
vSize (Vector v) = snd $ bounds v

fromList :: IArray u r => [r] -> Vector u r
fromList ls = vector (length ls) ls
vector ::  IArray u r => Int -> [r] ->  Vector u r
vector n ls = Vector $ listArray (1,n) ls
fromLists :: IArray u r => [[r]] -> Matrix u r
fromLists [] = error "empty matrix"
fromLists lsts = matrix (length lsts,length $ head lsts) lsts
matrix :: IArray u r => (Int,Int) -> [[r]] -> Matrix u r
matrix (m,n) lsts = Matrix $ array ((1,1),(m,n)) 
                    $ concat $ zipWith (\i ls -> zipWith (\j x -> ((i,j),x)) [1..n] ls) [1..m] lsts

mColumn :: (IArray r u) => Int -> Matrix r u -> Vector r u
mColumn i mat = Vector $ array (1,m) $ map (\j -> (i,mAt mat (j,i))) [1..m]
  where (m,_) = mSize mat
mRow :: (IArray r u) => Int -> Matrix r u -> Matrix r u
mRow i mat = Matrix $ array ((1,1),(1,m)) $ map (\j -> ((1,i),mAt mat (j,i))) [1..m]
  where (m,_) = mSize mat

