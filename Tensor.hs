{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module Tensor where

import NumericPrelude
import FreeModule
import qualified Data.Map as Map
import Utils 
import Data.List hiding (sum)
import Data.Array
import qualified MathObj.Matrix as Matrix
import qualified Algebra.Ring as Ring
import qualified MatrixAlgorithms
import qualified MatrixUtils
import qualified Algebra.Module as Module

data Tensor a b = Tensor a b deriving (Eq)

instance (Show a, Show b) => Show (Tensor a b) where
  show (Tensor a b) = (rshow a) ++ "(x)" ++ (rshow b)

instance (UnShow a, UnShow b) => UnShow (Tensor a b) where
  unShow str = let (a,b) = splitSubstr "(x)" str in Tensor (unShow a) (unShow b)

instance (Read a, Read b) => Read (Tensor a b) where
  readsPrec n str = (readsPrec n str) >>= (\(a,rst) -> 
                                            (if (take (length "(x)") rst) == "(x)" then readsPrec n (drop (length "(x)") rst) else [])
                                            >>= (\(b,rst') -> [(Tensor a b,rst')]))
  

instance (Ord a, Ord b) => Ord (Tensor a b) where
  compare (Tensor a b) (Tensor c d) = case compare b d of
    EQ -> compare a c
    comp -> comp

type FreeRTensor a b r = FreeModule (Tensor a b) r

type FreekTensor a b s t k = FreeModule (Tensor a b) (FreeModule (Tensor s t) k)

instance (Eq k, Ord r, Ord m, Ring.C k, Ring.C (FreeModule r k)) => (Module.C (FreeModule r k) (FreeModule (Tensor r m) k)) where
  r *> m = smap (\(Tensor s x) -> (r*(toFModule s)) `tensor` (toFModule x)) m 

tensor :: (Eq r, Ord b, Ord a, Ring.C r) =>
           FreeModule a r -> FreeModule b r -> FreeModule (Tensor a b) r
tensor x y = fromAList [(Tensor a b,r*s) | (a,r) <- toAList x, (b,s) <- toAList y]

tensor_k :: (Eq s, Eq t, Ring.C (FreeModule (Tensor s t) k), Ord t, Ord b, Eq k, Ord s, Ord a, Ring.C k)
            => FreeModule a (FreeModule s k) -> FreeModule b (FreeModule t k) 
            -> FreekTensor a b s t k
tensor_k x y = fromAList [(Tensor a b,(r*r')*>(toFModule (Tensor s t)))
                          | (Tensor s a,r) <- toAList dx, (Tensor t b,r') <- toAList dy]
  where (dx,dy) = (induceStructure x,induceStructure y)

ten_R f g = smap (\(Tensor x y) -> (f x)`tensor`(g y))


ten_k :: (Ord b, Ord a, Ord a', Ord b', Ord t', Ord s', Eq k, Ord s, Ord t, Ring.C k, Ring.C (FreeModule (Tensor s t) k),
         Ring.C (FreeModule s k), Ring.C (FreeModule t k), Ring.C (FreeModule s' k), Ring.C (FreeModule t' k)) =>
         (FreeModule a' (FreeModule s' k) -> FreeModule a (FreeModule s k))
         -> (FreeModule b' (FreeModule t' k)
             -> FreeModule b (FreeModule t k))
         -> FreekTensor a' b' s' t' k
         -> FreekTensor a b s t k
ten_k f g v = sum $ 
              map (\(Tensor x y,r) -> 
                    sum $ map (\(Tensor s t,r) -> 
                                (f $ reduceStructure $ r *> (toFModule $ Tensor s x))`tensor_k`
                                (g $ reduceStructure $ r *> (toFModule $ Tensor t y)))
                    $ toAList r)
              $ toAList v


tensorFlip_k v = vmap (\(Tensor x y, r) -> (Tensor y x, vmap (\(Tensor s t,x) -> (Tensor t s,x)) r)) v

induceStructure ::(Eq k, Ring.C k, Ord m, Ord r)
                  => (FreeModule m (FreeModule r k)) -> (FreeModule (Tensor r m) k)
induceStructure v = sum $ map (\(m,a) -> tensor a (toFModule m)) $ toAList v

reduceStructure :: (Eq r, Ord m', Ord m, Ring.C (FreeModule m r), Ring.C r) =>
     FreeModule (Tensor m m') r -> FreeModule m' (FreeModule m r)
reduceStructure v = vmap (\(Tensor r m,k) -> (m,k*>(toFModule r))) v

induceMatrix :: (Eq r, Ord b, Ord a, Ring.C r,Ring.C (FreeModule a r))
                => Array Int (Tensor a b) -> Array Int (Tensor a b) -> (b -> FreeModule (Tensor a b) r) -> Matrix.T r
induceMatrix dom codom fn = Matrix.fromColumns (d-b+1) (c-a+1)  
                            $ map (\i -> let rs = lk $ dom!i
                                         in map (\j -> (coefOf rs (codom!j))) [b..d]) [a..c]
  where lk (Tensor r x) = smap (\(Tensor s y) -> ((toFModule r)*(toFModule s))`tensor`(toFModule y)) (fn x)
        (a,c) = bounds dom
        (b,d) = bounds codom
        
