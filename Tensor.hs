{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module Tensor where

import NumericPrelude
import FreeModule
import qualified Data.Map as Map
import Utils 
import Data.List hiding (sum)
import GradedObject
import Data.Array
import qualified MathObj.Matrix as Matrix
import qualified Algebra.Ring as Ring
import qualified MatrixAlgorithms
import qualified MatrixUtils
import qualified Algebra.Module as Module


data Tensor a b = Tensor a b deriving (Eq, Ord)

instance (Show a, Show b) => Show (Tensor a b) where
  show (Tensor a b) = (rshow a) ++ "(x)" ++ (rshow b)


type FreeTensor a b r = FreeModule (Tensor a b) r

instance (Eq k, Ord r, Ord m, Ring.C k, Ring.C (FreeModule r k)) => (Module.C (FreeModule r k) (FreeModule (Tensor r m) k)) where
  r *> m = smap (\(Tensor s x) -> (r*(toFModule s)) `tensor` (toFModule x)) m 

tensor :: (Eq r, Ord b, Ord a, Ring.C r) =>
           FreeModule a r -> FreeModule b r -> FreeModule (Tensor a b) r
tensor x y = fromAList [(Tensor a b,r*s) | (a,r) <- toAList x, (b,s) <- toAList y]

ten f g = smap (\(Tensor x y) -> (f x)`tensor`(g y))

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
        
