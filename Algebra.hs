{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module Algebra where

import NumericPrelude 
import Data.List hiding (sum,product)
import Control.Monad.ST
import Data.Array.ST hiding (unsafeFreeze)
import Control.Monad
import Data.STRef
import Debug.Trace
import Data.Maybe
import Data.Array
import Utils
import qualified MathObj.Matrix as Matrix
import Data.Array
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Ring as Ring
import qualified Algebra.IntegralDomain as Domain
import qualified MathObj.Polynomial as Poly
import qualified Number.Ratio as Ratio
import qualified Algebra.Units as Units
import qualified Algebra.Additive as AbelianGroup
import qualified Algebra.Module as Module
import MatrixConversions
import qualified MatrixUtils                              
import Data.Array.Unsafe
import qualified PermutationAlgorithms as PermAlgs
import qualified Algebra.Monoid as Monoid
import qualified Data.Map as Map
import FreeModule
import Tensor

class (Ring.C k,Eq k, Ord g) => AlgebraGenerator g k where
  (<*>) :: g -> g -> FreeModule g k
  idt :: FreeModule g k
  

instance (AlgebraGenerator m k, AlgebraGenerator n k) => AlgebraGenerator (Tensor m n) k where
  (Tensor x1 x2)<*>(Tensor y1 y2) = (x1<*>y1)`tensor`(x2<*>y2)
  idt = idt`tensor`idt

-- r had best be a commutative ring for this to really make sence
instance (AlgebraGenerator m r, Ord m, Ring.C r, Eq r) => Ring.C (FreeModule m r) where
  m * n =  sum $ [(r1*r2)*>(x<*>y) 
                 | (x,r1) <- toAList m, (y,r2) <- toAList n]
  one = idt
    
kCoefOf t s = let (b,xr) = fromFModule t
                  (x,r) = fromFModule xr
              in (coefOf (coefOf s b) r)/x

algebraMapkMatrix source_basis target_basis fn = Matrix.fromColumns (d-c) (b-a)
  $ map (\s -> let fs = fn s 
             in map (\t -> kCoefOf t fs) (elems target_basis))
  (elems source_basis)
  where (a,b) = bounds source_basis
        (c,d) = bounds target_basis

--for convinience
(**>) :: (Ord r,AlgebraGenerator r k, Ord b) => r -> FreeModule b (FreeModule r k) -> FreeModule b (FreeModule r k)
(**>) r g = ((toFModule r) `asCoefOf` g) *> g