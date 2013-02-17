{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module Subalgebra where

import NumericPrelude
import FreeModule
import qualified Data.Map as Map
import Utils 
import Data.List
import GradedObject
import Data.Array
import qualified MathObj.Matrix as Matrix
import qualified Algebra.Ring as Ring
import qualified Algebra.Field as Field
import qualified Algebra.Additive as Additive
import qualified MatrixAlgorithms
import qualified MatrixUtils
import Tensor
import Debug.Trace
import Algebra
import Data.Char
import Data.Maybe
import qualified Subspace as SS

data Subalgebra b r k = SA (SS.Subspace (Tensor r b) k)


zeroSpace = SA SS.zeroSpace
size (SA s) = SS.size s


instance (Show k, Show r, Ord r, Ring.C k, Eq k, Show b, Ord b) => Show (Subalgebra b r k) where
  show (SA s) = show s
  
reduce vect (SA sb) = reduceStructure $ SS.reduce (induceStructure vect) sb
                                
insertContains vect subsp@(SA ss) = let (ss',b) = SS.insertContains (induceStructure vect) ss in (SA ss',b)
  
contains vect subsp = snd $ insertContains vect subsp

insert vect subsp = fst $ insertContains vect subsp

