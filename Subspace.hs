{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module Subspace where

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


data Subspace r k = SS (Map.Map r (FreeModule r k)) deriving (Eq,Ord)


zeroSpace = SS Map.empty
size (SS s) = Map.size s

leastNonzero v = fst $ head $ toAList v
leastNonzeroCoef v = coefOf v (leastNonzero v)

toList (SS s) = map snd $ Map.toList s

instance (Show k, Show r, Ord r, Ring.C k, Eq k) => Show (Subspace r k) where
  show (SS s) = show $ map snd $ Map.toList s
  
ssvKill u v = ((leastNonzeroCoef u)*>v) - ((leastNonzeroCoef v)*>u)
  
reduce vect (SS set) 
  | vect == zero = vect
  | otherwise = case Map.splitLookup (leastNonzero vect) set of
    (_,Just u,rst) -> reduce (ssvKill u vect) (SS rst)
    (_,Nothing,_) -> vect
                                
insertContains vect subsp@(SS set) = let v = reduce vect subsp in if v == zero then (subsp,True) else (SS $ Map.insert (leastNonzero v) v set, False)
  
contains vect subsp = snd $ insertContains vect subsp

insert vect subsp = fst $ insertContains vect subsp

