{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module Subspace where

import NumericPrelude
import FreeModule
import Data.List hiding (sum, insert)
import qualified Data.Map as Map
import Utils 
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
import Data.Tuple

data Subspace r k = SS (Map.Map r (FreeModule r k)) deriving (Eq,Ord)


zeroSpace = SS Map.empty
size (SS s) = Map.size s

leastNonzero v = fst $ head $ toAList v
leastNonzeroCoef v = snd $ head $ toAList v

toList (SS s) = Map.elems s

fromList lst = foldr insert zeroSpace lst

fillOutSpace (SS set) lst = SS $ foldr addIt set $ lst
  where addIt b st = case Map.lookup b set of
          Nothing -> Map.insert b (toFModule b) st
          (Just _) -> st

reduceWithCoefs vect (SS s) 
  | vect == zero = ([],zero)
  | otherwise = case Map.splitLookup (leastNonzero vect) s of
    (_,Just u,rst) -> let c = (leastNonzeroCoef vect)/(leastNonzeroCoef u)
                      in let (res,reduc) = reduceWithCoefs (vect-(c*>u)) $ SS rst
                         in ((c,u):res,reduc)
    _              -> ([],vect)

reduceWithAllCoefs :: (Eq b, Eq r, Field.C r, Ord b) => (FreeModule b r) -> (Subspace b r) -> ([(r,FreeModule b r)],FreeModule b r)
reduceWithAllCoefs vect (SS subspace) = swap $ 
                                        mapAccumL (\v (b_great,b) ->
                                                    if (v /= zero) && ((leastNonzero v)  == b_great)
                                                    then let c = (leastNonzeroCoef v)/(leastNonzeroCoef b)
                                                         in (v-(c*>b),(c,b))
                                                    else (v,(zero,b)))
                                        vect $ Map.toList subspace
                                                       

toMatrix src_basis targ_basis phi = Matrix.fromColumns (size targ_basis) (size src_basis) $
  map (\b -> 
        let (phi_b_cos,bezero) = reduceWithAllCoefs (phi b) targ_basis
        in if bezero == zero
           then map fst phi_b_cos
           else error $ "Subspace.toMatrix: phi(src_basis)\\not{\\subseteq} targ_basis\n  src = " ++ (show src_basis) ++ "\n  and targ = " ++ (show targ_basis))
   (toList src_basis)


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

