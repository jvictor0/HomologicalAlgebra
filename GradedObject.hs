{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module GradedObject where

import NumericPrelude
import FreeModule
import qualified Data.Map as Map
import Utils 
import Data.List
import Tensor

class Graded g where
  grading :: g -> Int
  
class (Graded g) => BiGraded g where
  biGrading :: g -> (Int,Int)
  biGrading g = (grading g, internalGrading g)
  internalGrading :: g -> Int
  internalGrading = snd.biGrading
  
    
totalGrading :: (BiGraded g) => g -> Int
totalGrading g = let (s,t) = biGrading g in s+t

stableGrading :: (BiGraded g) => g -> Int
stableGrading g = let (s,t) = biGrading g in s-t

gradedSummand v = map fromAList 
                    $ partitionsBy (\(g,r) -> grading g) $ toAList v

biGradedSummand v = map fromAList 
                    $ partitionsBy (\(g,r) -> biGrading g) $ toAList v


instance (Graded g) => Graded (FreeModule g r) where
  grading m = case nub $ map (grading.fst) $ toAList m of
    [n] -> n
    []  -> 0
    _   -> error "cannot get grading of nonhomogenious vector"

instance (BiGraded g) => BiGraded (FreeModule g r) where
  biGrading m = case nub $ map (biGrading.fst) $ toAList m of
    [n] -> n
    []  -> (0,0)
    _   -> error "cannot get grading of nonhomogenious vector"


instance (Graded s, Graded t) => Graded (Tensor s t) where
  grading (Tensor s t) = (grading s) + (grading t)

