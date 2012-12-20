{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module GradedObject where

import Prelude hiding ((+),(-),(*),negate,fromInteger,sum,product)
import AlgebraicStructure
import FreeModule
import qualified Data.Map as Map
import Utils 
import Instances
import Data.List

class Graded g where
  grading :: g -> Int
  
class BiGraded g where
  biGrading :: g -> (Int,Int)
  
totalGrading :: (BiGraded g) => g -> Int
totalGrading g = let (s,t) = biGrading g in s+t

englishLetters :: [String]
englishLetters = map return ['A'..'Z']

letters :: [String]
letters = "i":(englishLetters ++ (concatMap (\ls -> map (ls++) englishLetters) $ tail letters))

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




