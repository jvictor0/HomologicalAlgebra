{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module SteenrodAlgebra1 where


import NumericPrelude
import Algebra
import FreeModule
import Data.List
import ZMod2
import SteenrodAlgebra

data SteenrodSquare1 = Sq1 [Int] deriving (Ord)

type SteenrodAlgebra1 = FreeModule SteenrodSquare1 ZMod2

instance Eq SteenrodSquare1 where
  (Sq1 [1,2,1,2]) == (Sq1 [2,1,2,1]) = True
  (Sq1 x) == (Sq1 y) = x == y 

steenrodOneBasis 0 = [Sq1[]]
steenrodOneBasis 1 = [Sq1[1]] 
steenrodOneBasis 2 = [Sq1[2]] 
steenrodOneBasis 3 = [Sq1[2,1],Sq1[1,2]] 
steenrodOneBasis 4 = [Sq1[1,2,1]] 
steenrodOneBasis 5 = [Sq1[2,1,2]] 
steenrodOneBasis 6 = [Sq1[1,2,1,2]] 
steenrodOneBasis _ = []

instance Show SteenrodSquare1 where
  show (Sq1 x) = show $ Sq x

instance AlgebraGenerator SteenrodSquare1 ZMod2 where
  (Sq1 x1) <*> (Sq1 x2) = sq1 $ x1 ++ x2
  idt = toFModule $ Sq1 []

sq1 :: [Int] -> SteenrodAlgebra1
sq1 [2,1,2,1] = toFModule $ Sq1 [1,2,1,2]
sq1 [2,2]     = toFModule $ Sq1 [1,2,1]
sq1 [2,2,2]   = toFModule $ Sq1 [1,2,1,2]
sq1 (a:b:c:d:e:rst) = 0
sq1 sq 
  | all ((==1).length) $ group sq  = toFModule $ Sq1 sq
  | otherwise                      = 0
