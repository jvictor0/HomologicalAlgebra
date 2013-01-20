{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module SteenrodResolutions where

import NumericPrelude
import qualified Algebra.Module as Module
import FreeModule
import Algebra
import Utils
import ZMod2
import Data.Array
import Data.List
import Data.Char
import qualified Algebra.Monoid as Monoid
import SteenrodAlgebra1
import Resolution
import qualified Data.Map as Map

--- first we compute an A(1) resolution of the sphere
connectiveKTheorySphere :: Int -> Int -> FreeResData ZMod2 SteenrodAlgebra1 ZMod2
connectiveKTheorySphere s t = extendResolution steenrodOneBasis
                              (beginResolution steenrodOneBasis
                               (\_ i -> if i == 0
                                        then Map.singleton (ResGen 0 0 0) 1
                                        else Map.empty)
                               (\_ i -> if i `elem` [1,2]
                                        then Map.singleton (ResGen 1 i 0) 
                                             $ (sq1[i]) *> (toFModule $ ResGen 0 0 0)
                                        else Map.empty)
                               0
                               t)
                              s t
                               
