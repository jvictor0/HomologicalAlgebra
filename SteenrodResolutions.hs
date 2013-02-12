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
import SteenrodAlgebra 
import Resolution
import qualified Data.Map as Map
import ResolutionSaver

-- first we compute an A(1) resolution of the sphere
-- should we call this "connective K SHPHEORY???"
connectiveKTheoryResSphere :: Int -> Int -> FreeResData ZMod2 SteenrodSquare1 ZMod2
connectiveKTheoryResSphere s t = extendResolution 
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
                                
stableHomotopyResSphere :: Int -> Int -> Int -> FreeResData ZMod2 SteenrodSquare ZMod2
stableHomotopyResSphere scbLim s t = extendResolution 
                              (beginResolution (serreCartanBasis scbLim)
                               (\_ i -> if i == 0
                                        then Map.singleton (ResGen 0 0 0) 1
                                        else Map.empty)
                               (\_ i -> if powerOf 2 i
                                        then Map.singleton (ResGen 1 i 0) 
                                             $ (sq[i]) *> (toFModule $ ResGen 0 0 0)
                                        else Map.empty)
                               0
                               t)
                              s t
stableHomotopyResExpandAndSave = resolutionExpandAndSave "stableHom.ext_data" (stableHomotopyResSphere 50 3 10)