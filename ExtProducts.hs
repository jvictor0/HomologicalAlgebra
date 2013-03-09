{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module ExtProducts where


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
import qualified MatrixAlgorithms
import qualified MatrixUtils
import Tensor
import Debug.Trace
import Algebra
import Data.Char
import Data.Maybe
import Control.Monad.ST
import Control.Monad
import Data.Array.ST
import qualified Subalgebra as SA
import Data.STRef
import Data.Array.IO
import Data.IORef
import Resolution


extendToChainMap :: (Eq k, Ord s, Ring.C k, AlgebraGenerator s k)
                 => FreeResData m s k -> FreeResData k s k -> ResGen -> Map.Map ResGen (FreeModule ResGen (FreeModule s k))
extendToChainMap resM resk g = result 
  where result = Map.fromList $ (g,toFModule $ ResGen 0 0 0)
                 :(concat [gChainMap s t 
                          | s <- [1..(resolutionLength resM)-(grading g)], t <- [connectivity resM..(largestInternalDegree resM) - (internalGrading g)]])
        gChainMap s t = let im = reducibleImageAt resk s t
                        in map (\(h,dh) -> case gradedSummand $ SA.reduce (smapMaybe (flip Map.lookup result) dh) im of
                                   [] -> (h,zero)
                                   gs -> (h,last gs))
                           $ Map.toList $ (differentialMap resM)!(s+(grading g),t+(internalGrading g))

extendToChainMaps resM resk = Map.unions $ elems $ fmap (Map.mapWithKey (\g _ -> extendToChainMap resM resk g)) $ differentialMap resM
                              
generatorMultiply :: (Eq k, Ring.C k, Ord s, AlgebraGenerator s k) 
                     => FreeResData m s k -> Map.Map ResGen (Map.Map ResGen (FreeModule ResGen (FreeModule s k))) -> ResGen -> ResGen -> Maybe (FreeModule ResGen k)
generatorMultiply res chains x y = do
  chainx <- Map.lookup x chains
  let st = (grading x + grading y, internalGrading x + internalGrading y) 
  guard $ inRange (bounds (differentialMap res)) st
  let targets = Map.keys $ (differentialMap res)!st
      one_y = Tensor (fst $ fromFModule (idt`asCoefOf`(snd $ head $ Map.toList chainx))) y --ahhh!!!!!
  return $ fromAList $ map (\t -> (t,coefOf (induceStructure $ maybeZero $ Map.lookup t chainx) $ one_y)) targets
  
multiplicationTable resM resk = Map.fromList [((x,y),generatorMultiply resM chains x y)
                                             | x <- concatMap Map.keys $ elems $ differentialMap resM,
                                               y <- concatMap Map.keys $ elems $ differentialMap resk]
  where chains = extendToChainMaps resM resk