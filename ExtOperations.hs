{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module ExtOperations where

import NumericPrelude
import Resolution
import Tensor
import FreeModule
import qualified Algebra.Ring as Ring
import Algebra
import GradedObject
import Data.Array
import GradedSubspaceAvoidance
import qualified MatrixAlgorithms
import qualified MatrixUtils
import qualified MathObj.Matrix as Matrix
import qualified Algebra.Field as Field
import System.Random
import ResolutionSaver
import qualified Subalgebra as SA
import qualified Data.Set as Set
import SteenrodAlgebra
import System.IO.Unsafe
import ZMod2
import ToCNF
import Recursive 
import ToCNF
import Control.Monad.ST
import Control.Monad


-- we wish to take a resolution and make a subspace avoidnace problem
makeGradedSubspaceAvoidanceProblem :: (Eq k, Field.C k, AlgebraGenerator s k, Show k)
                                      => FreeResData a s k -> (Int -> [FreeModule s k]) -> Int -> Int -> GSPAProblem k
makeGradedSubspaceAvoidanceProblem resolution alg_gens s lid = GSPAP {
  avoidanceImage = array (connectivity resolution,lid) [(t,imageAt t) | t <- [connectivity resolution..lid]],
  algebraGenerators =  array ((connectivity resolution,connectivity resolution),(lid,lid)) 
                       [((t,t'),multMapAt t t') 
                       | t <- [connectivity resolution..lid],
                         t' <- [connectivity resolution..lid]]
  }
  where imageAt t = MatrixAlgorithms.image $ matrixAt resolution (s+1) t
        multMapAt t t' = map (\sq -> linearMatrix (kBasisInDegree resolution s t 0) (kBasisInDegree resolution s t' 0) (sq*>))
                         $ alg_gens $ t' - t  

makeAlignedGradedSubspaceAvoidanceProblem :: (Eq k, Field.C k, AlgebraGenerator s k, Show k, Ord k, Show s)
                                             => FreeResData a s k -> (Int -> [FreeModule s k]) -> Int -> Int -> GSPAProblem k
makeAlignedGradedSubspaceAvoidanceProblem resolution alg_gens s lid = GSPAP { avoidanceImage = avims, algebraGenerators = algMats}
  where bases = listArray (0,lid) $ flip map [connectivity resolution..lid] $ \t -> 
          let imag = SA.fromList $ map (\b -> differential resolution $ reduceStructure $ (toFModule b)`withCoefOf`(head $ alg_gens 0))
                     $ elems $ kBasisInDegree resolution (s+1) t 0 
          in (SA.toList imag, SA.fillOutSpace imag $ elems $ kBasisInDegree resolution s t 0)
        algMats = listArray ((0,0),(lid,lid)) $ flip map [(t,t') | t <- [0..lid], t' <- [0..lid]] $ \(t,t') -> 
          flip map (alg_gens $ t'-t) $ \sq ->
            let (iset,ss) = bases!t
                (iset',ss') = bases!t' 
              in SA.toMatrix ss ss' (sq*>)
        avims = listArray (0,lid) $ flip map [0..lid] $ \t -> 
          Matrix.fromColumns (SA.size $ snd $ bases!t) (length $ fst $ bases!t) $
          flip map (fst $ bases!t) $ \ b -> 
          map (\c -> if c == b then 1 else 0) (SA.toList $ snd $ bases!t)
        

myRes = unsafePerformIO $ loadE2Page 20
myProb = unsafePerformIO $ do
  res <- loadE2Page 20
  return $ makeGradedSubspaceAvoidanceProblem res squaresDim 2 8

findSubspaceAwayFromDifferential :: (Ord s, AlgebraGenerator s ZMod2) => 
     FreeResData a s ZMod2
     -> (Int -> [FreeModule s ZMod2])
     -> Int
     -> Int
     -> IO (Array Int [FreeModule (Tensor s ResGen) ZMod2])
findSubspaceAwayFromDifferential resolution alg_gens s lid = do
  subspaces <- gradedSubspaceAvoidanceSAT $ makeGradedSubspaceAvoidanceProblem resolution alg_gens s lid
  return $ listArray (bounds subspaces) $ 
    map (\(t,vs) -> map (\i -> recompose (kBasisInDegree resolution s t 0) $ MatrixUtils.column vs i) [0..Matrix.numColumns vs -1]) 
    $ assocs subspaces
    
squaresDim n = if n < 1 then [] else [sq [n]]
doit s t = do
  res <- loadE2Page 20
  findSubspaceAwayFromDifferential res squaresDim s t
  
