{-# LANGUAGE  NoImplicitPrelude, RebindableSyntax #-}
module GradedSubspaceAvoidance where

import NumericPrelude 
import Data.List hiding (sum,product)
import Control.Monad.ST
import Data.Array.ST hiding (unsafeFreeze)
import Control.Monad
import Data.STRef
import Debug.Trace
import Data.Maybe
import Utils
import qualified MathObj.Matrix as Matrix
import Data.Array
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Field as Field
import qualified Algebra.IntegralDomain as Domain
import qualified Number.Ratio as Ratio
import qualified Algebra.Units as Units
import qualified Algebra.Module as Module
import MatrixConversions
import qualified MatrixUtils                              
import Data.Array.Unsafe
import qualified PermutationAlgorithms as PermAlgs
import qualified Algebra.ZeroTestable as ZeroTestable
import Rand
import qualified MatrixUtils
import qualified MatrixAlgorithms
import System.Random
import qualified Data.Map as Map


data GSPAProblem r = GSPAP {
  avoidanceImage :: Array Int (Matrix.T r),
  algebraGenerators :: Array (Int,Int) [Matrix.T r]
  }
  
largestDimension prob = snd $ bounds $ avoidanceImage prob
                     
randomComplement :: (Field.C r, MonadRandom m, Random r, Eq r) => Matrix.T r -> Matrix.T r -> m (Matrix.T r)
randomComplement v l = do
  attempt <- MatrixUtils.randomMatrix (Matrix.numColumns v) (Matrix.numRows v)
  if l `MatrixAlgorithms.intersects` (v*attempt)
    then randomComplement v l
    else return attempt
           
         
gradedSubspaceAvoidancePure :: (Field.C r, MonadRandom m, Random r, Eq r)
                           => GSPAProblem r -> m (Array Int (Matrix.T r))
gradedSubspaceAvoidancePure prob = do
  let ld = largestDimension prob
  kn <- randomComplement (Matrix.one $ Matrix.numRows $ (avoidanceImage prob)!ld) 
        $ (avoidanceImage prob)!ld
  subspaceMap <- foldM (\mp i -> do
                           let m = MatrixAlgorithms.intersection $ concatMap 
                                   (\j -> map (\mjl -> MatrixAlgorithms.subspacePreimage mjl $ fromJust $ Map.lookup (i+j) mp) $ (algebraGenerators prob)!(i,j))
                                   [i+1..(largestDimension prob)]
                           k <- randomComplement m $ (avoidanceImage prob)!i
                           return $ Map.insert i k mp)
                 (Map.singleton ld kn)
                 [ld-1,ld-2..0]
  return $ array (0,ld) $ Map.toList subspaceMap

gradedSubspaceAvoidance :: (Field.C r, MonadRandom m, Random r, Eq r, Module.C r md) =>
                           Array Int [md] -> Array Int [md] -> m (Array Int [md])
gradedSubspaceAvoidance gBasis avoidSpace = undefined

