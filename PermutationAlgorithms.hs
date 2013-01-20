{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes #-}
module PermutationAlgorithms where

import NumericPrelude 
import Data.Array
import qualified MathObj.Permutation.Table as Perm
import qualified Algebra.Ring as Ring
import Data.Array.ST
import Control.Monad.ST
import Data.STRef
import Control.Monad



parity :: (Ring.C a) => Perm.T Int -> a
parity perm = runST $ do
  parity <- newSTRef False
  arr <- thaw perm :: ST s (STArray s Int Int)
  forM_ [fst $ bounds perm..snd $ bounds perm] $ \i -> do
    ai <- readArray arr i
    if i == ai then return () else do
      modifySTRef parity not
      aai <- readArray arr ai
      writeArray arr i aai
      writeArray arr ai i
  res <- readSTRef parity
  return $ if res then 1 else 0
      