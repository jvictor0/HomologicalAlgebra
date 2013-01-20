{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes #-}
module MatrixUtils where

import NumericPrelude 
import Data.List hiding (sum)
import Control.Monad.ST
import Debug.Trace
import Data.Maybe
import Utils
import qualified MathObj.Matrix as Matrix
import Data.Array
import MatrixConversions
import Rand

concat mat1 mat2 
  | Matrix.numRows mat1 == Matrix.numRows mat2 =
    let (n1,n2,m) = (Matrix.numColumns mat1,Matrix.numColumns mat2,Matrix.numRows mat1)
    in fromArray $ array ((0,0),(m-1,n1+n2-1)) $ 
       [((i,j),Matrix.index mat1 i j) | i <- [0..m-1], j <- [0..n1-1]] ++
       [((i,j+n1),Matrix.index mat2 i j) | i <- [0..m-1], j <- [0..n2-1]]
  | otherwise = error "concat: matrix dimension mismatch"
       
concats mats = foldr1 MatrixUtils.concat mats
                
stack mat1 mat2
  | Matrix.numColumns mat1 == Matrix.numColumns mat2 =
    let (m1,m2,n) = (Matrix.numRows mat1,Matrix.numRows mat2,Matrix.numColumns mat1)
    in fromArray $ array ((0,0),(m1+m2-1,n-1)) $ 
       [((i,j),Matrix.index mat1 i j) | i <- [0..m1-1], j <- [0..n-1]] ++
       [((i+m1,j),Matrix.index mat2 i j) | i <- [0..m2-1], j <- [0..n-1]]
  | otherwise = error "stack: matrix dimension mismatch"

subMatrix mat (m0,n0) (m,n) = fromArray $ array ((0,0),(m-m0,n-n0))
                              [((i-m0,j-n0),Matrix.index mat i j) 
                               | i <- [m0..m], j <- [n0..n]]
                              
column mat i = subMatrix mat (0,i) (Matrix.numRows mat-1,i)
row mat i = subMatrix mat (i,0) (i,Matrix.numColumns mat-1)

randomMatrix m n = do
  lsts <- mapM (const $ mapM (const getRandom) [1..n]) [1..m]
  return $ Matrix.fromRows m n lsts
                   

matrixNegate mat = (Matrix.zero (Matrix.numRows mat) (Matrix.numColumns mat)) - mat