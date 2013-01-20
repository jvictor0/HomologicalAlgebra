{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes #-}
module MatrixConversions where

import NumericPrelude 
import qualified MathObj.Matrix as Matrix
import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Data.Array.ST
import Control.Monad.ST

-- thaws a Matrix to an STArray for matrix algorithms.  
--thawMatrix :: (MArray u a m) => Matrix.T a -> m (u (Int,Int) a)
thawMatrix :: Matrix.T a -> ST s (STArray s (Int,Int) a)
thawMatrix matrix = thaw $ toArray matrix

toArray matrix = array ((0,0),(Matrix.numRows matrix-1,Matrix.numColumns matrix-1))
                 [((i,j),Matrix.index matrix i j)
                 | i <- [0..(Matrix.numRows matrix)-1],
                   j <- [0..(Matrix.numColumns matrix)-1]]

fromArray matrix =  Matrix.fromList (m+1) (n+1) $ elems matrix
  where ((0,0),(m,n)) = bounds matrix

runSTMatrix :: Matrix.T a -> (forall s . STArray s (Int, Int) a -> ST s b) -> Matrix.T a
runSTMatrix startMat matFun = fromArray $ runSTArray $ do
  stmat <- thawMatrix startMat
  matFun stmat
  return stmat
