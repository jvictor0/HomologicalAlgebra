{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module MatrixAlgorithms where

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
import qualified MathObj.Polynomial as Poly
import qualified Number.Ratio as Ratio
import qualified Algebra.Units as Units
import MatrixConversions
import qualified MatrixUtils                              
import Data.Array.Unsafe
import qualified PermutationAlgorithms as PermAlgs
import qualified Algebra.ZeroTestable as ZeroTestable
import qualified Data.Map as Map


swapRowsMA arr i j = do
  ((0,0),(m,n)) <-  getBounds arr
  forM_ [0..n] $ \k -> do
    atI <- readArray arr (i,k)
    atJ <- readArray arr (j,k)
    writeArray arr (j,k) atI
    writeArray arr (i,k) atJ

swapColumnsMA arr i j = do
  ((0,0),(m,n)) <- getBounds arr
  forM_ [0..m] $ \k -> do
    atI <- readArray arr (k,i)
    atJ <- readArray arr (k,j)
    writeArray arr (k,j) atI
    writeArray arr (k,i) atJ

clearRowMA arr from to lead = do
  ((0,0),(m,n)) <-  getBounds arr
  tolead <- readArray arr (to,lead)
  forM_ [lead..n] $ \i -> do
    num <- readArray arr (from,i)
    old <- readArray arr (to,i)
    writeArray arr (to,i) $ old - num*tolead

mapRowMA f arr i = do
   ((0,0),(m,n)) <- getBounds arr
   forM_ [0..n] $ \k -> do
     modifyArray arr (i,k) f

                                                
rref mat = runSTMatrix mat $ \arr -> do
  ((0,0),(m,n)) <- getBounds arr
  numZeroCols <- newSTRef 0
  forM_ [0..n] $ \i -> do
    nzc <- readSTRef numZeroCols
    b <- findM (\j -> fmap (/=zero) $ readArray arr (j,i)) $  [i-nzc..m]
    case b of
      Nothing ->  modifySTRef numZeroCols (+1)
      (Just j) -> do
        when (j/= i-nzc) $ swapRowsMA arr (i-nzc) j
        leadTerm <- readArray arr (i-nzc,i)
        mapRowMA (/leadTerm) arr (i-nzc)
        forM_ ([0..m]\\[i-nzc]) $ \k -> do
          target <- readArray arr (k,i)
          when (target/=zero) $ clearRowMA arr (i-nzc) k i
              
inverse mat 
  | Matrix.numColumns mat == Matrix.numRows mat = maybeIf (one == Matrix.index mat (n-1) (n-1))
                                                  $ MatrixUtils.subMatrix mat (n,2*n-1) (n,2*n-1)
 where n = Matrix.numRows mat 
       augmentedMatrix = MatrixUtils.concat (Matrix.one n) mat
       rrefAugMat = rref augmentedMatrix
           
inverseTest mat = case inverse mat of
  Nothing -> True
  (Just inv) -> mat*inv == Matrix.one (Matrix.numRows mat)
       
rankDecomposition mat = (rankFactor, nullFactor)
  where (m,n) = (Matrix.numRows mat, Matrix.numColumns mat)
        rrefmat = rref mat
        pivots = mapMaybe (\i -> find (\j -> zero /=(Matrix.index rrefmat i j)) [0..n-1]) [0..m-1]
        firstZeroRow = find (\i -> all (==zero) $ map (\j -> Matrix.index rrefmat i j) [0..n-1]) [0..m-1]
        rank = length pivots
        rankFactor = Matrix.fromColumns m rank $ map (\p -> map (\j -> Matrix.index mat j p) [0..m-1]) pivots
        nullFactor = case firstZeroRow of
          Nothing -> rrefmat
          (Just firstZRow) -> Matrix.fromRows rank n $ map (\i -> map (\j -> Matrix.index rrefmat i j)[0..n-1]) [0..firstZRow-1]
          
rankDecompositionTest mat = let (c,f) = rankDecomposition mat in c*f==mat

image mat = fst $ rankDecomposition mat
rank mat = Matrix.numColumns $ image mat

kernel mat = Matrix.fromColumns n nullity result
  where f = rref mat
        (m,n) = (Matrix.numRows f, Matrix.numColumns f)
        pivots = Map.fromList $ mapMaybe (\i -> fmap (flip (,)i) $ find (\j -> zero /=(Matrix.index f i j)) [0..n-1]) [0..m-1]
        nonpivots = [0..n-1]\\(Map.keys pivots)
        nullity = length nonpivots
        result = map (\j -> map (\i -> if i == j 
                                       then 1
                                       else case Map.lookup i pivots of
                                         Nothing -> 0
                                         (Just ip) -> -(Matrix.index f ip j))
                            [0..n-1])
                 nonpivots
                       

-- subspace must be a BASIS, else this will not quite work.
-- will return basis for the matrix^{-1}(subspace)
-- this of course includes matrix^{-1}(0) = ker(matrix)
subspacePreimage matrix subspace = MatrixUtils.subMatrix k
                                   (0,0) (Matrix.numColumns matrix-1,Matrix.numColumns k-1)
  where k = kernel $ MatrixUtils.concat matrix subspace

preimages :: (Field.C r, Eq r) => Matrix.T r -> Matrix.T r -> [Matrix.T r]
preimages matrix vector 
  | (Matrix.numRows matrix == Matrix.numRows vector) && (Matrix.numColumns vector == 1) = 
    map (\i -> MatrixUtils.subMatrix fullKernel (0,i) (n-1,i)) 
                          $ filter (\i -> (Matrix.index fullKernel n i) /= 0) [0..k-1]
  | otherwise = error $ "preimages: vector size = " ++ (show $ Matrix.dimension vector) ++ " and matrix size = " ++ (show $ Matrix.dimension matrix)
  where fullKernel = kernel $ MatrixUtils.concat matrix (MatrixUtils.matrixNegate $ vector)
        n = Matrix.numColumns matrix
        k = Matrix.numColumns fullKernel

inSubspace vector matrix = not $ null $ preimages matrix vector

-- when doing types, remember to make this take a field!!
pseudoInverse mat = cT*(fromJust $ inverse $ c*cT)*(fromJust $ inverse $ fT * f)*fT
  where (c,f) = rankDecomposition mat
        (cT,fT) = (Matrix.transpose c,Matrix.transpose f)


lu :: (Field.C a) =>  Matrix.T a ->(Matrix.T a,Matrix.T a)
lu mat = (fromArray l,fromArray u)
  where (m,n) = (Matrix.numRows mat,Matrix.numColumns mat) 
        u = array ((0,0),(m-1,n-1)) $
            [((i,j),zero) | i <- [0..m-1], j <- [0..i-1]] ++
            [((i,j),
              (Matrix.index mat i j) - (sum $ map (\k -> (l!(i,k))*(u!(k,j))) [0..i-1]))
            | i <- [0..m-1], j <- [i..n-1]]
        l = array ((0,0),(m-1,n-1)) $ 
            [((i,j),zero) | i <- [0..m-1], j <- [i+1..n-1]] ++
            [((i,i),one)  | i <- [0..m-1]] ++ 
            [((j,i),
              ((Matrix.index mat j i) - 
               (sum $ map (\k -> (l!(j,k))*(u!(k,i))) [0..i-1]))
              / (u!(i,i)))
            | i <- [0..m-1], j <- [i+1..n-1]]

lupTest mat = let (l,u,p) = lup mat in l*u == p*mat

type LUMatrix a = Matrix.T a

lupCompact :: (Eq a,Field.C a) =>  Matrix.T a -> (Matrix.T a,Array Int Int)
lupCompact mat = runST $ do
  let n = Matrix.numRows mat
  lu <- thawMatrix mat
  v <- thaw $ listArray (0,n-1) [0..n-1] :: ST s (STArray s Int Int)
  forM_ [0 .. n-1] $ \k -> do 
    pivot <- findM (\p -> do
                       vp <- readArray v p
                       fmap (/=zero) $ readArray lu (vp,k))
             [k..n-1]
    case pivot of         
      Nothing -> return ()
      (Just p) -> do
        when (p>k) $ do
          temp <- readArray v k
          writeArray v k p
          writeArray v p temp
    vk <- readArray v k
    lukk <- readArray lu (vk,k)
    forM_ [k+1..n-1] $ \j -> do
      vj <- readArray v j
      lu_vj_k <- readArray lu (vj,k)
      let s = if lu_vj_k == 0 then 0 else -lu_vj_k/lukk
      forM_ [k+1..n-1] $ \c -> do
        lu_vk_c <- readArray lu (vk,c)
        modifyArray lu (vj,c) (+(s*lu_vk_c))
      writeArray lu (vj,k) (-s)
  vf <- unsafeFreeze v    
  luf <- unsafeFreeze lu
  return (fromArray luf,vf)
  
lup :: (Eq a, Field.C a) => Matrix.T a -> (Matrix.T a,Matrix.T a, Matrix.T a)
lup mat = let (luMat,perm) = lupCompact mat 
              n = Matrix.numRows mat
          in (fromArray $ array ((0,0),(n-1,n-1))
              [((i,j),if i == j then 1 else if i < j then 0 else Matrix.index luMat i j)
               | i <- [0..n-1], j <- [0..n-1]],
              fromArray $ array ((0,0),(n-1,n-1))
              [((i,j),if i > j then 0 else Matrix.index luMat i j)
              | i <- [0..n-1], j <- [0..n-1]],
              fromArray $ array ((0,0),(n-1,n-1))
              [((i,j),if (perm!i) == j then 1 else 0)
              | i <- [0..n-1], j <- [0..n-1]])

determinant mat = let (lu,v) = lupCompact $ mat 
                  in (2*(PermAlgs.parity v)-1)*
                     (product 
                      $ map (\i -> Matrix.index lu i i) [0..Matrix.numRows mat -1])
                     
determinantPID mat = Ratio.numerator $ determinant $ fmap (Ratio.%1) mat


characteristicPolynomial :: (Eq a, Field.C a,ZeroTestable.C a) 
                            => Matrix.T a -> Poly.T a
characteristicPolynomial mat = determinantPID $ (fmap Poly.const mat) - lambdaI
  where n = Matrix.numColumns mat
        lambda = Poly.fromCoeffs [zero,one] 
        lambdaI = Matrix.scale lambda (Matrix.one n) 


intersection matlst = image $ MatrixUtils.concats matlst
v `intersects` w = (Matrix.numColumns $ intersection [v,w]) > 0

wikipediaMatrix = Matrix.fromColumns 5 4 [[1,2,1,1],[3,7,5,2],[1,3,3,0],[4,9,1,8]] :: Matrix.T (Ratio.T Integer)

smallMat = Matrix.fromColumns 2 2 [[4,6],[3,3]] :: Matrix.T (Ratio.T Integer)

