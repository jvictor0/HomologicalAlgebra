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
import PropFormula
import Recursive
import ToCNF
import SAT
import qualified Data.Set as Set
import ZMod2

data GSPAProblem r = GSPAP {
  avoidanceImage :: Array Int (Matrix.T r),
  algebraGenerators :: Array (Int,Int) [Matrix.T r]
  } deriving (Show)
  
largestDimension prob = snd $ bounds $ avoidanceImage prob
                     
randomComplement :: (Field.C r, Functor m,MonadRandom m, Random r, Eq r, Show r) => Matrix.T r -> Matrix.T r -> m (Matrix.T r)
randomComplement v l =  do
  attempt <- fmap (v*) $ MatrixAlgorithms.randomMatrixFullRank (Matrix.numColumns v) ((Matrix.numColumns v)-(Matrix.numColumns l))
  if l `MatrixAlgorithms.intersects` attempt
    then  randomComplement v l
    else return attempt
           
extendToRandomBasis l = do
  comp <- randomComplement (Matrix.one $ Matrix.numRows l) l
  return $ MatrixUtils.concat comp l
  
alignProblem prob = do
  newBases <- fmap (listArray (bounds $ avoidanceImage prob)) $
              mapM extendToRandomBasis $ elems $ avoidanceImage prob 
  print newBases
  let newInverse = fmap ((safeFromJust "alignProblem: it seems extendToRandom returned noninvertible matrix" ) . MatrixAlgorithms.inverse) newBases
  print newInverse
  let newAlgGens = array (bounds $ algebraGenerators prob) $
                   map (\((i,j),gs) -> 
                         ((i,j),map (\g -> (newInverse!j)*g*(newBases!i)) gs))
                   $ assocs $ algebraGenerators prob
      newImages  = fmap (\old_b -> MatrixUtils.stack (Matrix.zero (Matrix.numRows old_b - Matrix.numColumns old_b) (Matrix.numColumns old_b))
                                   (Matrix.one (Matrix.numColumns old_b)))
                   $ avoidanceImage prob
  return $ (newBases,GSPAP {avoidanceImage = newImages, algebraGenerators = newAlgGens})
               
gradedSubspaceAvoidancePure :: (Field.C r, MonadRandom m, Random r, Eq r, Show r, Functor m)
                           => GSPAProblem r -> m (Array Int (Matrix.T r))
gradedSubspaceAvoidancePure prob = do
  let ld = largestDimension prob
  kn <- randomComplement (Matrix.one $ Matrix.numRows $ (avoidanceImage prob)!ld) 
        $ (avoidanceImage prob)!ld
  subspaceMap <- foldM (\mp i -> do
                           let m = MatrixAlgorithms.intersections $ concatMap 
                                   (\j -> map (\mjl -> MatrixAlgorithms.subspacePreimage mjl 
                                                       $ fromJust  $ Map.lookup j mp) $ (algebraGenerators prob)!(i,j))
                                   [i+1..(largestDimension prob)]
                           k <- randomComplement m $ (avoidanceImage prob)!i
                           return $ Map.insert i k mp)
                 (Map.singleton ld kn)
                 [ld-1,ld-2..0]
  return $ array (0,ld) $ Map.toList subspaceMap

gradedSubspaceAvoidance :: (Field.C r, MonadRandom m, Random r, Eq r, Module.C r md) =>
                           Array Int [md] -> Array Int [md] -> m (Array Int [md])
gradedSubspaceAvoidance gBasis avoidSpace = undefined



data GSPAPBVTag = GSPAP_K Int (Int,Int) | 
                  GSPAP_W (Int,Int,Int) (Int,Int) | 
                  GSPAP_X Int |
                  GSPAP_H Int Int deriving (Eq,Ord)

instance Freshable GSPAPBVTag where
  fresh i = GSPAP_X i

instance Show GSPAPBVTag where
  show (GSPAP_K i (j,k)) = "(K_" ++ (texShow i) ++ ")_{" ++ (show j) ++ "," ++ (show k) ++ "}"
  show (GSPAP_W (i,p,l) (j,k)) = "(W_{" ++ (tail $ init $ show (i,p,l)) ++ "})_{" ++ (show j) ++ "," ++ (show k) ++ "}"
  show (GSPAP_X i) = "x_" ++ (texShow i)
  show (GSPAP_H k i) = "h_{" ++ show k ++ "," ++ show i ++ "}"
  
gspapWBitVar :: (Int,Int,Int) -> (Int,Int) -> (PropFormula GSPAPBVTag Bool ())
gspapWBitVar (k,p,l) (i,j) = wrapData $ (BitVar $ GSPAP_W (k,p,l) (i,j) :: PFData GSPAPBVTag Bool ())
gspapKBitVar :: Int -> (Int,Int) -> (PropFormula GSPAPBVTag Bool ())
gspapKBitVar n (i,j) = wrapData $ (BitVar $ GSPAP_K n (i,j) :: PFData GSPAPBVTag Bool ())
gspapXBitVar :: Int -> (PropFormula GSPAPBVTag Bool ())
gspapXBitVar n = wrapData $ (BitVar $ GSPAP_X n :: PFData GSPAPBVTag Bool ())
gspapHBitVar :: Int -> Int -> (PropFormula GSPAPBVTag Bool ())
gspapHBitVar n i = wrapData $ (BitVar $ GSPAP_H n i :: PFData GSPAPBVTag Bool ())
  

makeKMatrix :: Int -> Int -> Int -> Matrix.T (PropFormula GSPAPBVTag Bool ())
makeKMatrix matNum n h = Matrix.fromColumns n h $
                map (\j -> map (\i -> if j > i then 0 else gspapKBitVar matNum (i,j))
                           [0..n-1]) [0..h-1]
                
makeWMatrix :: (Int,Int,Int) -> Int -> Int -> Matrix.T (PropFormula GSPAPBVTag Bool ())
makeWMatrix (k,p,l) hi hj = Matrix.fromColumns hi hj $
                       map (\j -> map (\i -> gspapWBitVar (k,p,l) (i,j))
                                  [0..hi-1]) [0..hj-1]


hopefulDimensions prob = fmap (\m -> Matrix.numRows m - Matrix.numColumns m) $ avoidanceImage prob

makeSubmoduleConstraint (h,prob) = pfAnd $ map pfNot $ concatMap toList matList 
  where matList = concatMap 
                  (\((i,j),ms) ->
                    zipWith 
                    (\l m -> 
                      ((fmap fromZMod2 m)*(makeKMatrix i (Matrix.numColumns m) (h!i)))
                      - ((makeKMatrix j (Matrix.numRows m) (h!j))*(makeWMatrix (i,j,l) (h!j) (h!i))))
                    [1..] ms)
                  $ assocs $ algebraGenerators prob

makeUpperTriangularConstraintsAt k i j hk = pfOr $ (pfAnd [pfNot $ gspapKBitVar k (i,j') | j' <- [j+1..minimum [i,hk-1]]]):
                                            [gspapKBitVar k (i',j) | i' <- [j..i-1]]

makeUpperTriangularConstraints (h,prob) = pfAnd $ [makeUpperTriangularConstraintsAt k i j hk
                                                   | (k,hk) <- assocs h, 
                                                     i <- [0..Matrix.numRows ((avoidanceImage prob)!k) - Matrix.numColumns ((avoidanceImage prob)!k)-1],
                                                     j <- [0..minimum[i,hk-1]]]

makeNonIntersectionConstraints (h,prob) = pfAnd [pfOr $ (pfNot $ gspapHBitVar k j):
                                                 [gspapKBitVar k (i,j)
                                                 | i <- [j..Matrix.numRows ((avoidanceImage prob)!k) - Matrix.numColumns ((avoidanceImage prob)!k)-1]]
                                                | (k,hk) <- assocs h, j <- [0..hk-1]]
                                          
makeExtraZeroConstraints (h,prob) = pfAnd [(gspapHBitVar k j) ||| (pfNot $ gspapKBitVar k (i,j))
                                           | (k,hk) <- assocs h, j <- [0..hk-1],
                                             i <- [j..Matrix.numRows ((avoidanceImage prob)!k)-1]]
                                                                              
allGSPAPConstraints prob = (pfAnd [gspapHBitVar h j | (h,hk) <- assocs h, j <- [0..hk-1]],
                            pfAnd [makeUpperTriangularConstraints (h,prob),makeSubmoduleConstraint (h,prob),
                                   makeNonIntersectionConstraints (h,prob),makeExtraZeroConstraints (h,prob)])
  where h = hopefulDimensions prob

gradedSubspaceAvoidanceSAT prob = do
  putStrLn "Aligning Problem"
  (newBasis,newProb) <- alignProblem prob
  putStrLn "Generating Constraints"
  let (softs,constrs) = allGSPAPConstraints newProb
  let cnf_consts = toCNF constrs
  putStrLn $ "there are " ++ (show $ length $ getChildren cnf_consts) ++ " hard CNF clauses"
  putStrLn $ "converting to DIMACS"
  let (numMap,(soft,hard)) = toPMAXDIMACS (softs,cnf_consts)
  putStrLn $ "there are " ++ (show $ Map.size numMap) ++ " SAT variables"
  putStrLn "Running QMaxSat"
  result <-  solvePMAX_SAT2 (toPMAXDIMACSString numMap (soft,hard))
  putStrLn "Interperiting Results"
  case result of
    Nothing -> error "satSolveGSPAPProblem: wtf"
    (Just res) -> do
      let opts = filter (\x -> let (Just tm) = Map.lookup x numMap in Set.member tm res) $ getChildren softs
      let kmats = map (\(k,m) -> fmap (\j -> maybeZero $ fmap (ZMod2.(flip Set.member res)) $ Map.lookup j numMap)
                                 $ makeKMatrix k (Matrix.numRows m)
                                 (Matrix.numRows m - Matrix.numColumns m)) 
                  $ assocs $ avoidanceImage prob
      forM_ (zip ([0..]::[Int]) kmats) $ \(i,k) -> (print i) >> (putStrLn $Matrix.format k)
      putStrLn "These conditions are satisfied"
      mapM_ print opts
      putStrLn $ "dim_k(K_*) = " ++ (show $ length opts)
      return $ listArray (bounds newBasis) $ zipWith (*) (elems newBasis) kmats
  