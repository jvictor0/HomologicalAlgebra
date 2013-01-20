{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module Resolution where

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

englishLetters :: [String]
englishLetters = map return ['A'..'Z']

letters :: [String]
letters = "i":(englishLetters ++ (concatMap (\ls -> map (ls++) englishLetters) $ tail letters))


class (Graded c) => Complex cData c where
  differential :: cData -> c -> c

class (Complex cData c) => ContractibleComplex cData c where
  contraction :: cData  -> c -> c

--- ResGen for making Free Resolutions of modules
data ResGen = ResGen Int Int Int deriving (Eq,Ord)

instance Show ResGen where
  show (ResGen s t primes) = (letters!!s) ++ "_" ++ (texShow $ t) ++ (take primes $ repeat '\'')

instance Graded ResGen where
  grading (ResGen s t _) = s
  
instance BiGraded ResGen where
  biGrading (ResGen s t _) = (s,t)

type FreeResolution = FreeModule ResGen

data FreeResData a r k = FR {
  largestInternalDegree :: Int,
  connectivity :: Int,
  resolutionLength :: Int,
  differentialMap :: Array (Int,Int) 
                     (Map.Map ResGen (FreeResolution r)),
  resolutionAugmentation :: FreeResData a r k -> Int -> Map.Map ResGen a,
  augmentationSubmodule :: FreeResData a r k -> Int -> Map.Map ResGen (FreeResolution r),
  contractingMatrix :: Array (Int,Int) (Matrix.T k)
  }
                    
                                 
gensUpToMap :: FreeResData a r k -> Int -> Int -> Map.Map ResGen (FreeResolution r)
gensUpToMap cData s t = Map.unions 
                        $ map (\t' -> (differentialMap cData)!(s,t')) 
                        [connectivity cData .. t]

gensUpTo :: FreeResData a r k -> Int -> Int -> [ResGen]
gensUpTo cData s t = Map.keys $ gensUpToMap cData s t


kBasisInDegree :: (Eq s)  => 
                  FreeResData a (FreeModule s k) k -> (Int -> [s]) -> Int -> Int -> Int -> 
                  Array Int (Tensor s ResGen)
kBasisInDegree cData rBasis s t i = listArray (0,length baslst-1) baslst
  where gens = gensUpTo cData s (t-i)
        baslst = concatMap (\g -> map (\s -> Tensor s g) $ rBasis $ t-(internalGrading g)) gens


beginResolution :: (Eq a, Ord r, Ring.C (FreeModule r a), Field.C a) =>
     (Int -> [r])
     -> (FreeResData a1 (FreeModule r a) a -> Int -> Map.Map ResGen a1)
     -> (FreeResData a1 (FreeModule r a) a
         -> Int -> Map.Map ResGen (FreeResolution (FreeModule r a)))
     -> Int
     -> Int
     -> FreeResData a1 (FreeModule r a) a
beginResolution rBasis augmentation augSubmodule conn internalDeg = result
  where result = FR { 
          largestInternalDegree = internalDeg,
          connectivity = conn,
          resolutionLength = 1,
          differentialMap = array ((0,conn),(1,internalDeg)) 
                            $ [((1,t),(augmentationSubmodule result) result t) | t <- [conn..internalDeg]]
                            ++ [((0,t),fmap (const zero) $ (resolutionAugmentation result) result t) | t <- [conn..internalDeg]],
          resolutionAugmentation = augmentation,
          augmentationSubmodule = augSubmodule,
          contractingMatrix  = array ((1,conn),(1,internalDeg)) 
                               [((s,t),MatrixAlgorithms.pseudoInverse $
                                       induceMatrix (kBasisInDegree result rBasis s t 0) (kBasisInDegree result rBasis (s-1) t 0) $ 
                                       let gens_source = gensUpToMap result s (t-1) 
                                       in (\g -> induceStructure $ (maybeZero (Map.lookup g gens_source))))
                                | s <- [1], t <- [conn..internalDeg]]
          }

extendResolution :: (Field.C k, Ring.C (FreeModule s k), Ord s, Eq k, Show k, Show s)
                    => (Int -> [s]) -> FreeResData a (FreeModule s k) k -> Int -> Int -> FreeResData a (FreeModule s k) k
extendResolution rBasis old_res newLen newLID = result
  where ((0,conn),(oldLen,oldLID)) = bounds $ differentialMap old_res
        newDiffs =           
          [((s,t),((differentialMap old_res)!(s,t)))
          | s <- [0..oldLen], t <- [conn..oldLID]] ++ 
          [((s,t),newGensAndContract s t)
          | s <- [oldLen+1..newLen], t <- [oldLID+1..newLID]] ++ 
                    [((s,t),newGensAndContract s t)
          | s <- [2..oldLen], t <- [oldLID+1..newLID]] ++ 
          [((s,t),newGensAndContract s t)
          | s <- [oldLen+1..newLen], t <- [conn..oldLID]] 
        result =  FR {
          largestInternalDegree = newLID,
          connectivity = conn,
          resolutionLength = newLen,
          differentialMap = array ((0,conn),(newLen,newLID)) 
                            $ newDiffs
                            ++ [((1,t),(augmentationSubmodule result) result t) | t <- [oldLID+1..newLID]]
                            ++ [((0,t),fmap (const zero) $ (resolutionAugmentation result) result t) | t <- [oldLID+1..newLID]],
          resolutionAugmentation = resolutionAugmentation old_res,
          augmentationSubmodule = augmentationSubmodule old_res,
          contractingMatrix  = array ((1,conn),(newLen,newLID)) 
                               [((s,t),if inRange ((1,conn),(oldLen,oldLID)) (s,t)
                                       then (contractingMatrix old_res)!(s,t)
                                       else MatrixAlgorithms.pseudoInverse $
                                                    induceMatrix (kBasisInDegree result rBasis s t 0) (kBasisInDegree result rBasis (s-1) t 0) $ 
                                                    let gens_source = gensUpToMap result s (t-1) 
                                                    in (\g -> induceStructure $ (maybeZero (Map.lookup g gens_source))))
                                | s <- [1..newLen], t <- [conn..newLID]]
          }
        newGensAndContract s t = Map.fromList $ zip (map (ResGen s t) [0..]) 
                                 $ map (reduceStructure.(recompose target_basis)) targs
          where target_basis = kBasisInDegree result rBasis (s-1) t 1
                target_next_basis = kBasisInDegree result rBasis (s-2) t 0
                source_basis = kBasisInDegree result rBasis s t 1
                gens_source = gensUpToMap result s (t-1)
                gens_target = gensUpToMap result (s-1) (t-1)
                source_matrix = induceMatrix source_basis target_basis 
                                (\g -> induceStructure $ (maybeZero (Map.lookup g gens_source)))
                target_matrix = induceMatrix target_basis target_next_basis 
                                (\g -> induceStructure $ (maybeZero (Map.lookup g gens_target)))
                im = MatrixAlgorithms.image source_matrix
                rk = Matrix.numColumns im
                ker = MatrixAlgorithms.kernel target_matrix
                nul = Matrix.numColumns ker
                targs = trace ((show (s,t)) ++ "\n" ++ (show target_basis) ++ "\n" ++
                               (show target_next_basis) ++ "\n" ++ 
                               (Matrix.format target_matrix)) 
                        $ take (nul-rk) 
                        $ filter (\v -> not $ v `MatrixAlgorithms.inSubspace` im) 
                        $ map (MatrixUtils.column ker) [0..]
                

allGeneratorsMap resolution = Map.unions $ map (\s -> gensUpToMap resolution s (largestInternalDegree resolution)) [0..resolutionLength resolution]