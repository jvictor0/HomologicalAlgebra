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

englishLetters :: [String]
englishLetters = map return ['A'..'Z']

letters :: [String]
letters = "i":(englishLetters ++ (concatMap (\ls -> map (ls++) englishLetters) $ tail letters))


class (Graded c) => Complex cData c where
  differential :: cData -> c -> c

class (Complex cData c) => ContractibleComplex cData c where
  contraction :: cData -> c -> c

--- ResGen for making Free Resolutions of modules
data ResGen = ResGen Int Int Int deriving (Eq,Ord)

instance Show ResGen where
  show (ResGen s t primes) = (letters!!s) ++ "_" ++ (texShow $ t) ++ (take primes $ repeat '\'')

instance Read ResGen where
  readsPrec n (' ':rst) = readsPrec n rst
  readsPrec n ss = if and [(all (\c -> c >= 'A' && c <= 'Z') name) || (name == "i"),
                           not $ null num,
                           not $ null $ tail num,
                           ((num!!1) /= '{') || ((not $ null rst0) && ((head rst0)=='}')),
                           all isDigit nm]
                   then [(ResGen (fromJust $ elemIndex name letters) (read nm) (length ps),rst)]
                   else []
    where (name,num) = break (=='_') ss
          (nm,rst0) = break (not.isDigit) $ if num!!1 == '{' then drop 2 num else drop 1 num
          (ps,rst) = break (/='\'') $ if num!!1 == '{' then tail rst0 else rst0
          
instance UnShow ResGen where
  unShow ss = ResGen s (read nm) $ length $ takeWhile (=='\'') $ reverse ss
    where (name,num) = break (=='_') ss
          s =  (fromJust $ elemIndex name letters)
          nm = takeWhile isDigit $ if num!!1 == '{' then drop 2 num else drop 1 num


instance Graded ResGen where
  grading (ResGen s t _) = s
  
instance BiGraded ResGen where
  biGrading (ResGen s t _) = (s,t)

type FreeResolution = FreeModule ResGen

data FreeResData a s k = FR {
  largestInternalDegree :: Int,
  connectivity :: Int,
  kBasisOfRing :: Int -> [s],
  resolutionLength :: Int,
  differentialMap :: Array (Int,Int) 
                     (Map.Map ResGen (FreeResolution (FreeModule s k))),
  resolutionAugmentation :: FreeResData a s k -> Int -> Map.Map ResGen a,
  augmentationSubmodule :: FreeResData a s k -> Int -> Map.Map ResGen (FreeResolution (FreeModule s k)),
  contractingMatrix :: Array (Int,Int) (Matrix.T k)
  }
                    
                                 
gensUpToMap :: FreeResData a s k -> Int -> Int -> Map.Map ResGen (FreeResolution (FreeModule s k))
gensUpToMap cData s t = Map.unions 
                        $ map (\t' -> (differentialMap cData)!(s,t')) 
                        [connectivity cData .. t]

gensUpTo :: FreeResData a s k -> Int -> Int -> [ResGen]
gensUpTo cData s t = Map.keys $ gensUpToMap cData s t


kBasisInDegree :: (Eq s)  => 
                  FreeResData a s k -> Int -> Int -> Int -> 
                  Array Int (Tensor s ResGen)
kBasisInDegree cData s t i = listArray (0,length baslst-1) baslst
  where gens = gensUpTo cData s (t-i)
        baslst = concatMap (\g -> map (\s -> Tensor s g) $ rBasis $ t-(internalGrading g)) gens
        rBasis = kBasisOfRing cData

matrixAt resolution s t = induceMatrix (kBasisInDegree resolution s t 0) (kBasisInDegree resolution (s-1) t 0) $ 
                          let gens_source = gensUpToMap resolution s t 
                          in (\g -> induceStructure $ (maybeZero (Map.lookup g gens_source)))


imageAt res s t = SA.fromList baslst
  where gens = Map.toList $ gensUpToMap res s t
        baslst = concatMap (\(g,dg) -> map (\s -> ((toFModule s)`asCoefOf`dg)*>dg) $ rBasis $ t-(internalGrading g)) gens
        rBasis = kBasisOfRing res

reducibleImageAt res s t = SA.fromList baslst
  where gens = Map.toList $ gensUpToMap res s t
        baslst = concatMap (\(g,dg) -> map (\s -> (fromAList [(g,toFModule s)]) + (((toFModule s)`asCoefOf`dg)*>dg)) $ rBasis $ t-(internalGrading g)) gens
        rBasis = kBasisOfRing res


beginResolution :: (Eq k, Ord r, AlgebraGenerator r k, Field.C k) =>
      (Int -> [r])  
     -> (FreeResData a1 r k -> Int -> Map.Map ResGen a1)
     -> (FreeResData a1 r k
         -> Int -> Map.Map ResGen (FreeResolution (FreeModule r k)))
     -> Int
     -> Int
     -> FreeResData a1 r k
beginResolution rBasis augmentation augSubmodule conn internalDeg = result
  where result = FR { 
          largestInternalDegree = internalDeg,
          connectivity = conn,
          kBasisOfRing = rBasis,
          resolutionLength = 1,
          differentialMap = array ((0,conn),(1,internalDeg)) 
                            $ [((1,t),(augmentationSubmodule result) result t) | t <- [conn..internalDeg]]
                            ++ [((0,t),fmap (const zero) $ (resolutionAugmentation result) result t) | t <- [conn..internalDeg]],
          resolutionAugmentation = augmentation,
          augmentationSubmodule = augSubmodule,
          contractingMatrix  = array ((1,conn),(1,internalDeg)) 
                               [((s,t),MatrixAlgorithms.pseudoInverse $ matrixAt result s t)
                                | s <- [1], t <- [conn..internalDeg]]
          }

brunerResolution :: (Eq k, Ord r, AlgebraGenerator r k, Ring.C k, Show r, Show k) =>
      (Int -> [r])  
     -> (Int -> [FreeResolution (FreeModule r k)])
     -> Int
     -> Int
     -> Int
     -> IO (Array (Int,Int) (Map.Map ResGen (FreeResolution (FreeModule r k))))
brunerResolution rBasis augKernel conn reslength internalDeg = do
  result <- newArray ((1,conn),(reslength,internalDeg)) Map.empty :: IO(IOArray (Int,Int) (Map.Map ResGen (FreeResolution (FreeModule r k))))
  forM [conn..internalDeg] $ \t -> do
    oldKer <- newIORef $ augKernel t
    when True $ do
      res <- freeze result 
      writeFile ("partialExtData" ++ (show t) ++ ".dat")
        $ show $ array ((1,conn),(reslength,t-1))
        [((s',t'),res!(s',t')) | s' <- [1..reslength], t' <- [conn..t-1]]
        putStrLn $ "t = " ++ (show t)
    forM [1..reslength] $ \s -> do
      writeFile "progress.txt" (show (s,t))
      image <- newIORef SA.zeroSpace
      newKer <- newIORef []
      forM [conn..t-1] $ \t' -> do
        gens <- fmap Map.toList $ readArray result (s,t')
        forM gens $ \(g,dg) -> do
          forM (rBasis $ t - t') $ \op -> do
            i <- readIORef image
            let x  = op **> (toFModule g)
                dx = op **> dg
                red = SA.reduce (x+dx) i
            if (isHomogenious red) && ((red == zero) || ((grading red) == s))
              then do
              modifyIORef newKer (red:)
              else do
              modifyIORef image (SA.insert red) 
      cycs <- readIORef  oldKer   
      forM cycs $ \cyc -> do 
        i <- readIORef image
        let dg = SA.reduce cyc i
        when (not $ (isHomogenious dg) && ((dg == zero) || ((grading dg) == s))) $ do
          oldgs <- readArray result (s,t)
          modifyArray result (s,t) (Map.insert (ResGen s t (Map.size oldgs)) dg)
          writeIORef image $ SA.insert dg i
      newOldKer <- readIORef newKer
      writeIORef oldKer newOldKer              
  freeze result    
          
            


extendResolution :: (Field.C k, AlgebraGenerator s k, Ord s, Eq k, Show k, Show s)
                    => FreeResData a s k -> Int -> Int -> FreeResData a s k
extendResolution old_res newLen newLID = result
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
          kBasisOfRing = kBasisOfRing old_res,
          differentialMap = array ((0,conn),(newLen,newLID)) 
                            $ newDiffs
                            ++ [((1,t),(augmentationSubmodule result) result t) | t <- [oldLID+1..newLID]]
                            ++ [((0,t),fmap (const zero) $ (resolutionAugmentation result) result t) | t <- [oldLID+1..newLID]],
          resolutionAugmentation = resolutionAugmentation old_res,
          augmentationSubmodule = augmentationSubmodule old_res,
          contractingMatrix  = array ((1,conn),(newLen,newLID)) 
                               [((s,t),if inRange ((1,conn),(oldLen,oldLID)) (s,t)
                                       then (contractingMatrix old_res)!(s,t)
                                       else MatrixAlgorithms.pseudoInverse $ matrixAt result s t)
                                | s <- [1..newLen], t <- [conn..newLID]]
          }
        newGensAndContract s t = Map.fromList $ zip (map (ResGen s t) [0..]) 
                                 $ map (reduceStructure.(recompose target_basis)) targs
          where target_basis = kBasisInDegree result(s-1) t 1
                target_next_basis = kBasisInDegree result (s-2) t 0
                source_basis = kBasisInDegree result s t 1
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
                targs = take (nul-rk) 
                        $ filter (\v -> not $ v `MatrixAlgorithms.inSubspace` im) 
                        $ map (MatrixUtils.column ker) [0..]
                

allGeneratorsMap resolution = Map.unions $ map (\s -> gensUpToMap resolution s (largestInternalDegree resolution)) [0..resolutionLength resolution]

instance (Eq s, Eq k, Ring.C (FreeModule s k)) 
         => Complex (FreeResData a s k) (FreeResolution (FreeModule s k)) where
  differential resolution v = smapMaybe (\g -> Map.lookup g
                                               $ (differentialMap resolution)!(biGrading g)) v
                              
instance (Eq s, Ord s, Ring.C k, Eq k, Ring.C (FreeModule s k)) 
         => ContractibleComplex (FreeResData a s k) (FreeResolution (FreeModule s k)) where
  contraction resolution v = NumericPrelude.sum 
                             $ map (\vg -> let (s,t) = biGrading vg in
                                         reduceStructure 
                                         $ recompose (kBasisInDegree resolution (s+1) t 0)
                                         $ (((contractingMatrix resolution)!(s+1,t))*)
                                         $ decompose (kBasisInDegree resolution s t 0)
                                         $ induceStructure vg)
                             $ biGradedSummand v
                             
type FreeTensorResolution s k = FreekTensor ResGen ResGen s s k

instance (Eq s, Eq k, AlgebraGenerator s k, Ord s) 
         => Complex (FreeResData a s k) (FreeTensorResolution s k) where
  differential resolution = ((differential resolution)`ten_k`id) + (id`ten_k`(differential resolution))

-- this is not quite right unless it is when a = k, annoyingly.  
-- TODO: make this work for a /= k
instance (Eq s, Ord s, Ring.C k, Eq k, Ring.C (FreeModule s k), AlgebraGenerator s k) 
         => ContractibleComplex (FreeResData k s k) (FreeTensorResolution s k) where
  contraction resolution = ((contraction resolution)`ten_k`id) + 
                            ((smap (\g -> if g == ResGen 0 0 0 then toFModule g else zero))`ten_k`(contraction resolution))

