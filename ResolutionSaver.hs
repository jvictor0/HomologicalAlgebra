{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module ResolutionSaver where

import NumericPrelude
import FreeModule
import qualified Data.Map as Map
import Utils 
import Data.List hiding (sum)
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
import Resolution
import System.Directory
import System.IO
import Control.Monad
import SteenrodAlgebra
import ZMod2
import qualified MatrixConversions


saveResolution :: (Show s, Show k, AlgebraGenerator s k) =>
                  FilePath -> FreeResData a s k -> IO ()
saveResolution path frd = do
  writeFile path $ show (largestInternalDegree frd,
                         connectivity frd,
                         resolutionLength frd,
                         differentialMap frd)
--                         contractingMatrix frd)


resolutionExpandAndSave :: (Show s, Show k, AlgebraGenerator s k, Field.C k) =>
                           FilePath -> FreeResData a s k -> IO ()
resolutionExpandAndSave path res = do
  let expanded = extendResolution res (1+(resolutionLength res)) (1+(largestInternalDegree res))
  putStrLn $ "Resolution expanded to: " ++ (show ((1+(resolutionLength res)), (1+(largestInternalDegree res))))
  saveResolution path expanded
  resolutionExpandAndSave path expanded

{-
loadE2Page :: Int -> IO (FreeResData a SteenrodSquare ZMod2)
loadE2Page ld = do
  putStrLn $ "Loading E2 Page for t - s < " ++ (show $ ld + 1)
  exists <- doesFileExist outputFileName
  if exists
    then do
    f <- openFile outputFileName ReadMode
    [ld_old] <- fmap ((map read).words) $ hGetLine f :: IO [Int]
    putStrLn $ "We have data up to " ++ (show ld_old)
    conts <- fmap ((map words).lines)  $ hGetContents f
    let result = readE2Data conts ld_old
    putStrLn $ "There are " ++ (show $ length conts) ++ " known generators"
    hClose f
    when (ld_old < ld) $ error "not enough data" 
    return result
    else do
    putStrLn "no data found"
    error "no data"
  

readE2Data :: [[String]] -> Int -> FreeResData a SteenrodSquare ZMod2
readE2Data lsts ld = FR {connectivity= 0,
                         resolutionLength = ld,
                         largestInternalDegree = 2*ld,
                         differentialMap = array ((0,0),(ld,2*ld))
                                           [((s,t),Map.map reduceStructure $ Map.filterWithKey (\(ResGen s' t' _) _ -> (s,t) == (s',t')) tup)
                                           | s <- [0..ld], t <- [0..2*ld]],
                         kBasisOfRing = serreCartanBasis $ 2*ld,
                         resolutionAugmentation = undefined,
                         augmentationSubmodule = undefined,
                         contractingMatrix = undefined
                        }
  where tup = Map.fromList $ map (\(hed:terms) -> (unShow hed, sum $ map (toFModule . unShow) terms)) lsts

-}