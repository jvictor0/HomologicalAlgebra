{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, NoImplicitPrelude, RebindableSyntax #-}
module SAT where

import NumericPrelude
import Recursive
import Utils
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import PropFormula
import Control.Monad.State
import System.Process
import Control.Monad
import ToCNF


inputFileName = "__INPUT_FILE_TEMP__.txt"
outputFileName = "__OUTPUT_FILE_TEMP.txt"
qmaxsat_run = "./qmaxsat-v0.4a/0.4a/minisat_static"
qmaxsat_comp_run = "./qmaxsat-v0.4a/qmaxsat0.21comp_static"

solvePMAX_SAT :: String -> IO (Maybe (Set.Set Int))
solvePMAX_SAT form_str = do
  system $ "rm " ++ inputFileName
  system $ "rm " ++ outputFileName
  writeFile inputFileName form_str 
  system $ qmaxsat_run ++ " " ++ inputFileName ++ " " ++ outputFileName
  (header:rst) <- fmap lines $ readFile outputFileName
  if header == "SAT" then return $ Just $ Set.fromList $ filter (>0) $ map read $ words $ head rst else return Nothing
  
solvePMAX_SAT2 :: String -> IO (Maybe (Set.Set Int))
solvePMAX_SAT2 form_str = do
  system $ "rm " ++ inputFileName
  system $ "rm " ++ outputFileName
  system $ "touch " ++ outputFileName
  writeFile inputFileName form_str 
  system $ qmaxsat_comp_run ++ " " ++ inputFileName ++ " >> " ++ outputFileName
  ols <- fmap lines $ readFile outputFileName
  case fmap (Set.fromList.(map read).tail.words) $ find ((=='v').head) ols of
    Nothing -> error $ "solvePMAX_SAT2: expected v line in " ++ show ols
    (Just res) -> return $ Just res