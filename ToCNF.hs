{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, NoImplicitPrelude, RebindableSyntax #-}
module ToCNF where

import NumericPrelude
import Recursive
import Utils
import Debug.Trace
import qualified Data.Map as Map
import Data.List
import PropFormula
import Control.Monad.State
import System.Process
import Control.Monad
import Control.DeepSeq
import Control.Parallel
import System.CPUTime

type ToCNFState tag = State (Map.Map (PropFormula tag Bool ()) (PropFormula tag Bool ()))

toCNF0 :: (Freshable tag, Ord tag) => PropFormula tag Bool () -> ToCNFState tag (PropFormula tag Bool ())
toCNF0 term 
  | isBitVar term || isNot term = return term
  | isAnd term || isOr term  = fmap (recChildren term) $ 
    forM (getChildren term) $ \c -> do
      c' <- toCNF0 c
      label c'
    
label term
  | isBitVar term || isNot term = return term
  | isAnd term || isOr term     = do
    mp <- get
    case Map.lookup term mp of
      Nothing -> case Map.lookup (pfNot term) mp of
        Nothing -> (put $ Map.insert term (fresh $ Map.size mp + 1) mp) >> (return $ fresh $ Map.size mp + 1)
        (Just nlab) -> return $ pfNot nlab
      (Just lab) -> return lab

pdeepseq x y = (rnf x) `pseq` y

toCNFTime term = do
 start <- term `pdeepseq` getCPUTime
 let (topLevel,labels) = runState (mapM toCNF0 $ getChildren term) Map.empty
 first <- (topLevel,labels) `pdeepseq` getCPUTime
 let labiffs = concatMap (uncurry labiff) $ Map.toList labels
       where  labiff intTerm lab 
                | isAnd intTerm = (pfOr $ lab:(map pfNot $ getChildren intTerm)):
                                  (map ((pfNot lab)|||) $ getChildren intTerm)
                | isOr intTerm = (pfOr $ (pfNot lab):(getChildren intTerm)):
                                 (map (lab|||) $ map pfNot $ getChildren intTerm)
 second <- labiffs `pdeepseq` getCPUTime
 let result = pfAnd $ topLevel ++ labiffs
 end <- result `pdeepseq` getCPUTime
 putStrLn $ "toplelevel made in " ++ (show ((fromIntegral (first - start)) / (10^12) :: Double)) ++ " secs"
 putStrLn $ "labiffs made in " ++ (show ((fromIntegral (second - first)) / (10^12) :: Double)) ++ " secs"
 putStrLn $ "result wrapped made in " ++ (show ((fromIntegral (end - second)) / (10^12) :: Double)) ++ " secs"
 
  

toCNF term 
  | isAnd term = pfAnd $ topLevel ++ labiffs
  where (topLevel,labels) = runState (mapM toCNF0 $ getChildren term) Map.empty
        labiffs = concatMap (uncurry labiff) $ Map.toList labels
        labiff intTerm lab 
          | isAnd intTerm = (pfOr $ lab:(map pfNot $ getChildren intTerm)):
                            (map ((pfNot lab)|||) $ getChildren intTerm)
          | isOr intTerm = (pfOr $ (pfNot lab):(getChildren intTerm)):
                            (map (lab|||) $ map pfNot $ getChildren intTerm)

type Literal = Int
data ClauseType = XOrClause | OrClause deriving (Show,Eq,Ord)
type Clause = (ClauseType,[Literal])
type CNFFormula = [Clause]
type PMAX_SAT_Formula = (CNFFormula,CNFFormula)

toPMAXDIMACS (soft,hard) = let (mp,softs) = toXDIMACS soft in let (mp',hards) = toXDIMACS0 mp hard in (mp',(softs,hards))


toXDIMACS term = toXDIMACS0 Map.empty term
toXDIMACS0 mp term
  | isAnd term = mapAccumR processClause mp $ getChildren term
processLiteral mp tm 
  | isBitVar tm = case Map.lookup tm mp of
    Nothing -> (Map.insert tm (1+Map.size mp) mp,1+Map.size mp)
    (Just n) -> (mp,n)
  | isNot tm = case Map.lookup (head $ getChildren tm) mp of
    Nothing -> (Map.insert (head $ getChildren tm) (1+Map.size mp) mp,-(1+Map.size mp))
    (Just n) -> (mp,-n)
  | otherwise = error "not cnf: expected  literal"
processClause mp cl 
  | isBitVar cl || isNot cl = let (mp',lit) = processLiteral mp cl in (mp',(OrClause,[lit]))
  | otherwise  = let (mp',lits) = mapAccumR processLiteral mp $ getChildren cl
                         in (mp',(if isOr cl then OrClause else if isXOr cl then XOrClause else error "not cnf ugg",lits))
                          
toXDIMACSString mp formula = "p cnf " ++ (show $ Map.size mp) ++ " " ++ (show $ length formula) ++ "\n" ++
                             (concatMap (\(typ,lits) -> (if typ == OrClause then "" else "x") ++
                                                        (concat $ map (\l -> (show l) ++ " ") lits) ++ "0\n")
                              formula)
toPMAXDIMACSString mp (soft,hard) = "p wcnf " ++ (show $ Map.size mp) ++ " " ++ (show $ hardNum-1) ++ " " ++ (show $ hardNum) ++ "\n" ++
                             (concatMap (\(typ,lits) -> "1" ++ " " ++ (concat $ map (\l -> (show l) ++ " ") lits) ++ "0\n")
                              soft) ++ 
                             (concatMap (\(typ,lits) -> (show $ hardNum) ++ " "  ++ (concat $ map (\l -> (show l) ++ " ") lits) ++ "0\n")
                              hard)                        
  where hardNum = 1 + (length $ soft ++ hard)
