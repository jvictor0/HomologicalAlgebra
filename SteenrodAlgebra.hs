{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module SteenrodAlgebra where

import NumericPrelude
import qualified Algebra.Module as Module
import FreeModule
import Algebra
import Utils
import ZMod2
import Data.Array
import Data.List
import Data.Char
import qualified Algebra.Monoid as Monoid
import Debug.Trace

data SteenrodSquare = Sq [Int] deriving (Eq, Ord)

instance Show SteenrodSquare where
  show (Sq []) = "1"
  show (Sq [x]) = "Sq^" ++ (texShow x)
  show (Sq ls) = "Sq^{\\{" ++ (cim "," show ls ) ++ "\\}}"

instance UnShow SteenrodSquare where
  unShow "1" = Sq[]
  unShow str = if take (length "Sq^{\\{") str == "Sq^{\\{"
               then Sq $ read $ "[" ++ (dropLast (length "\\}}") $ drop (length "Sq^{\\{") str) ++ "]"
               else if isDigit $ last str
                    then Sq [read [last str]]
                    else Sq [read $ (init $ drop (length "Sq^{") str)]

instance Read SteenrodSquare where
  readsPrec n (' ':rst) = readsPrec n rst
  readsPrec _ ('S':'q':'^':n:rst) | isDigit n = [(Sq [read [n]],rst)]
  readsPrec _ ('S':'q':'^':'{':longNum) 
    | let (digs,rst) = break (not.isDigit) longNum
      in (not $ null rst) && ('}' == (head rst)) = [(Sq [read $ takeWhile isDigit longNum],tail $ dropWhile isDigit longNum)]
  readsPrec x ('S':'q':'^':'{':'\\':'{':seqlst) 
    | let (digs,rst) = break (=='\\') seqlst
      in ((take (length "\\}}") rst) == "\\}}") && (not $ null $ (readsPrec x $ "[" ++ digs ++ "]" :: [([Int],String)])) = 
         let (digs,rst) = break (=='\\') seqlst
         in let [(sm,"")] = readsPrec x $ "[" ++ digs ++ "]"
         in [(Sq sm,drop (length "\\}}") rst)]
  readsPrec _ _ = []
                                               
type SteenrodAlgebra = FreeModule SteenrodSquare ZMod2

instance AlgebraGenerator SteenrodSquare ZMod2 where
  idt = toFModule $ Sq []
  (Sq x) <*> (Sq y) = decompAdmis $ Sq $ x ++ y

adem ::  Int -> Int -> FreeModule SteenrodSquare ZMod2
adem i j = fromAList [(Sq $ filter (/=0) [i+j-k,k],
                       fromInteger $ choose (j-k-1) (i-2*k))
                     | k <- [0..i`div`2]]

decompAdmis :: SteenrodSquare -> FreeModule SteenrodSquare ZMod2
decompAdmis (Sq ss) = case admiss [] ss of
  Nothing -> toFModule $ Sq ss
  (Just (ad,i,j,rst)) -> (toFModule $ Sq ad)*(adem i j)*(decompAdmis $ Sq rst)
  where admiss _ [] = Nothing
        admiss _ [x] = Nothing
        admiss ps (x:y:rst)
          | x < 2*y   = Just (reverse ps,x,y,rst)
          | otherwise = admiss (x:ps) $ y:rst

admisArray :: Int -> Array Int [SteenrodSquare]
admisArray largeDeg = array (0,largeDeg) $
                      [(i,squaresInDegree i) | i <- [0..largeDeg]]

squaresInDegree 0 = [Sq []]
squaresInDegree i = map Sq $ concatMap (\j -> aft j i) [1..i]
  where aft i j
          | i == j     = [[i]]
          | i > j      = []
          | otherwise  = map (++[i]) $ concatMap (\k -> (aft k (j-i))) [2*i..j]

serreCartanBasis j = let arr = admisArray j in \i -> if i <= j then arr!i else squaresInDegree i                 
                                                                                
sq :: [Int] -> SteenrodAlgebra
sq ns = decompAdmis $ Sq ns

