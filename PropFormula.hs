{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, NoImplicitPrelude, RebindableSyntax #-}
module PropFormula where

import NumericPrelude
import Recursive
import qualified Data.MultiSet as MS
import LispShow
import Utils
import qualified Data.Set as Set
import Debug.Trace
import FreeModule
import ZMod2
import qualified Algebra.Module as Module
import qualified Algebra.Additive as Additive
import qualified Algebra.Ring as Ring
import Control.DeepSeq

data PropFormula tag aux spec = PF aux (PFData tag aux spec) | PTrue | PFalse | PUndefined  deriving (Eq, Ord)
data PFData tag aux special = And (Set.Set (PropFormula tag aux special)) |
                              Or (Set.Set (PropFormula tag aux special)) |
                              XOr (FreeModule (PropFormula tag aux special) ZMod2) |
                              Not (PropFormula tag aux special) |
                              BitVar tag |
                              SpecialTerm special
                            deriving (Eq,Ord)

fromZMod2 :: ZMod2 -> PropFormula tag aux spec
fromZMod2 0 = PFalse
fromZMod2 1 = PTrue

instance (Show tag, Show special) => Show (PropFormula tag aux special) where
  show (PF _ d) = show d
  show PTrue = "True"
  show PFalse = "False"
  show PUndefined = "?"

instance (Show tag, Show special) => Show (PFData tag aux special) where
  show (Not tm) = lispShow "not" [show tm]
  show (XOr ms) = lispShow "xor" $ map show $ toBasisList ms
  show (And ms) = lispShow "and" $ map show $ Set.toList ms
  show (Or ms) = lispShow "or" $ map show $ Set.toList ms
  show (BitVar v) = show v
  show (SpecialTerm t) = show t



class Normable hb where
  isNorm :: hb -> Bool
  setNorm :: Bool -> hb -> hb

instance (Normable aux) => Normable (PropFormula tag aux spec) where
  isNorm PTrue = True
  isNorm PFalse = True
  isNorm PUndefined = True
  isNorm (PF v _) = isNorm v
  setNorm b (PF v x) = PF (setNorm b v) x
  setNorm b x = x
  
instance Normable Bool where
  isNorm = id
  setNorm = const
  
class DataWrap dw dat where
  getData :: dw -> dat
  wrapData :: dat -> dw

class AuxData ad where
  calculateAuxData :: (PFData t ad s) -> ad
  
instance (AuxData ad) => DataWrap (PropFormula t ad s) (PFData t ad s) where
  getData (PF _ d) = d
  wrapData d = PF (calculateAuxData d) d

instance AuxData Bool where
  calculateAuxData _ = False

instance (Normable a, Ord t, Ord s, Ord a) => 
         Recursive (PropFormula t a s) where
  getChildren (PF _ (And s)) = Set.toList s
  getChildren (PF _ (Or s)) = Set.toList s
  getChildren (PF _ (XOr f)) = toBasisList f
  getChildren (PF _ (Not n)) = [n]
  getChildren _ = []
  recChildren PFalse [] = PFalse
  recChildren PTrue [] = PTrue
  recChildren PUndefined [] = PUndefined
  recChildren v cs
    | isNorm v = recChildren (setNorm False v) cs
  recChildren (PF v (And _)) cs = PF v $ And $ Set.fromList cs
  recChildren (PF v (Or _)) cs = PF v $ Or $ Set.fromList cs
  recChildren (PF v (XOr _)) cs = PF v $ XOr $ sum $ map toFModule cs
  recChildren (PF v (Not _)) [n] = PF v $ Not n
  recChildren v _ = v

instance (Normable a, Ord t, Ord s, Ord a) => 
         NFData (PropFormula t a s) where
  rnf = recursiveRNF

instance (Recursive s, Ord t, Ord s, Ord a, Normable a)
         => MuRecursive (PropFormula t a s) s where
  muGetChildren (PF _ (SpecialTerm s)) = ([],getChildren s)
  muGetChildren term = (getChildren term,[])
  muRecChildren v cs
    | isNorm v = muRecChildren (setNorm False v) cs
  muRecChildren (PF v (SpecialTerm s)) ([],cs) = PF v $ SpecialTerm $ recChildren s cs
  muRecChildren term (cs,[]) = recChildren term cs
  
class Freshable t where
  fresh :: Int -> t

instance (Freshable t, AuxData a) => Freshable (PropFormula t a s) where
  fresh i = wrapData $ (BitVar $ fresh i :: PFData t a s)

isAnd (PF _ (And _)) = True
isAnd _ = False
isOr (PF _ (Or _)) = True
isOr _ = False
isXOr (PF _ (XOr _)) = True
isXOr _ = False
isNot (PF _ (Not _)) = True
isNot _ = False
isSpecial (PF _ (SpecialTerm _)) = True
isSpecial _ = False
isTrue x = PTrue == x
isFalse x = PFalse == x
isBitVar (PF _ (BitVar _)) = True
isBitVar _ = False
isUndefined x = PUndefined ==x
getTag (PF _ (BitVar x)) = x
getTag x = error $ "wtf: " ++ (show x)



a &&& b = pfAnd [a,b]
a ||| b = pfOr [a,b]
a <+> b = pfXOr [a,b]
pfNot :: (AuxData a, Normable a, Ord t, Ord s, Ord a) =>
         (PropFormula t a s) -> (PropFormula t a s)
pfNot PTrue = PFalse
pfNot PFalse = PTrue
pfNot a 
  | isNot a   = head $ getChildren a
  | isXOr a   = let (PF _ (XOr s)) = a in wrapData $ XOr $ s+(toFModule PTrue)
  | isAnd a   = pfOr $ map pfNot $ getChildren a
  | isOr a    = pfAnd $ map pfNot $ getChildren a
  | otherwise = wrapData $ Not a
pfAnd :: (Ord t, Ord a, Ord s, AuxData a, Normable a) =>
         [PropFormula t a s] -> (PropFormula t a s)
pfAnd lst = if PFalse `elem` lst then PFalse else if null alist then PTrue else if null $ tail alist then head alist else  wrapData $ And $ Set.fromList alist
  where alist =  filter (/=PTrue)
                 $ concatMap (\t -> if isAnd t 
                                  then getChildren t 
                                    else if isXOr t
                                       then let first = pfXOr $ evens $ getChildren t
                                                second = pfXOr $ odds $ getChildren t
                                            in [first ||| second, (pfNot first) ||| (pfNot second)]
                                       else [t]) 
                  lst
pfOr :: (Ord t, Ord a, Ord s, AuxData a, Normable a) =>
        [PropFormula t a s] -> (PropFormula t a s)
pfOr lst = if PTrue `elem` lst then PTrue else if null alist then PFalse else if null $ tail alist then head alist else  wrapData $ Or $ Set.fromList alist
  where alist =  filter (/=PFalse)
                 $ concatMap (\t -> if isOr t 
                                  then getChildren t 
                                  else if isXOr t
                                       then let first = pfXOr $ evens $ getChildren t
                                                second = pfXOr $ odds $ getChildren t
                                            in [first &&& (pfNot second), (pfNot first) &&& second]
                                       else [t]) 
                  lst
pfXOr :: (Ord t, Ord a, Ord s, AuxData a, Normable a) =>
         [PropFormula t a s] -> (PropFormula t a s)
pfXOr lst = if null alist then PFalse else if null $ tail alist then head alist else wrapData $ XOr $  fromAList $ map (flip (,) 1) alist0
  where alist0 = concatMap (\t -> if isXOr t then getChildren t else [t]) alist
        alist = concatMap (\tm-> if isNot tm then [head $ getChildren tm,PTrue] else [tm]) $ filter (/=PFalse) lst


instance (Normable a,Ord t, Ord a, Ord s, AuxData a)
         => Additive.C (PropFormula t a s) where  
  (+) = (<+>)
  (-) = (<+>)
  zero = PFalse

instance (Normable a,Ord t, Ord a, Ord s, AuxData a) => Ring.C (PropFormula t a s) where
  (*) = (&&&)
  fromInteger 0 = PFalse
  fromInteger 1 = PTrue
  fromInteger n = fromInteger $ n `mod` 2


