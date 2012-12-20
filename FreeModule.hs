{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module FreeModule where

import Prelude hiding ((+),(-),(*),negate,fromInteger,sum,product)
import AlgebraicStructure
import qualified Data.Map as Map
import Utils 

-- We wish to create a Free Module datatype
data FreeModule b r = FM (Map.Map b r) deriving (Eq,Ord)

instance (Eq r,Ord b,Ring r) => AbelianGroup (FreeModule b r) where
  (FM m1) + (FM m2) = FM $ Map.filter (/=zero) $ Map.unionWith (+) m1 m2
  negate (FM m) = FM $ Map.map negate m
  zero = FM Map.empty
  
instance (Eq r,Ord m,Ring r) => Module (FreeModule m r) r where
  r *> (FM m) = FM $ Map.filter (/=zero) $ Map.map (r*) m

instance (Ord m,Ring r, Eq r, Show m,Show r) => Show (FreeModule m r) where
  show v      
   | v == zero = "0"
   | otherwise = cim " + " (\(g,r) -> (rrshow r) ++ (rshow g)) $ toAList v
    where rrshow r = if r == one then "" else rshow r

-- the so-called Monoid Ring r[m]
-- r had best be a commutative ring for this to make sence
instance (Monoid m, Ord m, Ring r, Eq r) => Monoid (FreeModule m r) where
  m * n =  sum $ [(r1*r2)*>(toFModule $ x*y) 
                 | (x,r1) <- toAList m, (y,r2) <- toAList n]
  one = toFModule one

-- monoid rings are rings and algebras
instance (Monoid m, Ord m, Ring r, Eq r) => Ring (FreeModule m r)
instance (Monoid m, Ord m, Ring r, Eq r) => Algebra (FreeModule m r) r


coefOf :: (Eq r,Ord m,Ring r) => (FreeModule m r) -> m -> r
coefOf (FM m) b = case Map.lookup b m of
  Nothing  -> zero
  (Just r) -> r

toAList :: FreeModule m r -> [(m,r)]
toAList (FM v) = Map.toList v

fromAList :: (Ord m,Ring r, Eq r) => [(m,r)] -> FreeModule m r
fromAList ls  = FM $ Map.filter (/=zero) $ foldr (\(m,r) mp -> Map.insertWith (+) m r mp) Map.empty ls

vmap :: (Ord m', Ring r', Eq r')
        => ((m,r) -> (m',r')) -> FreeModule m r -> FreeModule m' r'
vmap f = fromAList.(map f).toAList

emap :: (Ord m', Ring r,  Eq r)
        => (m -> m') -> FreeModule m r -> FreeModule m' r
emap f = vmap (\(x,y) -> (f x,y)) 

smap :: Module m r => (t -> m) -> FreeModule t r -> m
smap f v = sum $ map (\(g,r) -> r *> (f g)) $ toAList v

rmap :: (Eq r', Ord m', Ring r') =>
     (r -> r') -> FreeModule m' r -> FreeModule m' r'
rmap f v = vmap (\(x,y) -> (x,f y)) v

isMonomial :: FreeModule m r -> Bool
isMonomial (FM v) = Map.size v == 1

toFModule :: (Ord m, Ring r, Eq r) => m -> FreeModule m r
toFModule m = fromAList [(m,one)]

fromFModule :: FreeModule m r -> (m,r)
fromFModule v
  | isMonomial v = head $ toAList v
  | otherwise    = error "Cannot call fromFModule on non monomial vectors"

