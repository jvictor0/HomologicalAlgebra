{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, RebindableSyntax, NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module FreeModule where

import NumericPrelude 
import Data.List hiding (sum,product)
import Control.Monad.ST
import Data.Array.ST hiding (unsafeFreeze)
import Control.Monad
import Data.STRef
import Debug.Trace
import Data.Maybe
import Data.Array
import Utils
import qualified MathObj.Matrix as Matrix
import Data.Array
import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Ring as Ring
import qualified Algebra.IntegralDomain as Domain
import qualified MathObj.Polynomial as Poly
import qualified Number.Ratio as Ratio
import qualified Algebra.Units as Units
import qualified Algebra.Additive as AbelianGroup
import qualified Algebra.Module as Module
import MatrixConversions
import qualified MatrixUtils                              
import Data.Array.Unsafe
import qualified PermutationAlgorithms as PermAlgs
import qualified Algebra.Monoid as Monoid
import qualified Data.Map as Map


-- We wish to create a Free Module datatype
data FreeModule b r = FM (Map.Map b r) deriving (Eq,Ord)

instance (Eq r,Ord b,Ring.C r) => AbelianGroup.C (FreeModule b r) where
  (FM m1) + (FM m2) = FM $ Map.filter (/=zero) $ Map.unionWith (+) m1 m2
  negate (FM m) = FM $ Map.map negate m
  zero = FM Map.empty
  
instance (Eq r,Ord m,Ring.C r) => Module.C r (FreeModule m r) where
  r *> (FM m) = FM $ Map.filter (/=zero) $ Map.map (r*) m

instance (Ord m,Ring.C r, Eq r, Show m,Show r) => Show (FreeModule m r) where
  show v      
   | v == zero = "0"
   | otherwise = cim " + " (\(g,r) -> (rrshow r) ++ (rshow g)) $ toAList v
    where rrshow r = if r == one then "" else rshow r

instance (Ring.C r, Ord m, Eq r, Read m, Read r) => Read (FreeModule m r) where
  readsPrec _ ('0':rst) = [(zero,rst)]
  readsPrec n term = case firstTm of
    [] -> []
    [(f,r)] -> case words r of
      (('+':tm):rst) -> case readsPrec n (unwords $ tm:rst) of
        [(g,s)] -> [(f+g,s)]
        []      -> []
      _              -> firstTm
    where firstTm = case readParen False (readsPrec n) term of
            [] -> case readsPrec n term of
              [(mod,rst)] -> [(toFModule mod,rst)]
              []          -> []
            [(rng,modStr)] -> case readsPrec n modStr of
              [(mod,rst)] -> [(fromAList [(mod,rng)],rst)]
              []          -> []
      
coefOf :: (Eq r,Ord m,Ring.C r) => (FreeModule m r) -> m -> r
coefOf (FM m) b = case Map.lookup b m of
  Nothing  -> zero
  (Just r) -> r

toAList :: FreeModule m r -> [(m,r)]
toAList (FM v) = Map.toList v

toFModule :: (Ord m, Ring.C r, Eq r) => m -> FreeModule m r
toFModule m = fromAList [(m,one)]

asCoefOf :: a -> FreeModule r a -> a
asCoefOf r v = r 


fromAList :: (Ord m,Ring.C r, Eq r) => [(m,r)] -> FreeModule m r
fromAList ls  = FM $ Map.filter (/=zero) $ foldr (\(m,r) mp -> Map.insertWith (+) m r mp) Map.empty ls

toBasisList m = map fst $ toAList m

vmap :: (Ord m', Ring.C r', Eq r')
        => ((m,r) -> (m',r')) -> FreeModule m r -> FreeModule m' r'
vmap f = fromAList.(map f).toAList

emap :: (Ord m', Ring.C r,  Eq r)
        => (m -> m') -> FreeModule m r -> FreeModule m' r
emap f = vmap (\(x,y) -> (f x,y)) 

-- monad structure!!! 
smap :: Module.C r m => (t -> m) -> FreeModule t r -> m
smap f v = sum $ map (\(g,r) -> r *> (f g)) $ toAList v

smapMaybe :: Module.C r m => (t -> Maybe m) -> FreeModule t r -> m
smapMaybe f v = sum $ map (\(g,r) -> case f g of 
                          (Just fg) -> r *> fg 
                          Nothing -> zero) 
            $ toAList v

rmap :: (Eq r', Ord m', Ring.C r') =>
     (r -> r') -> FreeModule m' r -> FreeModule m' r'
rmap f v = vmap (\(x,y) -> (x,f y)) v

isMonomial :: FreeModule m r -> Bool
isMonomial (FM v) = Map.size v == 1

fromFModule :: FreeModule m r -> (m,r)
fromFModule v
  | isMonomial v = head $ toAList v
  | otherwise    = error "Cannot call fromFModule on non monomial vectors"


decompose :: (Eq r, Ord m, Ring.C r) => Array Int m -> FreeModule m r -> Matrix.T r
decompose basis vect = Matrix.fromColumns (b-a+1) 1 $ [map (coefOf vect) $ elems basis]
  where (a,b) = bounds basis
        
recompose :: (Eq r, Ord m, Ring.C r) => Array Int m -> Matrix.T r -> FreeModule m r
recompose basis vect 
  | let (a,b) = bounds basis 
    in Matrix.numRows vect == (b-a+1) && Matrix.numColumns vect == 1 = 
    sum $ zipWith (\[v] b -> v *> (toFModule b)) (Matrix.rows vect) (elems basis)
  | otherwise  = error "cannot recompose matrix or vector from wrong sized basis"
                 
linearMatrix src targ fun = Matrix.fromColumns (d-c+1) (b-a+1) $ 
  map (\e -> let fe = fun $ toFModule e
             in map (coefOf fe) $ elems targ)
  $ elems src
  where (a,b) = bounds src
        (c,d) = bounds targ
        
