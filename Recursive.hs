{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Recursive where

import Data.List
import Control.DeepSeq

class Recursive r where
  getChildren :: r -> [r]
  recChildren :: r -> [r] -> r

class (Recursive r, Recursive s) => MuRecursive r s where
 muGetChildren :: r -> ([r],[s])
 muGetChildren r = (getChildren r,muGetChildrenS r)
 muRecChildren :: r -> ([r],[s]) -> r
 muGetChildrenS :: r -> [s]
 muGetChildrenS  = snd . muGetChildren

recursiveRNF r = r `seq` (if all ((==()).recursiveRNF) $ getChildren r then () else ())

muMapTop :: (MuRecursive r s, MuRecursive s r) => (r -> r) -> (s -> s) -> r -> r
muMapTop f g r = muRecChildren r' (map (muMapTop f g) rs,map (muMapTop g f) ss)
  where r' = f r
        (rs,ss) = muGetChildren r'

muMapBot :: (MuRecursive s r, MuRecursive r s) => (r -> r) -> (s -> s) -> r -> r
muMapBot f g r = f $ muRecChildren r $ (map (muMapBot f g) rs, map (muMapBot g f) ss)
  where (rs,ss) = muGetChildren r

foldTop :: (Recursive s) => (s -> a -> a) -> a -> s -> a
foldTop f a r = foldr (flip $ foldTop f) (f r a) $ getChildren r

mapAccumTop :: (Recursive s) => (s -> a -> (a,s)) -> a -> s -> (a,s)
mapAccumTop f a r = (newerAcc,recChildren newR childs)
  where (newAcc,newR) = f r a
        (newerAcc,childs) = mapAccumR (mapAccumTop f) newAcc $ getChildren r

muFoldTop :: (MuRecursive s r,MuRecursive r s) => (r -> a -> a) -> (s -> a -> a) -> a -> r -> a
muFoldTop f g a r = foldr (flip $ muFoldTop f g) (foldr (flip $ muFoldTop g f) (f r a) ss) rs
  where (rs,ss) = muGetChildren r

muFoldBot :: (MuRecursive s r,MuRecursive r s) => (r -> a -> a) -> (s -> a -> a) -> a -> r -> a
muFoldBot f g a r = f r (foldr (flip $ muFoldBot f g) (foldr (flip $ muFoldBot g f) a ss) $ rs)
  where (rs,ss) = muGetChildren r


-- need M version