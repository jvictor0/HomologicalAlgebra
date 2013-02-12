{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}
module Utils where

import NumericPrelude
import Data.List hiding (product)
import Data.Array.MArray
import qualified Data.Map as Map

xor True = not
xor False = id


(f `on` g) x y = f (g x) (g y)

(f .-. g) x y = f $ g x y

-----------------------------------
-- some functions for printing ----
-----------------------------------

-- cim = concat-intersperse-map
cim t f l = concat $ intersperse t $ map f l


rshow s = if elem '+' ss then "(" ++ ss ++ ")" else ss
  where ss = show s


-- for printing latex numbers with {}
texShow s = if length ss > 1 then "{" ++ ss ++ "}" else ss
  where ss = show s


safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeFromJust str Nothing = error str
safeFromJust _ (Just x) = x

maybeIf True x = Just x
maybeIf False _ = Nothing

maybeZero Nothing = zero
maybeZero (Just x) = x

findM pred [] = return Nothing
findM pred (x:xs) = (pred x) >>= (\b -> if b then return $ Just x else findM pred xs)

fact :: Int -> Integer
fact n = product [1..fromIntegral n]

choose 0 0 = 1
choose 0 k = 0
choose n k
  | k > n || n < 0 || k < 0  = 0
choose n k = (fact n) `div` ((fact k) * (fact $ n - k))

powerOf :: Int -> Int -> Bool
powerOf _ 0 = False
powerOf n i = n^(round $ logBase (fromIntegral n :: Double) (fromIntegral i :: Double)) == i

dropLast n = reverse . (drop n) . reverse

splitSubstr str ls = sst [] ls
  where sst fst [] = (reverse fst,[])
        sst fst rst = if take n rst == str
                      then (reverse fst,drop n rst)
                      else sst ((head rst):fst) (tail rst)
        n = length str

class UnShow u where
  unShow :: String -> u

evens [] = []
evens [x] = [x]
evens (x:_:xs) = x:(evens xs)

odds [] = []
odds x = evens $ tail x 

modifyArray arr i fun = readArray arr i >>= (\x -> writeArray arr i (fun x))

partitionsBy valToKey list = map snd $ Map.toList 
                             $ foldr (\elt mp -> Map.insertWith (++) (valToKey elt) [elt] mp) Map.empty list
