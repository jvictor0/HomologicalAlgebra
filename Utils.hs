{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}
module Utils where

import NumericPrelude
import Data.List hiding (product)
import Data.Array.MArray


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


modifyArray arr i fun = readArray arr i >>= (\x -> writeArray arr i (fun x))