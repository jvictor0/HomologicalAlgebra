module LispShow where

import Data.List
import Utils

tabln = 2
maxLineLen = 30

indent n str = (take (n*tabln) $ repeat ' ')++str
tab n = init . unlines . (map (indent n)) . lines

lispShow :: String -> [String] -> String
lispShow fn showargs =  if any ('\n' `elem`) showargs || maxLineLen < (sum $ map length showargs)
                      then "(" ++ fn ++ "\n" ++ (cim "\n" (tab 1) showargs) ++ ")"
                      else "(" ++ fn ++ " " ++ (cim " " id showargs) ++ ")"
