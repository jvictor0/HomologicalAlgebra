module Main where

import ResolutionSaver
import SteenrodResolutions

main = do
  result <- (stableHomotopyBrunerSphere 200 400)
  writeFile "sphereResArray.dat" (show result)