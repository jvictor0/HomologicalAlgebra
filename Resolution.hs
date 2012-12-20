module Resolution where

import Prelude hiding ((+),(-),(*),negate,fromInteger,sum,product)
import AlgebraicStructure
import FreeModule
import qualified Data.Map as Map
import Utils 
import Instances
import Data.List
import GradedObject
import Data.Array
--- ResGen for making Free Resolutions of modules

data ResGen = ResGen Int Int Int deriving (Eq,Ord)

instance Show ResGen where
  show (ResGen s t primes) = (letters!!s) ++ "_" ++ (texShow $ t) ++ (take primes $ repeat '\'')

instance Graded ResGen where
  grading (ResGen s t _) = t
  
instance BiGraded ResGen where
  biGrading (ResGen s t _) = (s,t)

type FreeResolution = FreeModule ResGen

data FreeResolution a r = FR {largestInternalDegree :: Int,
                              resolutionLength :: Int,
                              differentialMap :: Array (Int,Int) 
                                                 (Map.Map ResGen FreeResolution a),
                              resolutionAugmentation :: Map.Map ResGen a,
                              Maybe (Array (Int,Int) (LUPMatrix r)}
                        deriving (Read,Eq)
                                 

                         