{-# LANGUAGE FlexibleInstances #-}
module IntegerRatioTableau where



import LPUtil
import Tableau
import LinearProgram


import Data.Ratio
import Data.List







{-    

instance (HumanShow a)=> HumanShow (LP(Tableau a)) where
    humanShow (FBS x) = humanShow x ++"(Feasible Basic Solution)"
    humanShow (IBS x) = humanShow x ++"(Infeasible Basic Solution)"
    humanShow (OBS x) = humanShow x ++"(Optimal Basic Solution)"
    humanShow (ULP x) = humanShow x ++"(Unbounded Linear Program)"
    humanShow (ILP x) = humanShow x ++"(Infeasible Linear Program)"

      
  -}    
      
      
      
      
-- Most important method exported by this module.
-- It creates a fractional tucker tableu from a matrix of integers
integerTableau::[[Integer]]->Tableau (Ratio Integer)
integerTableau l = T (length (l!!0) -1)  (length l-1) vs $ map (map (%1) ) l
    where
        vs = map (("x"++).show) [1..(length (l!!0) -1)+(length l-1)]
        
