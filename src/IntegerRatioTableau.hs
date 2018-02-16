{-# LANGUAGE FlexibleInstances #-}
module IntegerRatioTableau where



import LPUtil
import Tableau
import LinearProgram


import Data.Ratio
import Data.List









                
instance ( Fractional a, Num a, Eq a)=> LinearProgram (Tableau a) where
    interpret = classifyTableau
    solve = solveTableauLP . classifyTableau
    
    

--phase1step (IBS x) =      
phase1step = id
    
    
    
phase2step = id





classifyTableau::(Num a,Eq a) =>(Tableau a)->LP(Tableau a)
classifyTableau x@(T variables constraints varNames table)
    = if couldBeFeasible
      then 
          if isUnBoundedLP
          then (ULP x)
          else
              if isOptimal then (OBS x) else (FBS x)
      else
          if isUnBoundedLP
          then
              (ULP x)
          else
              if isInfeasibleLP then (ILP x) else (IBS x)

    where
        -- check that all entries in the last column are non negative
        couldBeFeasible = all (nonNegative) $ map last $ init table
        
        -- check that all entries in the last row are non positive
        isOptimal = all (nonPositive) $ init $ last $ table
        
        -- check that there is a column with all non positive entries except for the last and a negative last entry
        isUnBoundedLP = any (\row-> all (nonPositive) (init row) && positive (last row)) $ transpose $  map init table
            
        -- check that there is a row such that every entry is non positive except for the last one and
        -- a negative last one
        isInfeasibleLP = any (\row-> all (nonNegative) (init row) && negative (last row)) $ init table
        
        
    
    
solveTableauLP:: (Num a,Eq a,Fractional a)=>LP(Tableau a) -> LP(Tableau a)
solveTableauLP (IBS x) = solveTableauLP $ classifyTableau $ phase1step x
solveTableauLP (FBS x) = solveTableauLP $ classifyTableau $ phase2step x
solveTableauLP x = x
    

instance (HumanShow a)=> HumanShow (LP(Tableau a)) where
    humanShow (FBS x) = humanShow x ++"(Feasible Basic Solution)"
    humanShow (IBS x) = humanShow x ++"(Infeasible Basic Solution)"
    humanShow (OBS x) = humanShow x ++"(Optimal Basic Solution)"
    humanShow (ULP x) = humanShow x ++"(Unbounded Linear Program)"
    humanShow (ILP x) = humanShow x ++"(Infeasible Linear Program)"

                
integerTableau::[[Integer]]->Tableau (Ratio Integer)
integerTableau l = T (length (l!!0) -1)  (length l-1) vs $ map (map (%1) ) l
    where
        vs = map (("-=x"++).show) [1..(length (l!!0) -1)+(length l-1)]
        
