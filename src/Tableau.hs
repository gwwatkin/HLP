{-# LANGUAGE FlexibleInstances #-}
module Tableau where


import LPUtil
import LinearProgram


import Data.Ratio
import Data.List
import Data.List.Split
import Control.Monad
import Data.Maybe


type Variable = String

-- datatype representing a Tucker Tableau
data  Tableau a = T {vars::Int,constrs::Int,varNames::[Variable],table::[[a]]} deriving(Show,Eq)
-- FIXME vars indicates the number of dependant variables
--      ,switch to rows and columns


                           
                        
                           
instance (HumanShow a) => HumanShow (Tableau a) where
    humanShow (T variables constraints names tab) = 
        ( concat
        . map 
            ( (++"\n")
            . concatMap (++"\t")
            )
        . injectVars
        . map ( map humanShow )
        ) tab
        
        where
            injectVars::[[String]]->[[String]]
            injectVars ls =
                (take variables names ++ ["-1"] )
                : zipWith (\x y->x++[y]) ls (drop variables names ++ ["f"])
                
                
                
                
                
                
                
instance ( Fractional a, Num a, Eq a, Ord a)=> LinearProgram (Tableau a) where
    interpret = classifyTableau
    solve = solveTableauLP . classifyTableau
    
    

removeRow::(Eq a)=>(Tableau a)->Variable->(Tableau a)
removeRow t@(T variables constraints names tab) v =
    removeRowByIndex t ((fromJust $ findIndex (==v) names)-variables)
 
removeRowByIndex::(Eq a)=>(Tableau a)->Int->(Tableau a)
removeRowByIndex (T variables constraints names tab) n = 
    T   variables 
        (constraints-1) 
        (removeAtIndex (n+variables) names)
        (removeAtIndex n tab)     

    


removeColumn::(Eq a)=>(Tableau a)->Variable->(Tableau a)
removeColumn t@(T variables constraints names tab) v =
    removeColumnByIndex t (fromJust $ findIndex (==v) names)
 
removeColumnByIndex::(Eq a)=>(Tableau a)->Int->(Tableau a)
removeColumnByIndex (T variables constraints names tab) n = 
    T   (variables-1) 
        constraints 
        (removeAtIndex n names)
        (map (removeAtIndex n) tab) 

                
                
-- tableu pivot the chosen variables
-- Assumes variable names are unique and that you don't try funny pivots
pivot::(Fractional a, Num a)=>Tableau a ->Variable->Variable->Tableau a
pivot (T variables constraints names tab) freeVar depVar = 
        T   variables 
            constraints 
            (exchange freeVar depVar names)
            ( map ( map pivotPoint) [ [(x',y')|x'<-[0..variables]]|y'<-[0..constraints]] )
    where 
        pc = (fromJust ( elemIndex  freeVar names) )
        pr = (fromJust ( elemIndex  depVar names ) - variables)
        pivotPoint (x,y) 
              | y==pr && x==pc = 1 / (tab!!y!!x)
              | y==pr = (tab!!y!!x) / (tab!!pr!!pc)
              | x==pc = -(tab!!y!!x) / (tab!!pr!!pc)
              | otherwise = ( ((tab!!pr!!pc))*(tab!!y!!x) - (tab!!pr!!x)*(tab!!y!!pc) )/(tab!!pr!!pc)

              
              
              
type Column = Int
type Row = Int

pivotByArrayIndex::(Fractional a, Num a)=>Tableau a->Column->Row->Tableau a        
pivotByArrayIndex (T variables constraints names tab) pc pr = 
        T   variables 
            constraints 
            (exchange (names!!pc) (names!!(pr+variables)) names)
            ( map ( map pivotPoint) [ [(x',y')|x'<-[0..variables]]|y'<-[0..constraints]] )
    where 
        pivotPoint (x,y) 
              | y==pr && x==pc = 1 / (tab!!y!!x)
              | y==pr = (tab!!y!!x) / (tab!!pr!!pc)
              | x==pc = -(tab!!y!!x) / (tab!!pr!!pc)
              | otherwise = ( ((tab!!pr!!pc))*(tab!!y!!x) - (tab!!pr!!x)*(tab!!y!!pc) )/(tab!!pr!!pc) 
              
              
-- Determines the current state of a Tableau,
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


        
        
        
{- Resume here (inspect) -}        
        
-- Assumes that The tableu is IBS (TODO this needs more exceptions)
phase1step::(Fractional a, Num a,Eq a,Ord a)=>(Tableau a)->(Tableau a)
phase1step t@(T variables constraints names table) = pivotByArrayIndex t column row
    where
        -- If there is no negative tableu this is not an IBS (TODO add splecial exception)
        

        maxViolatedRowIndex = fromJust $ findIndex negative (map last (init table)) 
        -- If all entries are positive this is an ILP (TODO add splecial exception)
        
        column = chooseAntiCyclingColumn names $ findIndices negative $ checkForNeg (init (table!!maxViolatedRowIndex))
        
        
        row = getNegMin $ init $ map (\l->(last l)/(l!!column)) $ drop (maxViolatedRowIndex-1) table
        
        
        
        
        getNegMin l = chooseAntiCyclingRow names $ maxes $ filter negative l
            where maxes _ = [0]
        
        -- Just a debug method
        checkForNeg l = case find negative l of
                        Nothing -> error "passed an ILP to a phase1step"
                        Just _ ->l
                                          
                                          
phase2step::(Fractional a, Num a)=>(Tableau a)->(Tableau a)    
phase2step = id



--FIXME
chooseAntiCyclingRow::[Variable]->[Int]->Int
chooseAntiCyclingRow _ s = head s
--FIXME
chooseAntiCyclingColumn::[Variable]->[Int]->Int
chooseAntiCyclingColumn _  s  = head s
     
    
    
solveTableauLP:: (Num a,Eq a,Fractional a,Ord a)=>LP(Tableau a) -> LP(Tableau a)
solveTableauLP (IBS x) = solveTableauLP $ classifyTableau $ phase1step x
solveTableauLP (FBS x) = solveTableauLP $ classifyTableau $ phase2step x
-- other states of  tableu are terminal
solveTableauLP x = x






