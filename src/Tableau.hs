{-# LANGUAGE FlexibleInstances #-}
module Tableau where


import LPUtil

import Data.Ratio
import Data.List
import Data.List.Split
import Control.Monad
import Data.Maybe


-- datatype representing a Tucker Tableau
data  Tableau a = T {vars::Int,constrs::Int,varNames::[String],table::[[a]]} deriving(Show,Eq)
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
                
                

pivot::(Fractional a, Num a)=>Tableau a ->String->String->Tableau a
pivot (T variables constraints names tab) pc' pr' = 
        T 
            variables 
            constraints 
            (exchange pc' pr' names)
            ( map ( map pivotPoint) [ [(x',y')|x'<-[0..variables]]|y'<-[0..constraints]] )
    where 
        pc = (fromJust ( elemIndex  pc' names) )
        pr = (fromJust ( elemIndex  pr' names) ) - variables
        pivotPoint (x,y) 
              | y==pr && x==pc = 1 / (tab!!y!!x)
              | y==pr = (tab!!y!!x) / (tab!!pr!!pc)
              | x==pc = -(tab!!y!!x) / (tab!!pr!!pc)
              | otherwise = ( ((tab!!pr!!pc))*(tab!!y!!x) - (tab!!pr!!x)*(tab!!y!!pc) )/(tab!!pr!!pc)
         


