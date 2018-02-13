{-# LANGUAGE FlexibleInstances #-}

--import qualified Data.Map.Strict as M
import Data.Ratio
import Data.List
import Data.List.Split
import Control.Monad
import Data.Maybe


import System.IO
import System.Environment

import Debug.Trace


-- datatype representing a Tucker Tableu
data  Tableu a = T {vars::Int,constrs::Int,varNames::[String],table::[[a]]} deriving(Show,Eq)


-- print things in a nice human readable form
class HumanShow a where
    humanShow::a->String

    
instance HumanShow (Ratio Integer) where
    humanShow a = case denominator a of
                           1 -> show $ numerator a
                           _ -> (show.numerator) a ++ "/" ++ (show.denominator) a
    
    
    
    
instance (HumanShow a) => HumanShow (Tableu a) where
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


_NUMBER_CHARS::[Char]
_NUMBER_CHARS = "1234567890/."



exchange::Eq a =>a->a->[a]->[a]
exchange x y (e:es)
    | e == x = y:(exchange x y es)
    | e == y = x:(exchange x y es)
    | otherwise = e:(exchange x y es)
exchange _ _ [] = []



         
integerTableu::[[Integer]]->Tableu (Ratio Integer)
integerTableu l = T (length (l!!0) -1)  (length l-1) vs $ map (map (%1) ) l
    where
        vs = map (("-=x"++).show) [1..(length (l!!0) -1)+(length l-1)]


pivot::(Fractional a, Num a)=>Tableu a ->String->String->Tableu a
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
         

         
         

 


            

mainloop::Tableu (Ratio Integer)-> IO (Tableu (Ratio Integer))
mainloop a  = do
    putStrLn $ humanShow $ a
    putStr "->"
    s<-getLine
    let ls = splitOn " " s
    case ls!!0 of 
         "pivot"->
            mainloop $ pivot a (ls!!1) (ls!!2)
            
         "quit"-> return a
         
         _ ->do
             putStrLn "feature not yet implemented"
             mainloop a

getTableu:: IO (Tableu (Ratio Integer))
getTableu = liftM (integerTableu . map (map (read) . splitOneOf " ,\t") . reverse) $ tHelp []
    where
        tHelp::[String]->IO [String]
        tHelp x = do
            ln <- getLine
            if ln == "" then
                return x
            else 
                tHelp (ln:x)

                
                
getTableuFromFile::String->IO (Tableu (Ratio Integer))
getTableuFromFile file =do
    h <-openFile file ReadMode
    t <- tHelp [] h
    hClose h
    return $ (integerTableu . map (map (read) . splitOneOf " ,\t") . reverse) t
    where
        tHelp::[String]->Handle->IO [String]
        tHelp x h' = do
            ln <- hGetLine h'
            if ln == "" then
                return x
            else 
                tHelp (ln:x) h'
    
-- #################################### Linear Programs ######################################

-- This class represents anything that can be turned into a Linear Program
class LinearProgram t where
    -- make sense of the format
    interpret::t->LP t
    
    solve::t ->LP t

    
    
    
    

-- helper type to classify  the state of a n LPs 
data LP a = FBS a | IBS a | OBS a | ULP a | ILP a






instance Functor LP where
    fmap f (FBS x) = classifyLP (f x) 
    fmap f (IBS x) = classifyLP (f x) 
    fmap f (OBS x) = classifyLP (f x) 
    fmap f (ULP x) = classifyLP (f x) 
    fmap f (ILP x) = classifyLP (f x) 
   
    

    
instance ( Fractional a, Num a)=> LinearProgram (Tableu a) where
    interpret = classifyLP
    solve = solveLP . classifyLP
    
    
    
phase1step = id
phase2step = id



-- Given a 
classifyLP
classifyLP = IBS
    
    
    
solveLP:: LP(Tableu a) -> LP(Tableu a)
solveLP (IBS x) = solveLP $ classifyLP $ phase1step x
solveLP (FBS x) = solveLP $ classifyLP $ phase2step x
solveLP x = x
    

instance (HumanShow a)=> HumanShow (LP(Tableu a)) where
    humanShow (FBS x) = humanShow x ++"(Feasible Basic Solution)"
    humanShow (IBS x) = humanShow x ++"(Infeasible Basic Solution)"
    humanShow (OBS x) = humanShow x ++"(Optimal Basic Solution)"
    humanShow (ULP x) = humanShow x ++"(Unbounded Linear Program)"
    humanShow (ILP x) = humanShow x ++"(Infeasible Linear Program)"



    
main = do
    args<-getArgs 
    t<-getTableuFromFile (args !! 0)
    putStrLn $ humanShow $ classifyLP t
    

    
    
interpreterMain = do 
    putStrLn "type a Tableu (enter to finish):"
    x <- getTableu
    mainloop x
    
    
