
module Main where


import IntegerRatioTableau
import Tableau
import LPUtil
import LinearProgram


import Data.Ratio
import Data.List
import Data.List.Split
import Control.Monad


import System.IO
import System.Environment



pivotWithCheck = pivot

mainloop::Tableau (Ratio Integer)-> IO (Tableau (Ratio Integer))
mainloop a  = do
    putStrLn $ humanShow $ a
    putStr "->"
    hFlush stdout
    s<-getLine
    let ls = splitOn " " s
    case ls!!0 of 
        "pivot"->
            mainloop $ pivotWithCheck a (ls!!1) (ls!!2)
            
        'q':_-> return a
         
        
        "new" -> interpreterMain
        
        "remove" -> case ls!!1 of
                        "row" -> mainloop $ removeRow a (ls!!2)
                        "column" -> mainloop $ removeColumn a (ls!!2)
                        _    -> do
                                putStrLn "error on input"
                                mainloop a
                        
        _ ->do
             putStrLn "feature not yet implemented"
             mainloop a
             
getTableau:: IO (Tableau (Ratio Integer))
getTableau = liftM (integerTableau . map (map (read) . splitOneOf " ,\t") . reverse) $ tHelp []
    where
        tHelp::[String]->IO [String]
        tHelp x = do
            ln <- getLine
            if ln == "" then
                return x
            else 
                tHelp (ln:x)

                
                
                
getTableauFromFile::String->IO (Tableau (Ratio Integer))
getTableauFromFile file =do
    h <-openFile file ReadMode
    t <- tHelp [] h
    hClose h
    return $ (integerTableau . map (map (read) . splitOneOf " ,\t") . reverse) t
    where
        tHelp::[String]->Handle->IO [String]
        tHelp x h' = do
            ln <- hGetLine h'
            if ln == "" then
                return x
            else 
                tHelp (ln:x) h'
                


    
    
interpreterMain = do 
    putStrLn "type a Tableau (enter to finish):"
    x <- getTableau
    mainloop x
    
    

    
                

someTest = do
    args<-getArgs 
    putStrLn "\n\n[Running tests]"
    t<-getTableauFromFile "tableu.txt"
    putStrLn $ humanShow $ classifyTableau t
    putStrLn "Pivoting 1 2 (x2, x5):"
    putStrLn $ humanShow $ pivotByArrayIndex t 1 2
   
    
    
    
    
    
    
main = interpreterMain
