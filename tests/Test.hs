
module Main where


import IntegerRatioTableau
import Tableau
import LPUtil


import Data.Ratio
import Data.List
import Data.List.Split
import Control.Monad


import System.IO
import System.Environment


mainloop::Tableau (Ratio Integer)-> IO (Tableau (Ratio Integer))
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
                
                
                
                

main = do
    args<-getArgs 
    t<-getTableauFromFile (args !! 0)
    putStrLn $ humanShow $ classifyTableau t
    

    
    
interpreterMain = do 
    putStrLn "type a Tableau (enter to finish):"
    x <- getTableau
    mainloop x
