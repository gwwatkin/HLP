
module HLPInterpreter where


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



pivotWithCheck = pivot -- TODO implement


type NiceSimplexType = LP (Tableau (Ratio Integer) )

mainloop::[NiceSimplexType]-> IO (NiceSimplexType)
mainloop (a:xs) = do
    putStrLn $ humanShow $ a
    putStr "->"
    hFlush stdout
    s<-getLine
    let tokens = splitOn " " s
    case tokens!!0 of 
        "pivot"->
            mainloop $ lpMap (pivotWithCheck (tokens!!1) (tokens!!2)) a  : a:xs
            
        'q':_-> return a
         
        
        "new" -> interpreterMain
        
        "remove" -> case tokens!!1 of
                        "row" -> mainloop $  lpMap (removeRow (tokens!!2)) a  : a:xs
                        "column" -> mainloop $ lpMap (removeColumn (tokens!!2)) a : a:xs
                        _    -> do
                                putStrLn "error on input"
                                mainloop (a:xs)
        "back" -> mainloop xs
        
        
        --"phase 1 step" -> mainloop
        
        "solve" -> mainloop $ ( solve a) : a:xs 

         
        _ ->do
             putStrLn "feature not yet implemented"
             mainloop (a:xs)
             
mainloop [] = interpreterMain
             
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
    mainloop [interpret x]
    
    


