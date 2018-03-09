{-# LANGUAGE FlexibleInstances #-}

module LPUtil where
    
    
import Data.List
import Data.Ratio


class HumanShow a where
    humanShow::a->String 


instance HumanShow (Ratio Integer) where
    humanShow a = case denominator a of
                           1 -> show $ numerator a
                           _ -> (show.numerator) a ++ "/" ++ (show.denominator) a

-- some number comparison utilities
    
negative::(Num a,Eq a)=>a->Bool
negative = (fromInteger (-1) ==).signum

nonNegative::(Num a,Eq a)=>a->Bool
nonNegative = not.negative

positive::(Num a,Eq a)=>a->Bool
positive = (fromInteger 1 ==).signum

nonPositive::(Num a,Eq a)=>a->Bool
nonPositive = not.positive

isZero::(Num a,Eq a)=>a->Bool
isZero = (fromInteger 0 ==).signum


_NUMBER_CHARS::[Char]
_NUMBER_CHARS = "1234567890/."



exchange::Eq a =>a->a->[a]->[a]
exchange x y (e:es)
    | e == x = y:(exchange x y es)
    | e == y = x:(exchange x y es)
    | otherwise = e:(exchange x y es)
exchange _ _ [] = []




removeAtIndex:: (Eq a)=>Int->[a]->[a]
removeAtIndex n l = let (xs,y:ys) = splitAt n l in xs++ys 
