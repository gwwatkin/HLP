module TableauHistory where 


import Tableau

data History a b = History [a] b 


instance Applicative (History Tableau) where
    pure = (History [])
    
    (History l1 f) <*> (History l2 x) = (History (l2++l1) (f x))
