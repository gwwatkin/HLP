module LinearProgram where

-- This class represents anything that can be turned into a Linear Program using the LP class
class LinearProgram t where
    -- make sense of the format
    interpret::t->LP t
    
    solve::t ->LP t

    
    
    
    

-- This is wrapper that describes the current state of a Linear LinearProgram
data LP a = FBS a | IBS a | OBS a | ULP a | ILP a

fromLp (FBS a) = a
fromLp (IBS a) = a
fromLp (OBS a) = a
fromLp (ULP a) = a
fromLp (ILP a) = a
