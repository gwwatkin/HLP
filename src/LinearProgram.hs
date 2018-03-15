module LinearProgram where



import LPUtil

-- This class represents anything that can be turned into a Linear Program using the LP class
class LinearProgram t where
    -- make sense of the format
    interpret::t->LP t
    
    solve::LP t ->LP t

    
    
    
    

-- This is wrapper that describes the current state of a Linear LinearProgram
data LP a = FBS a | IBS a | OBS a | ULP a | ILP a

fromLp (FBS a) = a
fromLp (IBS a) = a
fromLp (OBS a) = a
fromLp (ULP a) = a
fromLp (ILP a) = a

lpMap :: LinearProgram b => (a ->b) -> LP a -> LP b
lpMap f  = interpret . f . fromLp


instance (HumanShow a) =>  HumanShow (LP a) where
    humanShow (FBS a) = "Feasible Basic Solution\n" ++ (humanShow a)
    humanShow (IBS a) = "Infeasible Basic Solution\n" ++ (humanShow a)
    humanShow (OBS a) = "Optimum Basic Solution\n" ++ (humanShow a)
    humanShow (ULP a) = "Unbounded Linear Program\n" ++ (humanShow a)
    humanShow (ILP a) = "Infeasible Linear Program\n" ++ (humanShow a)
