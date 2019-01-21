module Util where
    
popAt :: Int -> [a] -> ([a], a)
popAt idx xs = (lh ++ rh, el)
    where (lh, el:rh) = splitAt idx xs