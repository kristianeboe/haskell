module Lab1 (subst, interleave, unroll) where

subst :: Eq t => t -> t -> [t] -> [t]
subst a b xs = [ if x == a then b else x | x<-xs]

interleave :: [t] -> [t] -> [t]
interleave as [] = as
interleave [] bs = bs
interleave (a:as) (b:bs) = a:b:interleave as bs 

unroll :: Int -> [a] -> [a]
unroll n as = take n (cycle as)