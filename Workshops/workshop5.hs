maybeApply :: (a->b) -> Maybe a -> Maybe b
maybeApply _ Nothing = Nothing
maybeApply f (Just a) = Just $ f a

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] xs = []
zipWith' _ xs [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

linearEqn :: Num a => a -> a -> [a] -> [a]
linearEqn _ _ [] = []
linearEqn m n xs = map (multAdd m n) xs

multAdd :: Num a => a -> a -> a -> a
multAdd m n x = m*x + n

sqrtPM :: (Floating a, Ord a) => a -> [a]
sqrtPM x
  | x  > 0    = let y = sqrt x in [y, -y]
  | x == 0    = [0]
  | otherwise = []

allSqrts :: (Floating a, Ord a) => [a] -> [a]
allSqrts [] = []
allSqrts (x:xs) = sqrtPM x ++ allSqrts xs

sqrtOfPos :: (Floating a, Ord a) => [a] -> [a]
sqrtOfPos xs = map sqrt $ filter (>0) xs

sqrtOfPos' [] = []
sqrtOfPos' (x:xs)
    | x>0 = sqrt x : sqrtOfPos' xs
    | otherwise = sqrtOfPos' xs
