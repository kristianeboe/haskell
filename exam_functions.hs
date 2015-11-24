q2 _ _ [] = []
q2 a b (c:cs) =
  if a == c then b:(q2 a b cs)
  else c:(q2 a b cs)

q3 a (b:[]) = a b
q3 a (b1:b2:bs) = q3 a (b2:bs)

data Ttree t = Nil | Node t (Ttree t) (Ttree t) (Ttree t) deriving (Show, Eq, Read)

instance Foldable Ttree where
    foldMap f Nil = mempty
    foldMap f (Node t l r m) = foldMap f l `mappend`
                               foldMap f m `mappend`
                               f t         `mappend`
                               foldMap f r

testTree = Node 5.0 (Node 3.0 (Node 1.0 Nil Nil Nil) Nil (Node 6.0 Nil Nil Nil)) Nil (Node 9.0 (Node 8.0 Nil Nil Nil) Nil (Node 10 Nil Nil Nil))
testTree2 = Node 5.0 (Node 3.0 (Node 1.0 Nil Nil Nil) Nil (Node 6.0 Nil Nil Nil)) Nil (Node 9.0 (Node 8.0 Nil Nil Nil) Nil Nil)

testListTree = Node [1,2,3,4,5] (Node [1,2,3] (Node [1,2] Nil Nil Nil) Nil (Node [1,2,3] Nil Nil Nil)) Nil (Node [1,2,3] (Node [1,2,3,4,5] Nil Nil Nil) Nil (Node [1,2,3,4,5] Nil Nil Nil))

-- [1,2]
-- [1,2,3]
-- [1,2,3,4,5]
avg' :: Ttree Double -> Double
avg' tree = (foldl (+) 0 tree ) / (fromIntegral (length tree))

average :: (Show a, Fractional a) => Ttree a -> String--a
average tree = let (len, sums) = avg tree 0.0 0.0 in "len: " ++ show len ++ " sums: " ++ show sums--sums / len

avg :: Fractional a => Ttree a -> a -> a -> (a, a)
avg Nil len sums = (len, sums)
avg (Node v l m r) len sums = let leng = len + 1
                                  sumss = v + sums
                              in tupleSum (avg l leng sumss) (avg m leng sumss) (avg r leng sumss)

tupleSum :: Fractional a => (a, a) -> (a, a) -> (a,a) -> (a,a)
tupleSum (l1, s1) (l2, s2) (l3, s3) = (l1+l2+l3 , s1+s2+s3)




sameShape :: Ttree a -> Ttree b -> Bool
sameShape Nil Nil = True
sameShape Nil _ = False
sameShape _ Nil = False
sameShape (Node _ a b c) (Node _ d e f) = sameShape a d && sameShape b e && sameShape c f

shortestList :: Ord a => Ttree [a] -> ([a], [a])
shortestList Nil = ([], [])
shortestList tree = shortestAndConcat $ amassLists tree

amassLists :: Ttree [a] -> [[a]]
amassLists Nil = []
amassLists (Node v l m r) = [v] ++ amassLists l ++ amassLists m ++ amassLists r

shortestAndConcat :: Ord a => [[a]] -> ([a], [a])
shortestAndConcat xs = (minimum xs, concat xs)


subst :: Eq t => t -> t -> [t] -> [t]
subst _ _ [] = []
subst a b (c:cs)
    | a == c = b: subst a b cs
    | otherwise = c : subst a b cs

interleave :: [t] -> [t] -> [t]
interleave [] bs = bs
interleave as [] = as
interleave (a:as) (b:bs) = a:b: interleave as bs


unroll :: Int -> [a] -> [a]
unroll n as = take n $ cycle as

allPrimes :: [Integer]
allPrimes = primeFilter([2..])

primeFilter :: [Integer] -> [Integer]
primeFilter [] = []
primeFilter (x:xs) = x:(primeFilter (filterMultiples x xs))

filterMultiples :: Integer -> [Integer] -> [Integer]
filterMultiples _ [] = []
filterMultiples n (x:xs)
    | (mod x n) == 0 = filterMultiples n xs
    | otherwise = x:(filterMultiples n xs)
