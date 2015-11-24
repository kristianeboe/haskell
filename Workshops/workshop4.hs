lenSumSquareSum :: Num a=> [a] -> (Int, a, a)
lenSumSquareSum xs = (length xs, sum xs, sum $ map (^2) xs)

--lenSumSquareSum' :: (Num n, Fractional a)=> [n] -> (a, a, a)
--lenSumSquareSum' xs = lenSumSquareSum'' xs 0.0 0.0 0.0
--
--lenSumSquareSum'' :: (Num n, Fractional a) => [n] -> a -> a -> a -> (a, a, a)
--lenSumSquareSum'' [] l s sq = (l, s, sq)
--lenSumSquareSum'' (x:xs) l s sq = let len = l+1.0
--                                      su = s + x
--                                      sqe = sq + (x*x)
--                                  in lenSumSquareSum'' xs len su sqe

stats2 [] = (0,0,0)
stats2 (n:ns) =
	let (l,s,sq) = stats2 ns
	in (l+1, s+n, sq+n*n)

transList = [[1,2],[4,4],[8,9]]

--transpose' :: [[a]] -> [[a]]
--transpose' [[]] = error "!"
--transpose' [xs] = 
--transpose' (x@(y:ys):xs) = [y:he]--[y:transpose' xs] ++ transpose' ys


data Tree t = Leaf | Node t (Tree t) (Tree t) deriving (Eq, Show)

treeInsert :: Ord t => t -> Tree t -> Tree t
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node v l r)
    | x <= v = (Node v (treeInsert x l) r)
    | x  > v = (Node v l (treeInsert x r))

lst = [4,6,7,2,7,3,4,1,8]

treeSort :: Ord a => [a] -> [a]
treeSort xs = let tree = buildTree xs
                in extractValues tree

buildTree :: Ord a => [a] -> Tree a
buildTree xs = foldr treeInsert Leaf xs

extractValues :: Tree a -> [a]
extractValues Leaf = []
extractValues (Node v Leaf Leaf) = [v]
extractValues (Node v l r) = extractValues l ++ [v] ++ extractValues r

-- buildTree' :: Ord a => [a] -> Tree a -> Tree a
-- buildTree' (x:xs) tree =

insert :: Ord a => [a] -> Tree a -> Tree a
insert [] tree = tree
insert (x:xs) tree = insert xs (treeInsert x tree)
