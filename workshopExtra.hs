--type Heap a = Empty | [a]


--heapInsert :: a -> Heap a -> Heap a
--heapInsert a Empty = [a]
--heapInsert a (x:xs) =

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap f (x:xs) = case f x of
    Nothing -> filterMap f xs
    Just y -> y:filterMap f xs


lst = [1,2,-3,4,-2]

sum' xs = foldl (+) 0 xs

product' xs = foldl (*) 1 xs

all_pos xs = all (<0) xs

some_not_pos xs = any (<0) xs

--length' xs = foldl (\_ -> (+1)) 0 xs

mapFoldr f xs = foldr ((:).f) [] xs

filterFoldr p xs = foldr (\x -> if p x then (x:) else id) [] xs

data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

testTree = Node (Node Empty 3 (Node Empty 4 Empty)) 5 (Node Empty 7 Empty)

mapTree :: (a->a) -> Tree a -> Tree a
mapTree f Empty = Empty
mapTree f (Node l v r) = Node (mapTree f l) (f v) (mapTree f r)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node l _ r) = 1 + max (treeHeight l) (treeHeight r)

numNodes :: Tree a -> Int
numNodes Empty = 0
numNodes (Node l _ r) = 1 + numNodes l + numNodes r

testListTree = Node (Node (Node Empty [1,2] Empty) [1,2,3] (Node Empty [1,2,3] Empty)) [1,2,3,4,5] (Node (Node Empty [1,2,3,4,5] Empty) [1,2,3] (Node Empty [1,2,3,4,5] Empty))

concatTreeOfLists :: Tree [a] -> [a]
concatTreeOfLists Empty = []
concatTreeOfLists (Node l xs r) = concatTreeOfLists l ++ xs ++ concatTreeOfLists r

sumElems :: Num a => Tree a -> a
sumElems Empty = 0
sumElems (Node l v r) = sumElems l + v + sumElems r

foldrTree :: (a->b->a->a) -> a -> Tree b -> a
foldrTree f base (Node l v r) = f (foldrTree f base l) v (foldrTree f base r)

treeMax :: Ord a => Tree a -> Maybe a
treeMax Empty = Nothing
treeMax (Node l v r) = max' (max' (treeMax l) (treeMax r)) (Just v)




max' :: Ord a => Maybe a -> Maybe a -> Maybe a
max' Nothing Nothing = Nothing
max' (Just a) Nothing = Just a
max' Nothing (Just b) = Just b
max' (Just a) (Just b)
    | a > b = Just a
    | otherwise = Just b
