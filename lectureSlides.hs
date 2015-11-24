sumlist :: Num a => [a] -> a
sumlist xs = foldl (+) 0 xs

data LibraryItem = Book Int String String | Periodicals Int String Freq
data Freq = Days Int | Months Int

getTitle :: LibraryItem -> String
getTitle (Book _ title _) = title
getTitle (Periodicals _ title _) = title

-- data Tree = Leaf | Node String Int Tree Tree deriving Show

testTree = (Node "One" 1 (Node "Two" 2 (Node "Three" 3 Leaf Leaf) (Node "Four" 4 Leaf Leaf)) (Node "Five" 5 (Node "Six" 6 Leaf Leaf) (Node "Seven" 7 Leaf Leaf)))

countNodes :: Tree k v -> Int
countNodes Leaf = 1
countNodes (Node _ _ left right) = 1 + countNodes left + countNodes right

searchTree ::  Ord k => Tree k v -> k -> (Maybe v)
searchTree Leaf _ = Nothing
searchTree (Node k v l r) sk =
    if sk == k then
        Just v
    else if sk < k then
        searchTree l sk
    else
        searchTree r sk


insertIntoList :: Ord v => v -> [v] -> [v]
insertIntoList n [] = [n]
insertIntoList n (x:xs)
    | n < x = x: insertIntoList n xs
    | n >= x = x:n:xs

insertBst ::Ord k =>  k -> v -> Tree k v -> Tree k v
insertBst nk nv Leaf = Node nk nv Leaf Leaf
insertBst nk nv (Node k v left right)
    | nk < k = Node k v (insertBst nk nv left) right
    | otherwise = Node k v left (insertBst nk nv right)

data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)
