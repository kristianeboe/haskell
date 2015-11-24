import Data.Char

maybe_tail :: [a] -> Maybe [a]
maybe_tail [] = Nothing
maybe_tail (_:xs) = Just xs

maybe_drop :: Int -> [a] -> Maybe [a]
maybe_drop n lst
  | n < 0 = Nothing
  | n == 0 = Just lst
  | otherwise = maybe_tail lst >>= maybe_drop (n-1)


lst = [1,2,3,4]

data Tree a = Empty | Node a (Tree a) (Tree a)
testTree = Node 5.0 (Node 3.0 (Node 1.0 Empty Empty) (Node 6.0 Empty Empty)) (Node 9.0 (Node 8.0 Empty Empty) (Node 10 Empty Empty))

print_tree :: Show a => Tree a -> IO ()
print_tree Empty = putStrLn "Empty tree"
print_tree (Node v Empty Empty) = putStrLn $ show v
print_tree (Node v l r) = do
    print_tree l
    putStrLn $ show v
    print_tree r

str_to_num :: String -> Maybe Int
str_to_num [] = Nothing
str_to_num xs = f 0 xs--(f (reverse xs) 1)

numStr = "1234"

f :: Int -> String -> Maybe Int
f val [] = Just val
f val (x:xs) = if isDigit x
                then f ((digitToInt x) + val*10)  xs
                else Nothing

sumFromLines :: IO Int
sumFromLines = do
    line <- getLine
    case str_to_num line of
        Nothing -> return 0
        Just num -> do
            sum' <- sumFromLines
            return (num+sum')

sumFromLines' :: IO Int
sumFromLines' = do
    nums <- getList
    return $ sum nums

getList :: IO [Int]
getList = do
    line <- getLine
    case str_to_num line of
        Nothing -> return []
        Just num -> do
            nums <- getList
            return $ [num] ++ nums
