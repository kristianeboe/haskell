all_pos :: (Ord a, Num a) => [a] -> Bool --All items are positive
all_pos [] = True
all_pos (x:xs) = x > 0 && all_pos xs


data Suit = Club | Diamond | Heart | Spade
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | Jack | Queen | King | Ace deriving (Show, Eq, Ord)
--data Card = Card Suit Rank deriving (Show, Eq, Ord)

biggerThanZero :: [Int] -> [Int]
biggerThanZero xs = [x | x<-xs, x > 0]

nonegs :: [Int] -> [Int]
nonegs [] = []
nonegs (x:xs)
  | x > 0 = x : nonegs xs
  | otherwise = nonegs xs

insertToList :: [Int] -> Int -> [Int]
insertToList [] x = [x]
insertToList xs i = smallerThan xs i ++ [i] ++ biggerThan xs i

smallerThan :: [Int] -> Int -> [Int]
smallerThan xs i = [x | x<-xs, x < i]

biggerThan :: [Int] -> Int -> [Int]
biggerThan xs i = [x | x<-xs, x >= i]

insertToList' :: Int -> [Int] -> [Int]
insertToList' elt [] = [elt]
insertToList' elt (e:es)
  | e < elt = e:insertToList' elt es
  | otherwise = elt:e:es

square x = x^2
hypotenuse = sqrt . sum . map square

countdown :: Int -> IO ()
countdown 0 = putStrLn "Done"
countdown n = putStrLn n countdown (n-1)


transpose :: [[a]] -> [[a]]
transpose [] = error "transpose of zero-height matrix"
transpose list@(xs:xss)
  | len > 0   = transpose' len list
  | otherwise = error "transpose of zero-width matrix"
  where len = length xs

transpose' len [] = replicate len []
transpose' len (xs:xss)
    | len == length xs = zipWith (:) xs (transpose' len xss)
    | otherwise = error "transpose of non-rectangular matrix"
