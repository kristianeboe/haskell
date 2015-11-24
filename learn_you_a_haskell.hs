boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

combineListsOfWords xs ys = [x ++" "++ y | x<-xs, y<-ys]

length' xs = sum [1 | _ <- xs]

-- These functions work if you "let" in ghci, but won't load :()
--triangles = [(a,b,c) | c<-[1..10], b<-[1..10],a<-[1..10]]
--rightTriangles = [(a,b,c) | c<-[1..10], b<-[1..c],a<-[1..b], (a^2)+(b^2)==(c^2)]
--rightTrianglesWith circumferance = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == circumferance]

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n*factorial (n-1)

head' :: [a] -> a
head' [] = error "Can't get head from empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "This is an empty list"
tell (x:[]) = "This list only has one lement, " ++ show x
tell (x:y:[]) =  "This list has two elements, " ++ show x ++ " and " ++ show y
tell (x:y:ys) = "This list has a length of "++show ((length ys)+2) ++", and the first two elements are "++ show x ++ " and " ++ show y

fibb :: (Integral a) => a -> a
fibb 0 = 0
fibb 1 = 1
fibb n = fibb (n-1) + fibb (n-2)

bmiCalc :: (RealFloat a) => a -> a -> a
bmiCalc h w = (w/(h*h))

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You are a whale, congratulations"

bmiFunc h w = bmiTell (bmiCalc h w)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' i _
  | i <= 0 = [] -- No otherwise means we will procede to next pattern
take' _ [] = []
take' i (x:xs) = x:take' (i-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <=x]
      biggerSorted = quicksort [a | a <- xs, a > x]
      in smallerSorted ++ [x] ++ biggerSorted


applyTwise :: (a -> a) -> a -> a
applyTwise f x = f (f x)

zipWith' :: (a->b->c) -> [a]-> [b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

largestDivisible :: (Integral a) => a -> a
largestDivisible a = head (filter p [100000,99999..])
  where p x = x `mod`a == 0

sumOfOddSquares = sum (takeWhile p (filter odd (map (^2) [1..])))
  where p x = x < 10000


-- fold action
maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

sqrtSums :: Int -> Int
sqrtSums n = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- sum (filter (>10) (map (*2)[2..10]))
-- sum $ filter (>10) $ map (*2) [2..10]
