module NintyNinequestions () where

import Data.List

myLast1 :: [a] -> a
myLast1 [x] = x
myLast1 (x:xs) = myLast1 xs

myLast :: [a] -> a
myLast a = last a

myButLast' :: [a] -> a
myButLast' a = reverse a !! 1

elementAt :: [a] -> Int -> a
elementAt [] _ = error "No elements in list"
elementAt xs n = xs !! (n-1)

myLength :: (Num n)=> [a] -> n
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

palindrome :: (Eq a) => [a] -> [a] -> Bool
palindrome xs ys = if xs == myReverse ys then True else False

--my_flatten

compress :: (Eq a) => [a] -> [a]
compress [x] = [x]
compress (x1:x2:xs) = if x1 /= x2 then x1:x2: compress xs else x1:compress xs
--compress xs = [x | x:x2<-x:x2:xs, x /=x2]

dupli :: [a]->[a]
dupli [] = []
dupli (x:xs) = x:x: dupli xs

--repli :: [a] -> int -> [a]
--repli

dropN :: [a] -> Int -> [a]
dropN xs n = [x | x<-xs, elemIndex x xs `mod` n /= 0]
