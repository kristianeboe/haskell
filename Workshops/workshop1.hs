xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor False False = False
xor True True = False

xor' a b = if a+b == 1 then True else False

zero = len []
one = len [1]
two = len [1,2]
three = len [1,2,3]
four = len [1,2,3,4]
len [] = 0
len (x:xs) = 1 + len xs

head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

append' :: [a] -> [a] -> [a]
append' [] ys = ys
append' (x:xs) ys = x :append' xs ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs : myReverse (init xs)

getNthElem :: Int -> [a] -> a
getNthElem n as = as !! n

getNthElem' :: Int -> [a] -> a
getNthElem' _ [] = error "noElement"
getNthElem' n (x:xs)
    | n == 0 = x
    | otherwise = getNthElem' (n-1) xs
