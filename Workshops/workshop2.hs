factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

myElem :: Eq a => a -> [a] -> Bool
myElem i (x:xs)
  | i == x = True
  | xs == [] = False
  | otherwise = myElem i xs

longestPrefix :: Eq a => [a] -> [a] -> [a]
longestPrefix (x:xs) (y:ys)
  | x == y = x : longestPrefix xs ys
  | otherwise = []

minToMax ::(Enum a) => a -> a -> [a]
minToMax a b = [a..b]

--mccarthy_91 :: Int -> Int
--mccarthy_91 n
--  | n > 100   = n - 10
--  | otherwise = mccarthy_91 (mccarthy_91) (n + 11)

type FontTag = [Tag]
data Tag = Size Int | Face String | Color Color
data Color = Name String | Hex String | RGB Int Int Int

data FontTag' = FontTag' (Maybe Int) (Maybe String) (Maybe Color)
