ftoc :: Double -> Double
ftoc f = (5/9)*(f-32)

quadRoots :: Double -> Double -> Double -> [Double]
quadRoots a b c = ((-b+(sqrt ((b^2)-(4*a*c)))/2*a)) : [] ++ ((-b-(sqrt ((b^2)-(4*a*c)))/2*a)) : []

mergeSortedLists :: (Ord a) => [a] -> [a] -> [a]
mergeSortedLists a [] = a
mergeSortedLists [] a = a
mergeSortedLists (x:xs) (y:ys) = if x < y then x : mergeSortedLists (xs) (y:ys)
                                 else y : mergeSortedLists (x:xs) (ys)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
 let smallerSorted = quicksort [a | a <- xs, a <=x]
     biggerSorted = quicksort [a | a <- xs, a > x]
     in smallerSorted ++ [x] ++ biggerSorted

data Tree k v = Leaf | Node k v (Tree k v) (Tree k v) deriving (Eq, Show)

sameShape :: Tree a b -> Tree c d -> Bool
sameShape Leaf Leaf = True
sameShape Leaf (Node _ _ _ _) = False
sameShape (Node _ _ _ _) Leaf = False
sameShape (Node _ _ leftA rightA) (Node _ _ leftB rightB) = sameShape leftA leftB && sameShape rightA rightB

data Expression
       = Var Variable
       | Num Integer
       | Plus Expression Expression
       | Minus Expression Expression
       | Times Expression Expression
       | Div Expression Expression

data Variable = A | B

exp1 = Plus (Times (Num 2) (Var A)) (Var B)

eval :: Integer -> Integer -> Expression -> Integer
eval a b (Plus x y) = (eval a b x) + (eval a b y)
eval a b (Minus x y) = (eval a b x) - (eval a b y)
eval a b (Times x y) = (eval a b x) * (eval a b y)
eval a b (Div x y) = (eval a b x) `div` (eval a b y)
eval a b (Var A) = a
eval a b (Var B) = b
eval a b (Num i) = i
