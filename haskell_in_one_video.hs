-- Comments
{-
  Multiline Comments
-}

import Data.List
import System.IO

sumOfNums :: Num a => [a] -> a
sumOfNums xs = sum xs

--All pattern
firstLetterInSentence :: [Char] -> String
firstLetterInSentence all@(x:xs) = "The first letter in " ++ all ++ " is " ++  x:[]

-- case example
getClass n = case n of
  5 -> "Kindergarten"
  10 -> "Sasdfasdfas"
  _ -> "Something else"

-- lambda
dbl1To10 = map (\x -> x*2) [1..10]

{-
typeclass
typeconstructor
Enumerated type

-}
data BaseballPlayer = Pitcher
                    | Catcher
                    | Infielder
                    | Outfield
                  deriving Show

data Customer = Customer {name::String, address::String, balance::Double} deriving Show

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b

data Shape = Circle Float Float Float | Rectange Float Float Float Float
    deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r^2
--area (Rectange x1 y1 x2 y2)

-- main = do
--     putStrLn = "What's your name?"
--     name <- getLine
--     putStrLn ("Hello" ++ name)
--

{-
important functions
fromInteger :: Num a => Integer -> a

:: (RealFrac a, Integral b) => a -> b
truncate
round
ceiling
floor
-}

{-
list functions
(++)
:
length
reverse
null : checks if list is empty
!!
head tail
init last
take drop
elem

Ranges
[1..100] :: (Num t, Enum t) => [t]
[2,4..20]
['A'..'Z']

Infinite lists
[1..]
repeat(inf), replicate a b
cycle
takeWhile

-}
