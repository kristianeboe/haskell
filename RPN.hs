import Data.Char

--solveRPN :: Num a => String -> [a] -> a
--solveRPN [] [x] = x
--solveRPN expr@(x:xs) stack@(s1:s2:ss)
--    | isDigit x = let num = fromIntegral (digitToInt x) in solveRPN xs (num:stack)
--    | x == '+' = let num = s1 + s2 in solveRPN xs (num:ss)
--

solveRPN :: => String -> Float
solveRPN head . foldl foldingfunction [] . words
    where foldingfunction (x:y:ys) "*" (x*y):ys
          foldingfunction (x:y:ys) "+" (x+y):ys
          foldingfunction (x:y:ys) "-" (x-y):ys
          foldingFunction (x:y:ys) "/" = (y / x):ys
          foldingfunction (x:y:ys) "^" (x**y):ys
          foldingFunction (x:xs) "ln" = log x:xs
          foldingFunction xs "sum" = [sum xs]
          foldingfunction xs numberString = read numberString:xs
