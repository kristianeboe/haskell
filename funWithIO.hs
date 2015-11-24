--main = do
--    putStrLn "Hello, what is your name?"
--    name <- getLine
--    putStrLn ("Hey "++ name ++ ", you rock!")

--import Data.Char
--
--main = do
--    putStrLn "What's your first name?"
--    firstName <- getLine
--    putStrLn "What's your last name?"
--    lastName <- getLine
--    let bigFirstName = map toUpper firstName
--        bigLastName = map toUpper lastName
--    putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
--

-- Return
--main = do
--    let a = return "hell"
--        b = return "yeah"
--    putStrLn $ a ++ " " ++ b
--
--

-- Fun with files
--main = do
--    contents <- getContents
--    putStr (shortLinesOnly contents)
--
--shortLinesOnly :: String -> String
--shortLinesOnly input =
--    let allLines = lines input
--        shortLines = filter (\line -> length (line <10)) allLines
--        result = unlines shortLines
--    in result

respondPalindromes = unlines . map (\xs -> if isPalindrome xs
                                           then "palindrome"
                                           else "not a palindrome") . lines
                                           where isPalindrome xs = xs == reverse xs
