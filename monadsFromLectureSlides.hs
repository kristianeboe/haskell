countDown :: Int -> IO ()
countDown 0 = putStrLn "Done"
countDown n = do
    putStrLn $ show n
    countDown $ n-1


data Mtree a = Mnode a [Mtree a] deriving Show

testTree = Mnode 1 [Mnode 2 [], Mnode 3 [Mnode 4 []]]

printMtree :: Show a => Mtree a -> IO ()
printMtree (Mnode a []) = putStrLn $ " " ++ show a
printMtree (Mnode a (x:xs)) = do
    putStrLn $ show a
    printMtree x
    printMtree (head xs)

type HTML = [HTMLelement]
data HTMLelement = HTMLtext String | HTMLp HTML
stripFontTags (x@(HTMLtext str):xs) = [x] ++ stripFontTags xs
