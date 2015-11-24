module Proj1 (initialGuess, nextGuess, GameState) where
import Data.List

data GameState = GameState {possibleTargets ::[[String]], hits :: [String], useHits :: Bool}

initialGuess :: ([String], GameState)
initialGuess = (head poss, GameState {possibleTargets = tail poss, hits = [], useHits = False})
                where poss = allPossibilities

nextGuess :: ([String], GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (lastGuess, state) (0, 0, 0) = (head poss, GameState {possibleTargets = tail poss, hits = hits state, useHits = useHits state})
                                          where poss = pruneList (createCharList lastGuess) (possibleTargets state)
nextGuess (lastGuess, state) (0, _, 0) = (head poss, GameState {possibleTargets = tail poss, hits = hits state, useHits = useHits state})
                                          where poss = pruneList [last x | x<-lastGuess] (possibleTargets state)
nextGuess (lastGuess, state) (0, 0, _) = (head poss, GameState {possibleTargets = tail poss, hits = hits state, useHits = useHits state})
                                          where poss = pruneList [head x | x<-lastGuess] (possibleTargets state)
nextGuess (lastGuess, state) (a, _, _)
  | length hits' == 9 = (take 3 (nub hits'), GameState {possibleTargets = [drop 3 (hits')], hits = nub hits', useHits = True})
  | a > 0 = (head poss, GameState {possibleTargets = tail poss, hits = hits'})
    where poss = pruneElemList lastGuess $ possibleTargets state
          hits' = (hits state) ++ lastGuess
nextGuess (lastGuess, state) (_, _, _) = (head poss, GameState {possibleTargets = tail poss, hits = hits state})
                                          where poss = possibleTargets state

--nextGuess (lastGuess, state) (_, _, _) = (x, GameState {possibleChords = xs})
                                          --where x:xs = state possibleChords
createCharList as = nub $ concat [(head x) : (tail x) | x<-as]

prune :: Char -> [[String]] -> [[String]]
prune ele possibleTargets = filter (all (notElem ele)) possibleTargets

pruneElem :: String -> [[String]] -> [[String]]
pruneElem ele possibleChords = filter (notElem ele) possibleChords

pruneList :: [Char] -> [[String]] -> [[String]]
pruneList [] possibleChords = possibleChords
pruneList (x:xs) possibleChords = pruneList xs (prune x possibleChords)

pruneElemList :: [String] -> [[String]] -> [[String]]
pruneElemList [] possibleChords = possibleChords
pruneElemList (x:xs) possibleChords = pruneElemList xs (pruneElem x possibleChords)


allPossibilities :: [[String]]
allPossibilities = nub $ map sort poss
  where poss = [[x,y,z] | x<-as, y<-as, z<-as, length (nub [x,y,z]) == 3]
        as = [x:show y | x<-['A'..'G'], y<-[1,2,3]]
