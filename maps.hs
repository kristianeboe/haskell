import qualified Data.Map as Map

al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

mapFromAl :: Ord k => [(k, v)] -> Map.Map k v
mapFromAl = Map.fromList

mapFold :: Ord k => [(k, v)] -> Map.Map k v
mapFold = foldl (\map (k, v) -> Map.insert k v map) Map.empty
