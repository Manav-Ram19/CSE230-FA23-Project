module Common (
modifyListAtInd, getElemAtInd, contains, containsAll
) where

---------- LISTS ----------

-- >>> modifyListAtInd 2 [1,2,3] [[], [4,5,6], [7,8,9]]
-- [[],[4,5,6],[1,2,3]]

modifyListAtInd :: Int -> a -> [a] -> [a]
modifyListAtInd ind newVal oldList
    | ind >= 0 && ind < length oldList = take ind oldList ++ [newVal] ++ drop (ind + 1) oldList
    | otherwise = oldList

getElemAtInd :: Int -> a -> [a] -> a
getElemAtInd _ defaultVal [] = defaultVal
getElemAtInd 0 _ l = head l
getElemAtInd n defaultVal (_ : ls) = getElemAtInd (n - 1) defaultVal ls

contains :: (Eq a) => a -> [a] -> Bool
contains = elem 

containsAll :: (Eq a) => [a] -> [a] -> Bool
containsAll [] _ = True
containsAll (x:xs) y = contains x y && containsAll xs y
-- containsAll x y = ((x `intersect` y) == x)