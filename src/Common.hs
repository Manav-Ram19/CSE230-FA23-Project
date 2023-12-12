module Common (
modifyListAtInd, getElemAtInd, isInList, isSubset
) where

---------- LISTS ----------

-- >>> modifyListAtInd 2 [1,2,3] [[], [4,5,6], [7,8,9]]
-- [[],[4,5,6],[1,2,3]]

modifyListAtInd :: Int -> a -> [a] -> [a]
modifyListAtInd ind newVal oldList = take ind oldList ++ [newVal] ++ drop (ind + 1) oldList

getElemAtInd :: Int -> a -> [a] -> a
getElemAtInd _ defaultVal [] = defaultVal
getElemAtInd 0 _ l = head l
getElemAtInd n defaultVal (_ : ls) = getElemAtInd (n - 1) defaultVal ls

isInList :: (Eq a) => a -> [a] -> Bool
isInList x = foldr (\ y -> (||) (x == y)) False

isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset [] _ = True
isSubset (x:xs) y = isInList x y && isSubset xs y
