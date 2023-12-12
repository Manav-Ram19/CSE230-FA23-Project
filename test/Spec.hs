import ClientMessages
import Test.QuickCheck
import Types (numRows, numCols, Cell(..), GameTurn (Player1, Player2, GameOver))
import Common (modifyListAtInd, contains, containsAll)

---------- CLIENT MESSAGES ----------

prop_ClientMsgDecClientMsgEnc :: Property
prop_ClientMsgDecClientMsgEnc =
    forAll genClientMessageCSU
    (\msg -> case decodeClientMessage $ encodeClientMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

genClientMessageCSU :: Gen ClientMessages
genClientMessageCSU = do
    cell <- genCell
    ClientStateUpdate cell <$> genTurn

genCell :: Gen Cell
genCell = do
    r <- elements [0..(numRows-1)]
    c <- elements [0 .. (numCols-1)]
    return $ Cell r c

genTurn :: Gen GameTurn
genTurn = do
    elements [Player1, Player2, GameOver]

---------- COMMON ----------

prop_ModifyListAtInd :: Int -> Int -> [Int] -> Bool
prop_ModifyListAtInd ind newVal l =
    length newList == length l &&
    take ind newList == take ind l &&
    drop (ind+1) newList == drop (ind+1) l
    where
        newList = modifyListAtInd ind newVal l

prop_Contains :: Int -> [Int] -> Bool
prop_Contains e l =
    contains e l == any (\x -> x == e) l

prop_ContainsAll :: [Int] -> [Int] -> Bool
prop_ContainsAll l1 l2 =
    containsAll l1 l2 ==
    foldr (\val acc -> acc && (freq l2 val > 0)) True l1
    where
        freq l e = length $ filter (\v -> v == e) l

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ClientMsgDecClientMsgEnc
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ModifyListAtInd
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_Contains
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ContainsAll
    putStrLn "Done"
