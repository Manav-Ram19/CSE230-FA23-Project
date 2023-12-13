import Common (modifyListAtInd, getElemAtInd, contains, containsAll)
import ClientMessages
import ServerMessages
import GameLogic
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.State
import System.IO
import Data.List (intersect)
import Types (numRows, numCols, Cell(..), Ship, GameTurn (Player1, Player2, GameOver))
import UIConst (Direction (..), GameStateForUI (..))
import GHC.IO.Handle

import Prelude hiding (Left, Right)
---------- COMMON ----------

prop_ModifyListAtInd :: Int -> Int -> [Int] -> Bool
prop_ModifyListAtInd ind newVal l =
    length newList == length l &&
    ((ind < 0 || ind >= length l) || (newList !! ind == newVal)) && 
    take ind newList == take ind l &&
    drop (ind+1) newList == drop (ind+1) l
    where
        newList = modifyListAtInd ind newVal l

prop_GetElemAtInd :: Int -> Int -> [Int] -> Bool
prop_GetElemAtInd ind defaultVal l =
    (not ((null l) || (ind < 0 || ind >= length l)) || (val == defaultVal)) &&
    (((null l) || (ind < 0 || ind >= length l)) || (val == l !! ind))
    where
        val = getElemAtInd ind defaultVal l

prop_Contains :: Int -> [Int] -> Bool
prop_Contains e l =
    contains e l == elem e l

prop_ContainsAll :: [Int] -> [Int] -> Bool
prop_ContainsAll l1 l2 =
    containsAll l1 l2 == ((l1 `intersect` l2) == l1)

---------- CLIENT MESSAGES ----------

prop_ClientMsgDecClientMsgEncSS :: Property
prop_ClientMsgDecClientMsgEncSS =
    forAll genClientMessageSS
    (\msg -> case decodeClientMessage $ encodeClientMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

genClientMessageSS :: Gen ClientMessages
genClientMessageSS = do
    ships <- genShips 
    return $ SetShips ships

prop_ClientMsgDecClientMsgEncCSU :: Property
prop_ClientMsgDecClientMsgEncCSU =
    forAll genClientMessageCSU
    (\msg -> case decodeClientMessage $ encodeClientMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

genClientMessageCSU :: Gen ClientMessages
genClientMessageCSU = do
    cell <- genCell
    ClientStateUpdate cell <$> genTurn

---------- SERVER MESSAGES ----------

prop_ServerMsgDecServerMsgEncGS :: Property
prop_ServerMsgDecServerMsgEncGS =
    forAll genServerMessageGS
    (\msg -> case decodeServerMessage $ encodeServerMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

genServerMessageGS :: Gen ServerMessage
genServerMessageGS = do
    isPlayerOne <- genBool
    return $ GetShips isPlayerOne

prop_ServerMsgDecServerMsgEncSS :: Property
prop_ServerMsgDecServerMsgEncSS =
    forAll genServerMessageSS
    (\msg -> case decodeServerMessage $ encodeServerMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

genServerMessageSS :: Gen ServerMessage
genServerMessageSS = do
    ships <- genShips 
    return $ SendShips ships

prop_ServerMsgDecServerMsgEncSSU :: Property
prop_ServerMsgDecServerMsgEncSSU =
    forAll genServerMessageSSU
    (\msg -> case decodeServerMessage $ encodeServerMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

genServerMessageSSU :: Gen ServerMessage
genServerMessageSSU = do
    cell <- genCell
    ServerStateUpdate cell <$> genTurn

prop_ServerMsgDecServerMsgEncEG :: Property
prop_ServerMsgDecServerMsgEncEG =
    forAll genServerMessageEG
    (\msg -> case decodeServerMessage $ encodeServerMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

genServerMessageEG :: Gen ServerMessage
genServerMessageEG = do
    return $ EndGame

---------- GAME LOGIC ----------

-- prop_AddShipSGSUI :: Property
-- prop_AddShipSGSUI = monadicIO $ do
--     -- server <- run $ genIOHandle
--     forAll (genGameStateForUISGSUI)
--     (\oldGS -> (oldGS == oldGS))
--         -- let newGS = addShip oldGS 
--         -- in assert $ newGS == oldGS)
        

-- genGameStateForUISGSUI :: Handle -> Gen GameStateForUI
-- genGameStateForUISGSUI = do
--     ships <- genShips
--     server <- run $ genIOHandle
--     row <- elements [0..(numRows-1)]
--     col <- elements [0..(numCols-1)]
--     dir <- genDirection
--     size <- elements [2..5]
--     isP1 <- genBool
--     sent <- genBool
--     return $ SetupGameStateForUI ships server row col dir size isP1 sent



---------- GENERATORS ----------

genIOHandle :: IO Handle
genIOHandle = openFile "./test/dummyTestFile" ReadMode

genDirection :: Gen Direction
genDirection = do
    d <- elements [0..3]
    case d of
        0 -> return $ Left
        1 -> return $ Right
        2 -> return $ Up
        3 -> return $ Down

genShips :: Gen [Ship]
genShips = do
    ship1 <- replicateM 2 genCell
    ship2 <- replicateM 3 genCell
    ship3 <- replicateM 3 genCell
    ship4 <- replicateM 4 genCell
    ship5 <- replicateM 5 genCell
    return [ship1, ship2, ship3, ship4, ship5]

genCell :: Gen Cell
genCell = do
    r <- elements [0..(numRows-1)]
    c <- elements [0 .. (numCols-1)]
    return $ Cell r c

genTurn :: Gen GameTurn
genTurn = do
    elements [Player1, Player2, GameOver]

genBool :: Gen Bool
genBool = choose (False, True)



---------- RUN TESTS ----------
main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncGS
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncSS
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncEG
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncSSU
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ClientMsgDecClientMsgEncSS
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ClientMsgDecClientMsgEncCSU
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ModifyListAtInd
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_GetElemAtInd
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_Contains
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_ContainsAll
    putStrLn "Done"
