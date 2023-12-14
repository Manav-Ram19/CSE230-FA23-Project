import Common (modifyListAtInd, getElemAtInd, contains, containsAll)
import ClientMessages
import ServerMessages
import GameLogic
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.State
import System.IO
import Data.List (intersect)
import Types 
-- import UIConst (Direction (..), GameStateForUI (..))
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

prop_AddShip_SGSUI :: Property
prop_AddShip_SGSUI = 
    forAll genClientGameState_SGSUI
    (\oldGS@(SetupGameState oSs oR oC oDir oCurShipSize oIsP1) -> 
        let 
            newGS@(SetupGameState nSs nR nC nDir nCurShipSize nIsP1) = addShip oldGS
            newShipPlacement = map (uncurry Cell) (getPositionsFromStartDirAndLen (oR, oC) oCurShipSize oDir)
        in (
            (length oSs >= numShipsPerPlayer || isShipPlacementOutOfBounds (oR, oC) oCurShipSize oDir ||
            foldr (\cell acc -> acc || contains cell (concat oSs)) False newShipPlacement ||
            ((length nSs) == (length oSs) + 1 && (nCurShipSize == oCurShipSize + 1 || oCurShipSize == 3) && nIsP1 == oIsP1)) &&
            (not (length oSs >= numShipsPerPlayer || isShipPlacementOutOfBounds (oR, oC) oCurShipSize oDir ||
            foldr (\cell acc -> acc || contains cell (concat oSs)) False newShipPlacement) ||
            (newGS == oldGS))
        ))

prop_AddShip_GPS :: Property
prop_AddShip_GPS = 
    forAll genClientGameState_GPS
    (\oldGS -> 
        let newGS = addShip oldGS
        in newGS == oldGS)

prop_AddShip_EGS :: Property
prop_AddShip_EGS = 
    forAll genClientGameState_EGS
    (\oldGS -> 
        let newGS = addShip oldGS
        in newGS == oldGS)

prop_ExecPlayerTurn_SGSUI :: Property
prop_ExecPlayerTurn_SGSUI = 
    forAll genClientGameState_SGSUI
    (\oldGS -> 
        let newGS = execPlayerTurn oldGS
        in newGS == oldGS)

prop_ExecPlayerTurn_GPS :: Property
prop_ExecPlayerTurn_GPS = 
    forAll genClientGameState_GPS
    (\oldGS@(GamePlayState oMyB oOppB oIsP1 oT oR oC) -> 
        let 
            newGS = execPlayerTurn oldGS
            attackCell = Cell oR oC
            newAttackedCells = attackCell : attackedCells oOppB
            isGameOver = checkIfPlayerWon newAttackedCells (ships oOppB)
            isHit = checkForCollision attackCell (ships oOppB) 
        in (
            ((isCellAttackedBefore (Cell oR oC) oOppB) && (oldGS == newGS)) || 
            ((isGameOver || oT == GameOver) && (newGS == EndGameState True || newGS == EndGameState False)) || 
            (oMyB == _myBoard newGS) && (ships oOppB == ships (_oppBoard newGS)) && 
            (attackedCells (_oppBoard newGS) == newAttackedCells) && (oIsP1 == _amIP1 newGS) && 
            ((isHit && (oT == _turn newGS)) || (not isHit && (oT /= _turn newGS)))
        )
    )

prop_ExecPlayerTurn_EGS :: Property
prop_ExecPlayerTurn_EGS = 
    forAll genClientGameState_EGS
    (\oldGS -> 
        let newGS = execPlayerTurn oldGS
        in newGS == oldGS)

genClientGameState_SGSUI :: Gen ClientGameState
genClientGameState_SGSUI = do
    ships <- genShips
    row <- elements [0..(numRows-1)]
    col <- elements [0..(numCols-1)]
    dir <- genDirection
    size <- elements [2..5]
    isP1 <- genBool
    return $ SetupGameState ships row col dir size isP1

genClientGameState_GPS :: Gen ClientGameState
genClientGameState_GPS = do
    myB <- genBoard
    oppB <- genBoard
    amIP1 <- genBool
    turn <- genTurn
    row <- elements [0..(numRows-1)]
    col <- elements [0..(numCols-1)]
    return $ GamePlayState myB oppB amIP1 turn row col

genClientGameState_EGS :: Gen ClientGameState
genClientGameState_EGS = do
    isWinner <- genBool
    return $ EndGameState isWinner

---------- GENERATORS ----------

genBoard :: Gen Board
genBoard = do
    ships <- genShips
    n <- choose(0, (numRows-1)*(numCols-1))
    attackedCells <- replicateM n genCell
    return $ Board ships attackedCells

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
    putStrLn "prop_AddShip_SGSUI"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_AddShip_SGSUI
    putStrLn "prop_AddShip_GPS"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_AddShip_GPS
    putStrLn "prop_AddShip_EGS"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_AddShip_EGS
    putStrLn "prop_ExecPlayerTurn_SGSUI"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ExecPlayerTurn_SGSUI
    putStrLn "prop_ExecPlayerTurn_GPS"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ExecPlayerTurn_GPS
    putStrLn "prop_ExecPlayerTurn_EGS"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncGS
    putStrLn "prop_ServerMsgDecServerMsgEncSS"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncSS
    putStrLn "prop_ServerMsgDecServerMsgEncEG"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncEG
    putStrLn "prop_ServerMsgDecServerMsgEncSSU"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncSSU
    putStrLn "prop_ClientMsgDecClientMsgEncSS"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ClientMsgDecClientMsgEncSS
    putStrLn "prop_ClientMsgDecClientMsgEncCSU"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ClientMsgDecClientMsgEncCSU
    putStrLn "prop_ModifyListAtInd"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ModifyListAtInd
    putStrLn "prop_GetElemAtInd"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_GetElemAtInd
    putStrLn "prop_Contains"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_Contains
    putStrLn "prop_ContainsAll"; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ContainsAll
    putStrLn "Done"
