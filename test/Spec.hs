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

prop_ClientMsgDecClientMsgEncCSU :: Property
prop_ClientMsgDecClientMsgEncCSU =
    forAll genClientMessageCSU
    (\msg -> case decodeClientMessage $ encodeClientMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

---------- SERVER MESSAGES ----------

prop_ServerMsgDecServerMsgEncGS :: Property
prop_ServerMsgDecServerMsgEncGS =
    forAll genServerMessageGS
    (\msg -> case decodeServerMessage $ encodeServerMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

prop_ServerMsgDecServerMsgEncSS :: Property
prop_ServerMsgDecServerMsgEncSS =
    forAll genServerMessageSS
    (\msg -> case decodeServerMessage $ encodeServerMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

prop_ServerMsgDecServerMsgEncSSU :: Property
prop_ServerMsgDecServerMsgEncSSU =
    forAll genServerMessageSSU
    (\msg -> case decodeServerMessage $ encodeServerMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

prop_ServerMsgDecServerMsgEncEG :: Property
prop_ServerMsgDecServerMsgEncEG =
    forAll genServerMessageEG
    (\msg -> case decodeServerMessage $ encodeServerMessage msg of
        Nothing -> False
        Just msg' -> msg == msg')

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
        in newGS == oldGS
    )

prop_ExecOpponentTurn_SGSUI :: Property
prop_ExecOpponentTurn_SGSUI = 
    forAll genCell
    (\attackedCell ->
        (forAll genTurn
        (\turnUpdateFromOpponent ->
            (forAll genClientGameState_SGSUI
            (\oldGS -> 
                let newGS = execOpponentTurn attackedCell turnUpdateFromOpponent oldGS
                in newGS == oldGS
            ))
        ))
    )

prop_ExecOpponentTurn_GPS :: Property
prop_ExecOpponentTurn_GPS = 
    forAll genCell
    (\attackedCell ->
        (forAll genTurn
        (\turnUpdateFromOpponent ->
            (forAll genClientGameState_GPS
            (\oldGS@(GamePlayState oMyB oOppB oIsP1 oT oR oC) -> 
                let 
                    newGS = execOpponentTurn attackedCell turnUpdateFromOpponent oldGS
                    newAttackedCells = attackedCell : attackedCells oMyB
                in ( 
                    ((turnUpdateFromOpponent == GameOver) && (newGS == EndGameState True || newGS == EndGameState False)) || 
                    (oOppB == _oppBoard newGS) && (ships oMyB == ships (_myBoard newGS)) && 
                    (attackedCells (_myBoard newGS) == newAttackedCells) && (oIsP1 == _amIP1 newGS) && 
                    (_turn newGS == turnUpdateFromOpponent)
                )
            ))
        ))
    )

prop_ExecOpponentTurn_EGS :: Property
prop_ExecOpponentTurn_EGS = 
    forAll genCell
    (\attackedCell ->
        (forAll genTurn
        (\turnUpdateFromOpponent ->
            (forAll genClientGameState_EGS
            (\oldGS -> 
                let newGS = execOpponentTurn attackedCell turnUpdateFromOpponent oldGS
                in newGS == oldGS
            ))
        ))
    )        

---------- GAME LOGIC HELPERS ----------

prop_MoveSelectedCell_SGSUI :: Property
prop_MoveSelectedCell_SGSUI = 
    forAll genDirection
    (\dir ->
        (forAll genClientGameState_SGSUI
        (\oldGS@(SetupGameState oShips oR oC oDir oShipSize oIsP1) -> 
            let 
                ngs = moveSelectedCell dir oldGS
                newGS@(SetupGameState nShips nR nC nDir nShipSize nIsP1) = ngs
            in (Cell nR nC) == moveCell dir (Cell oR oC)
        ))
    )    

prop_MoveSelectedCell_GPS :: Property
prop_MoveSelectedCell_GPS = 
    forAll genDirection
    (\dir ->
        (forAll genClientGameState_GPS
        (\oldGS@(GamePlayState oMyB oOppB oIsP1 oT oR oC) -> 
            let 
                ngs = moveSelectedCell dir oldGS
                newGS@(GamePlayState nMyB nOppB nIsP1 nT nR nC) = ngs
            in (Cell nR nC) == moveCell dir (Cell oR oC)
        ))
    )    

prop_MoveSelectedCell_EGS :: Property
prop_MoveSelectedCell_EGS = 
    forAll genDirection
    (\dir ->
        (forAll genClientGameState_EGS
        (\oldGS -> 
            let newGS = moveSelectedCell dir oldGS
            in newGS == oldGS
        ))
    )

prop_MoveCell :: Property
prop_MoveCell = 
    forAll genDirection
    (\dir ->
        (forAll genCell
        (\oldCell@(Cell oR oC) -> 
            let newCell@(Cell nR nC) = moveCell dir oldCell
            in case dir of
                Left    -> (nR == oR) && ((nC == oC - 1) || (oC == 0 && nC == numCols - 1))
                Right   -> (nR == oR) && ((nC == oC + 1) || (nC == 0 && oC == numCols - 1))
                Up      -> (nC == oC) && ((nR == oR - 1) || (oR == 0 && nR == numRows - 1))
                Down    -> (nC == oC) && ((nR == oR + 1) || (nR == 0 && oR == numRows - 1))
        ))
    )

prop_IsShipCollidingWithExistingShip :: Property
prop_IsShipCollidingWithExistingShip = 
    forAll genShip
    (\ship ->
        (forAll genShips
        (\ships -> 
            let 
                collision = isShipCollidingWithExistingShip ship ships
                shipsCells = concat ships
            in  
                collision == ((ship `intersect` shipsCells) /= [])
        ))
    )

prop_IsShipPlacementOutOfBounds :: Property
prop_IsShipPlacementOutOfBounds = 
    forAll genInt
    (\startR ->
        (forAll genInt
        (\startC -> 
            (forAll genInt
            (\shipLen -> 
                (forAll genDirection
                (\dir -> 
                    let outOfBounds = isShipPlacementOutOfBounds (startR, startC) shipLen dir
                    in case dir of
                        Left    -> outOfBounds == (startC - shipLen + 1 < 0)
                        Right   -> outOfBounds == (startC + shipLen - 1 >= numCols)
                        Up      -> outOfBounds == (startR - shipLen + 1 < 0)
                        Down    -> outOfBounds == (startR + shipLen - 1 >= numRows)
                ))     
            ))    
        ))
    )

prop_GetPositionsFromStartDirAndLen :: Property
prop_GetPositionsFromStartDirAndLen = 
    forAll (genIntInRange 0 (numRows - 1))
    (\row ->
        (forAll (genIntInRange 0 (numCols - 1))
        (\col -> 
            (forAll genPosInt
            (\shipLen -> 
                (forAll genDirection
                (\dir -> 
                    let 
                        positions = getPositionsFromStartDirAndLen (row, col) shipLen dir
                        rows = map fst positions
                        cols = map snd positions
                    in case dir of
                        Left    -> (all (== head rows) (tail rows)) && (all (isConsecutive Left) (zip cols (drop 1 cols)))
                        Right   -> (all (== head rows) (tail rows)) && (all (isConsecutive Right) (zip cols (drop 1 cols)))
                        Up      -> (all (== head cols) (tail cols)) && (all (isConsecutive Up) (zip rows (drop 1 rows)))
                        Down    -> (all (== head cols) (tail cols)) && (all (isConsecutive Down) (zip rows (drop 1 rows)))
                ))     
            ))    
        ))
    )

---------- HELPERS ----------

isConsecutive :: Direction -> (Int, Int) -> Bool
isConsecutive Left (i, j)  = (j - i == -1) || (i == 0 && j == numCols - 1)
isConsecutive Right (i, j) = (j - i == 1) || (j == 0 && i == numCols - 1)
isConsecutive Up (i, j)    = (j - i == -1) || (i == 0 && j == numRows - 1)
isConsecutive Down (i, j)  = (j - i == 1) || (j == 0 && i == numRows - 1)

---------- GENERATORS ----------

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

genServerMessageGS :: Gen ServerMessage
genServerMessageGS = do
    isPlayerOne <- genBool
    return $ GetShips isPlayerOne

genServerMessageSS :: Gen ServerMessage
genServerMessageSS = do
    ships <- genShips 
    return $ SendShips ships

genServerMessageSSU :: Gen ServerMessage
genServerMessageSSU = do
    cell <- genCell
    ServerStateUpdate cell <$> genTurn

genServerMessageEG :: Gen ServerMessage
genServerMessageEG = do
    return $ EndGame

genClientMessageSS :: Gen ClientMessages
genClientMessageSS = do
    ships <- genShips 
    return $ SetShips ships

genClientMessageCSU :: Gen ClientMessages
genClientMessageCSU = do
    cell <- genCell
    ClientStateUpdate cell <$> genTurn

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
    ship1 <- genShipOfSizeN 2 
    ship2 <- genShipOfSizeN 3 
    ship3 <- genShipOfSizeN 3 
    ship4 <- genShipOfSizeN 4 
    ship5 <- genShipOfSizeN 5 
    return [ship1, ship2, ship3, ship4, ship5]

genShipOfSizeN :: Int -> Gen Ship
genShipOfSizeN n = do
    ship <- replicateM n genCell
    return ship

genShip :: Gen Ship
genShip = do
    n <- choose(2, 5)
    ship <- replicateM n genCell
    return ship

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

genInt :: Gen Int
genInt = arbitrary :: Gen Int

genPosInt :: Gen Int
genPosInt = abs `fmap` genInt `suchThat` (> 0)

genIntInRange :: Int -> Int -> Gen Int
genIntInRange lo hi = choose (lo, hi)


---------- RUN TESTS ----------
main :: IO ()
main = do
    putStr "prop_MoveCell: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_MoveCell
    putStr "prop_IsShipPlacementOutOfBounds: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_IsShipPlacementOutOfBounds
    putStr "prop_GetPositionsFromStartDirAndLen: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_GetPositionsFromStartDirAndLen
    putStr "prop_IsShipCollidingWithExistingShip: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_IsShipCollidingWithExistingShip
    putStr "prop_MoveSelectedCell_SGSUI: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_MoveSelectedCell_SGSUI
    putStr "prop_MoveSelectedCell_GPS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_MoveSelectedCell_GPS
    putStr "prop_MoveSelectedCell_EGS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_MoveSelectedCell_EGS
    putStr "prop_ExecOpponentTurn_SGSUI: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ExecOpponentTurn_SGSUI
    putStr "prop_ExecOpponentTurn_GPS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ExecOpponentTurn_GPS
    putStr "prop_ExecOpponentTurn_EGS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ExecOpponentTurn_EGS
    putStr "prop_AddShip_SGSUI: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_AddShip_SGSUI
    putStr "prop_AddShip_GPS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_AddShip_GPS
    putStr "prop_AddShip_EGS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_AddShip_EGS
    putStr "prop_ExecPlayerTurn_SGSUI: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ExecPlayerTurn_SGSUI
    putStr "prop_ExecPlayerTurn_GPS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ExecPlayerTurn_GPS
    putStr "prop_ExecPlayerTurn_EGS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncGS
    putStr "prop_ServerMsgDecServerMsgEncSS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncSS
    putStr "prop_ServerMsgDecServerMsgEncEG: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncEG
    putStr "prop_ServerMsgDecServerMsgEncSSU: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ServerMsgDecServerMsgEncSSU
    putStr "prop_ClientMsgDecClientMsgEncSS: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ClientMsgDecClientMsgEncSS
    putStr "prop_ClientMsgDecClientMsgEncCSU: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ClientMsgDecClientMsgEncCSU
    putStr "prop_ModifyListAtInd: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ModifyListAtInd
    putStr "prop_GetElemAtInd: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_GetElemAtInd
    putStr "prop_Contains: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_Contains
    putStr "prop_ContainsAll: "; quickCheckWith stdArgs { maxSuccess = 10000 } prop_ContainsAll
    putStr "Done"
