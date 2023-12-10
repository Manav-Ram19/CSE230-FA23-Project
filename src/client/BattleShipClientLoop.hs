module BattleShipClientLoop where

import Network.Socket
import GameServerConfig (serverPort)
import GameClient
import Types
import ClientInfra (initClientSocket)




startGame :: HostName -> IO ()
startGame hostName = do
    h <- initClientSocket hostName serverPort
    igs <- getInitialGameState h
    _ <- clientGameLoop igs h
    pure ()



clientGameLoop :: LocalGameState -> Server -> IO LocalGameState
clientGameLoop gs@(LocalGameState myb ob isP1 turn _server) server
    | (isP1 && (turn == Player1)) || (not isP1 && (turn == Player2)) = do
        (attackCell, b1, b2, newTurn) <- playTurn myb ob turn
        let newGs = LocalGameState b1 b2 isP1 newTurn _server
        sendGameStateUpdate server attackCell newTurn
        clientGameLoop newGs server
    | turn == GameOver = do
        showClient gs
        pure gs
    | otherwise = do
        (b1, b2, newTurn) <- opponentTurn server myb ob
        let newGs = LocalGameState b1 b2 isP1 newTurn _server
        showClient newGs
        clientGameLoop newGs server

opponentTurn :: Server -> Board -> Board -> IO (Board, Board, GameTurn)
opponentTurn server myb opb = do
    (myAttackedCell, turnUpdateFromOpponent) <- getGameStateUpdate server
    let myNewBoard = Board (ships myb) (myAttackedCell : attackedCells myb)
    pure (myNewBoard, opb, turnUpdateFromOpponent)

playTurn :: Board -> Board -> GameTurn -> IO (Cell, Board, Board, GameTurn)
playTurn myb opb curTurn = do
    attackCell <- getAttackFromPlayer opb
    let isHit = checkForCollision attackCell (ships opb)
    let newAttackedCells = attackCell: attackedCells opb
    let isGameOver = checkIfPlayerWon newAttackedCells (ships opb)
    let newOpB = Board (ships opb) newAttackedCells
    pure (attackCell, myb, newOpB, findNextGameTurn isHit isGameOver curTurn)

checkIfPlayerWon :: [Cell] -> [Ship] -> Bool
checkIfPlayerWon attackedcells opponentships = isSubset (concat opponentships) attackedcells

checkForCollision :: Cell -> [Ship] -> Bool
checkForCollision cell s = isInList cell (concat s)

findNextGameTurn :: Bool -> Bool -> GameTurn -> GameTurn
findNextGameTurn isHit isGameOver curTurn
    | isGameOver || (curTurn == GameOver) = GameOver
    | isHit = curTurn
    | curTurn == Player1 = Player2
    | otherwise = Player1

getAttackFromPlayer :: Board -> IO Cell
getAttackFromPlayer opBoard = do
    cell@(Cell row col) <- getCellFromClient
    if (row < 0 || row >= 10 || col < 0 || col >= 10) || isCellChosenBefore cell opBoard
        then getAttackFromPlayer opBoard
        else pure cell

isCellChosenBefore :: Cell -> Board -> Bool
isCellChosenBefore c (Board _ ac) = isInList c ac

isInList :: (Eq a) => a -> [a] -> Bool
isInList x = foldr (\ y -> (||) (x == y)) False

isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset [] _ = True
isSubset (x:xs) y = isInList x y && isSubset xs y


