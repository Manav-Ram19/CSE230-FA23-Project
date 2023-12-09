module BattleShipServerLoop (startBattleShipServer) where
    
import Types
import GameServer

startBattleShipServer :: IO ()
startBattleShipServer = do
    startGameServer gameLoop

gameLoop :: GameState -> IO GameState
gameLoop gs@(GameState p1 p2 turn) =
    case turn of
        Player1 -> do
            (cell, newTurn) <- getGameUpdateFromPlayer p1
            let newP2Board = Board (ships (board p2)) (cell: attackedCells (board p2))
            let newP2 = Player (handle p2) newP2Board
            sendGameUpdateToPlayer p2 cell newTurn
            gameLoop (GameState p1 newP2 newTurn)
        Player2 -> do
            (cell, newTurn) <- getGameUpdateFromPlayer p2
            let newP1Board = Board (ships (board p1)) (cell: attackedCells (board p1))
            let newP1 = Player (handle p1) newP1Board
            sendGameUpdateToPlayer p1 cell newTurn
            gameLoop (GameState newP1 p2 newTurn)
        GameOver -> do
            pure gs

