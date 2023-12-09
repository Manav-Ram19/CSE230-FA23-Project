module BattleShipServerLoop (startBattleShipServer) where
    
import Types
import GameServer

startBattleShipServer :: IO ()
startBattleShipServer = do
    startGameServer gameLoop

gameLoop :: GameState -> IO GameState
gameLoop gs@(GameState p1 p2 curTurn) =
    case curTurn of
        Player1 -> do
            (cell, newTurn) <- getGameUpdateFromPlayer p1
            sendGameUpdateToPlayer p2 cell newTurn
            gameLoop (GameState p1 p2 newTurn)
        Player2 -> do
            (cell, newTurn) <- getGameUpdateFromPlayer p2
            sendGameUpdateToPlayer p1 cell newTurn
            gameLoop (GameState p1 p2 newTurn)
        GameOver -> do
            pure gs

