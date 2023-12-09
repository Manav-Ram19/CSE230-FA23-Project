module GameServer (
    startGameServer,
    getGameUpdateFromPlayer,
    sendGameUpdateToPlayer
) where

import ServerInfra
import GHC.IO.Handle
import GHC.Conc
import Control.Concurrent.Async
import Types
import ServerMessages
import ClientMessages
import GameServerConfig

numPlayersPerGame :: Int
numPlayersPerGame = 2

type NewPlayer = Handle
type GameLoopCallBack = GameState -> IO GameState

startGameServer :: GameLoopCallBack -> IO ()
startGameServer gameloop = do
    initServer serverPort numPlayersPerGame (validateAndStartGame gameloop)

validateAndStartGame :: GameLoopCallBack -> [NewPlayer] -> IO ()
validateAndStartGame gameloop players
    | length players == numPlayersPerGame = execGame gameloop (head players) (head (tail players)) -- Implicit dry violation?
    | otherwise = do
        putStrLn ("error: Ending Server gracefully. Reason: Game started with " ++ show (length players) ++ " players. Expecting:" ++ show numPlayersPerGame)


execGame :: GameLoopCallBack -> NewPlayer -> NewPlayer -> IO ()
execGame gameloop p1Handle p2Handle = do
    -- Get players' boards
    b1Future <- async (getBoard True p1Handle)
    b2Future <- async (getBoard False p2Handle)
    b1 <- wait b1Future
    b2 <- wait b2Future
    let p1 = Player p1Handle b1
    let p2 = Player p2Handle b2
    let gameState = makeInitialGameState p1 p2
    p1Future <- async (sendToClient (SendShips (ships (board p2))) (handle p1))
    p2Future <- async (sendToClient (SendShips (ships (board p1))) (handle p2))
    wait p1Future
    wait p2Future
    {- Run Game Loop -}
    _ <- gameloop gameState
    {- Winning state is resolved on both sides locally -}
    hClose (handle p1) -- Close connections since game is over
    hClose (handle p2) -- Close connections since game is over
    {- Should we make these async calls? -}

getBoard :: IsPlayerOne -> NewPlayer -> IO Board
getBoard isplayerOne player = do
    sendToClient (GetShips isplayerOne) player
    response <- getFromClient player
    case response of
        Just (SetShips s) -> pure (Board s [])
        _ -> error "Invalid api call to Server. Expecting SetShips call."

makeInitialGameState :: Player -> Player -> GameState
makeInitialGameState p1 p2 = GameState p1 p2 Player1

getGameUpdateFromPlayer :: Player -> IO (Cell, GameTurn)
getGameUpdateFromPlayer p@(Player h _) = do
    clientMsg <- getFromClient h
    case clientMsg of
        Just (ClientStateUpdate c t) -> pure (c, t)
        _ -> error "Invalid message from client. Expecting cell."

sendGameUpdateToPlayer :: Player -> Cell -> GameTurn -> IO ()
sendGameUpdateToPlayer (Player h _) c t = do
    sendToClient (ServerStateUpdate c t) h