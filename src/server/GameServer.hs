module GameServer (
    startGameServer,
    getGameUpdateFromPlayer,
    sendGameUpdateToPlayer
) where

import ServerInfra ( getFromClient, initServer, sendToClient )
import GHC.IO.Handle ( Handle, hClose )
import GHC.Conc ()
import Control.Concurrent.Async ( async, wait )
import Types
    ( Board(Board, ships),
      Cell,
      GameState(GameState),
      GameTurn(Player1),
      Player )
import ServerMessages
    (ServerMessage(ServerStateUpdate, SendShips, GetShips) )
import ClientMessages
    ( ClientMessages(ClientStateUpdate, SetShips) )
import GameServerConfig ( serverPort )

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
execGame gameloop p1 p2 = do
    -- Get players' boards
    b1Future <- async (getBoard True p1)
    b2Future <- async (getBoard False p2)
    b1 <- wait b1Future
    b2 <- wait b2Future
    let gameState = makeInitialGameState p1 p2
    p1Future <- async (sendToClient (SendShips (ships b2)) p1)
    p2Future <- async (sendToClient (SendShips (ships b1)) p2)
    wait p1Future
    wait p2Future
    {- Run Game Loop -}
    _ <- gameloop gameState
    {- Winning state is resolved on both sides locally -}
    hClose p1 -- Close connections since game is over
    hClose p2 -- Close connections since game is over
    {- Should we make these async calls? -}

getBoard :: Bool -> NewPlayer -> IO Board
getBoard isplayerOne player = do
    sendToClient (GetShips isplayerOne) player
    response <- getFromClient player
    case response of
        Just (SetShips s) -> pure (Board s [])
        _ -> error ("Invalid api call to Server. Expecting SetShips call. Got: " ++ show response)

makeInitialGameState :: Player -> Player -> GameState
makeInitialGameState p1 p2 = GameState p1 p2 Player1

getGameUpdateFromPlayer :: Player -> IO (Cell, GameTurn)
getGameUpdateFromPlayer playerHandle = do
    clientMsg <- getFromClient playerHandle
    case clientMsg of
        Just (ClientStateUpdate c t) -> pure (c, t)
        _ -> error ("Invalid message from client. Expecting cell. Got: " ++ show clientMsg)

sendGameUpdateToPlayer :: Player -> Cell -> GameTurn -> IO ()
sendGameUpdateToPlayer playerHandle c t = do
    sendToClient (ServerStateUpdate c t) playerHandle