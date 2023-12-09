module GameServer (
    startGameServer,
    getGameUpdateFromPlayer,
    sendGameUpdateToPlayer
) where

import ServerInfra
import GHC.IO.Handle
import GHC.Conc
import Types
import ServerMessages
import ClientMessages
import GameServerConfig

numPlayersPerGame :: Int
numPlayersPerGame = 2

type NewPlayer = Handle
type NumPlayersPerGame = Int
type StartGameCallBack = [NewPlayer] -> IO ()
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
    b1 <- getBoard True p1Handle -- TODO: Make these async calls
    b2 <- getBoard False p2Handle -- TODO: Make these async calls
    -- TODO: If using async approach, then wait for both players' boards to be resolved
    let p1 = Player p1Handle b1
    let p2 = Player p2Handle b2
    let gameState = makeInitialGameState p1 p2
    -- TODO: Send opponent ship locations to both players
    sendToClient (SendShips $ ships $ board p2) (handle p1) -- TODO: Make these async calls
    sendToClient (SendShips $ ships $ board p1) (handle p2) -- TODO: Make these async calls
    -- TODO: If using async approach, then wait for both players' boards to be resolved
    {- Run Game Loop -}
    gameloop gameState
    {- Winning state is resolved on both sides locally -}
    hClose (handle p1) -- Close connections since game is over
    hClose (handle p2) -- Close connections since game is over
    {- Should we make these async calls? -}

getBoard :: IsPlayerOne -> NewPlayer -> IO Board
getBoard isplayerOne handle = do
    sendToClient (GetShips isplayerOne) handle
    response <- getFromClient handle
    case response of
        Nothing -> error "Invalid api call to Server. Expecting SetShips call."
        Just (SetShips s) -> pure (Board s [])

makeInitialGameState :: Player -> Player -> GameState
makeInitialGameState p1 p2 = GameState p1 p2 Player1

getGameUpdateFromPlayer :: Player -> IO (Cell, GameTurn)
getGameUpdateFromPlayer p@(Player h _) = do
    clientMsg <- getFromClient h
    case clientMsg of
        Nothing -> getGameUpdateFromPlayer p
        Just (ClientStateUpdate c t) -> pure (c, t)
        _ -> error "Invalid message from client. Expecting cell."

sendGameUpdateToPlayer :: Player -> Cell -> GameTurn -> IO ()
sendGameUpdateToPlayer p@(Player h _) c t = do
    sendToClient (ServerStateUpdate c t) h