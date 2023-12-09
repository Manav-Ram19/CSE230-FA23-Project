module GameClient where
import GameServerConfig (serverPort)
import Network.Socket
import ClientInfra
import GHC.IO.Handle
import ServerMessages
import Types
import ClientMessages

data LocalGameState = LocalGameState {
    myBoard :: Board,
    oppBoard :: Board,
    amIP1 :: Bool,
    gameTurn :: GameTurn
}

type Server = Handle
type ClientGameLoopCallBack = (LocalGameState -> Server -> IO LocalGameState)

startGameClient :: ClientGameLoopCallBack -> HostName -> IO ()
startGameClient callback h = do
    h <- initClientSocket h serverPort
    startGame callback h
    pure ()

startGame :: ClientGameLoopCallBack -> Handle -> IO ()
startGame callback h = do
    isP1 <- isPlayerOne h
    ships <- getShipsFromClient
    sendToServer (SetShips ships) h
    opponentShips <- getOpponentShips h
    let localGameState = LocalGameState (Board ships []) (Board opponentShips []) isP1 Player1
    let finalGameState = callback localGameState h
    pure ()

isPlayerOne :: Server -> IO Bool
isPlayerOne handle = do
    response <- getFromServer handle
    case response of
        Nothing -> error "Invalid api call to client. Expecting getShips call."
        Just (GetShips b) -> pure b

getOpponentShips :: Server -> IO [Ship]
getOpponentShips h =
    do
        response <- getFromServer h
        case response of
            Nothing -> error "Invalid api call to client. Expecting getOpponentShips call."
            Just (SendShips s) -> pure s

getGameStateUpdate :: Server -> IO (Cell, GameTurn)
getGameStateUpdate h =
    do
        response <- getFromServer h
        case response of
            Nothing -> error "Invalid api call to client. Expecting getServerStatusUpdate call."
            Just (ServerStateUpdate c t) -> pure (c, t)

sendGameStateUpdate :: Server -> Cell -> GameTurn -> IO ()
sendGameStateUpdate s c t = sendToServer (ClientStateUpdate c t) s

-- Interact with the view
getShipsFromClient :: IO [Ship]
getShipsFromClient = error "todo"

-- Interact with the view
getCellFromClient :: IO Cell
getCellFromClient = error "todo"

-- Interact with the view
showClient :: LocalGameState -> IO ()
showClient gs = error "todo"

