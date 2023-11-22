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
        Nothing -> error "todo"
        Just (GetShips b) -> pure b

getOpponentShips :: Server -> IO [Ship]
getOpponentShips h =
    do
        response <- getFromServer h
        case response of
            Nothing -> error "todo"
            Just (SendShips s) -> pure s

getGameStateUpdate :: Server -> IO (Cell, GameTurn)
getGameStateUpdate h =
    do
        response <- getFromServer h
        case response of
            Nothing -> error "todo"
            Just (ServerStateUpdate c t) -> pure (c, t)

getShipsFromClient :: IO [Ship]
getShipsFromClient = error "todo"

getCellFromClient :: IO Cell
getCellFromClient = error "todo"

showClient :: LocalGameState -> IO ()
showClient gs = error "todo"

