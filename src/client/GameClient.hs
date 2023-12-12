module GameClient (
    getIsPlayerOne, getOpponentShips, getGameStateUpdate, sendGameStateUpdate, sendPlayerShips
) where
import ClientInfra ( getFromServer, sendToServer )
import GHC.IO.Handle ( hWaitForInput )
import ServerMessages
    ( ServerMessage(ServerStateUpdate, GetShips, SendShips) )
import Types ( Cell, GameTurn, Server, Ship )
import ClientMessages
    ( ClientMessages(SetShips, ClientStateUpdate) )

getIsPlayerOne :: Server -> IO Bool
getIsPlayerOne serverHandle = do
    response <- getFromServer serverHandle
    case response of
        Just (GetShips b) -> pure b
        _ -> error "Invalid api call to client. Expecting getShips call."


-- Non-Blocking Read
getOpponentShips :: Server -> IO (Maybe [Ship])
getOpponentShips h =
    do
        resExists <- hWaitForInput h 1 {- 1 ms delay -}
        if resExists then do
            response <- getFromServer h
            case response of
                Just (SendShips s) -> pure $ Just s
                _ -> error "Invalid api call to client. Expecting getOpponentShips call."
        else pure Nothing


getGameStateUpdate :: Server -> IO (Maybe (Cell, GameTurn))
getGameStateUpdate h =
    do
        resExists <- hWaitForInput h 1 {- 1 ms delay -}
        if resExists then do
            response <- getFromServer h
            case response of
                Just (ServerStateUpdate c t) -> pure $ Just (c, t)
                _ -> error "Invalid api call to client. Expecting getServerStatusUpdate call."
        else pure Nothing


sendGameStateUpdate :: Server -> Cell -> GameTurn -> IO ()
sendGameStateUpdate s c t = sendToServer (ClientStateUpdate c t) s

sendPlayerShips :: Server -> [Ship] -> IO ()
sendPlayerShips s ss = sendToServer (SetShips ss) s
