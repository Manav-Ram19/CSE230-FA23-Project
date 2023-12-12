module ClientNetwork (
    getIsPlayerOne, getOpponentShips, getGameStateUpdate, sendGameStateUpdate, sendPlayerShips, initClientSocket
) where
import ServerMessages
    ( ServerMessage(ServerStateUpdate, GetShips, SendShips),
      decodeServerMessage )
import Types ( Cell, GameTurn, Server, Ship, Port )
import ClientMessages
    ( ClientMessages(SetShips, ClientStateUpdate),
      encodeClientMessage )
import Network.Socket
    ( AddrInfo(addrSocketType),
      getAddrInfo,
      defaultHints,
      SocketType(..),
      AddrInfo(addrSocketType, addrFamily, addrAddress),
      socket,
      setSocketOption,
      SocketOption(KeepAlive),
      connect,
      socketToHandle )
import Network.BSD
import GHC.IO.IOMode
import GHC.IO.Handle
import GHC.IO.Handle.Text (hPutStrLn)

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

------------ LOWER LEVEL SOCKET APIS -----------

initClientSocket :: HostName -> Port -> IO Handle
initClientSocket = createSocket

createSocket :: HostName -> Port -> IO Handle
createSocket hostname p = do
    addrInfos <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just hostname) (Just p)
    sock <- socket (addrFamily (head addrInfos)) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress (head addrInfos))
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    pure h

sendToServer :: ClientMessages -> Handle -> IO ()
sendToServer s h = hPutStrLn h (encodeClientMessage s)

getFromServer :: Handle -> IO (Maybe ServerMessage)
getFromServer h = do
    s <- hGetLine h
    pure (decodeServerMessage s)
