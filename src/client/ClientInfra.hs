module ClientInfra where

import Network.Socket (HostName, AddrInfo (addrSocketType, addrFamily, addrAddress), getAddrInfo, defaultHints, SocketType (Stream), socket, setSocketOption, SocketOption (KeepAlive), connect, socketToHandle, Socket)
import Types (Port)
import Network.BSD
import GHC.IO.IOMode
import GHC.IO.Handle
import ClientMessages (ClientMessages, encodeClientMessage)
import ServerMessages (ServerMessage, decodeServerMessage)
import GHC.IO.Handle.Text (hPutStrLn)
import Text.Read

initClientSocket :: HostName -> Port -> IO Handle
initClientSocket h p = do
    createSocket h p

createSocket :: HostName -> Port -> IO Handle
createSocket h p = do
    addrInfos <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just h) (Just p)
    sock <- socket (addrFamily $ head addrInfos) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock $ addrAddress (head addrInfos)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    pure h

sendToServer :: ClientMessages -> Handle -> IO ()
sendToServer s h = do
    hPutStrLn h (encodeClientMessage s)

getFromServer :: Handle -> IO (Maybe ServerMessage)
getFromServer h = do
    s <- hGetLine h
    case readMaybe s of
        Nothing -> getFromServer h
        Just str -> pure (decodeServerMessage str)
