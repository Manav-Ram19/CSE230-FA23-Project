module ClientInfra where

import Network.Socket (AddrInfo (addrSocketType, addrFamily, addrAddress), getAddrInfo, defaultHints, SocketType (Stream), socket, setSocketOption, SocketOption (KeepAlive), connect, socketToHandle)
import Types (Port)
import Network.BSD
import GHC.IO.IOMode
import GHC.IO.Handle
import ClientMessages (ClientMessages, encodeClientMessage)
import ServerMessages (ServerMessage, decodeServerMessage)
import GHC.IO.Handle.Text (hPutStrLn)

initClientSocket :: HostName -> Port -> IO Handle
initClientSocket h p = do
    createSocket h p

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
sendToServer s h = do
    hPutStrLn h (encodeClientMessage s)

getFromServer :: Handle -> IO (Maybe ServerMessage)
getFromServer h = do
    s <- hGetLine h
    pure (decodeServerMessage s)
