module ServerInfra (
    initServer,
    ConnectionCallBack,
    NumBufferedConnectionsBeforeCallBack,
    sendToClient,
    getFromClient
) where
import Network.Socket
    ( socketToHandle,
      defaultHints,
      getAddrInfo,
      setSocketOption,
      accept,
      bind,
      listen,
      socket,
      defaultProtocol,
      AddrInfo(addrAddress, addrFlags, addrSocketType, addrFamily),
      AddrInfoFlag(AI_PASSIVE),
      SocketOption(ReuseAddr),
      Socket,
      SocketType(Stream) )
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.IO.Handle
    ( Handle, hSetBuffering, hGetLine, BufferMode(LineBuffering) )
import Control.Concurrent (forkIO)
import ServerMessages ( encodeServerMessage, ServerMessage )
import ClientMessages ( decodeClientMessage, ClientMessages )
import Types ( Port )
import GHC.IO.Handle.Text ( hPutStrLn )

type ConnectionCallBack = [Handle] -> IO ()
type NumBufferedConnectionsBeforeCallBack = Int

maxQueuedCons :: Int
maxQueuedCons = 1024 -- TODO: Verify if this constant matters

initServer :: Port -> NumBufferedConnectionsBeforeCallBack -> ConnectionCallBack -> IO ()
initServer portNumberAsStr numBufferedConnectionsBeforeCallBack callBack = do
    sock <- createSocket portNumberAsStr
    serverLoop sock numBufferedConnectionsBeforeCallBack callBack []

createSocket :: Port -> IO Socket
createSocket portNumberAsStr = do
    addrInfos <- getAddrInfo (Just defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}) Nothing (Just portNumberAsStr)
    sock <- socket (addrFamily (head addrInfos)) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress (head addrInfos))
    listen sock maxQueuedCons
    pure sock

serverLoop :: Socket -> NumBufferedConnectionsBeforeCallBack -> ConnectionCallBack -> [Handle] -> IO ()
serverLoop serverSock numBufferedConnectionsBeforeCallBack callBack queuedConnections  = do
    (clientSock, _) <- accept serverSock
    clientHandle <- socketToHandle clientSock ReadWriteMode
    hSetBuffering clientHandle LineBuffering
    remainingQueuedConections <- handleCallBack numBufferedConnectionsBeforeCallBack callBack (queuedConnections ++ [clientHandle])
    serverLoop serverSock numBufferedConnectionsBeforeCallBack callBack remainingQueuedConections

handleCallBack :: NumBufferedConnectionsBeforeCallBack -> ConnectionCallBack -> [Handle] -> IO [Handle]
handleCallBack numBufferedConnectionsBeforeCallBack callBack queuedConnections
    | length queuedConnections >= numBufferedConnectionsBeforeCallBack = do
        _ <- forkIO (callBack (take numBufferedConnectionsBeforeCallBack queuedConnections))
        handleCallBack numBufferedConnectionsBeforeCallBack callBack (drop numBufferedConnectionsBeforeCallBack queuedConnections)
    | otherwise = pure queuedConnections

sendToClient :: ServerMessage -> Handle -> IO ()
sendToClient s h = do
    hPutStrLn h (encodeServerMessage s)

getFromClient :: Handle -> IO (Maybe ClientMessages)
getFromClient h = do
    s <- hGetLine h
    pure (decodeClientMessage s)
