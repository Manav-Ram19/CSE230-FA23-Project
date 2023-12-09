module ServerInfra (
    initServer,
    ConnectionCallBack,
    NumBufferedConnectionsBeforeCallBack,
    sendToClient,
    getFromClient
) where
import Network.Socket
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.IO.Handle
import Control.Concurrent (forkIO)
import ServerMessages
import ClientMessages
import Text.Read
import Types
import GHC.IO.Handle.Text

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
    putStrLn s
    pure (decodeClientMessage s)
