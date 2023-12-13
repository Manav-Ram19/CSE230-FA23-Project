module GameServer (
    startGameServer,
    getGameUpdateFromPlayer,
    sendGameUpdateToPlayer
) where

import GHC.IO.Handle
    ( Handle,
      hClose,
      Handle,
      hSetBuffering,
      hGetLine,
      BufferMode(LineBuffering) )
import GHC.Conc ()
import Control.Concurrent.Async ( async, wait )
import Types
    ( Board(Board, ships),
      Cell,
      ServerGameState(GameState),
      GameTurn(Player1),
      Player,
      Port )
import ServerMessages
    ( ServerMessage(ServerStateUpdate, SendShips, GetShips),
      encodeServerMessage,
      ServerMessage )
import ClientMessages
    ( ClientMessages(ClientStateUpdate, SetShips),
      decodeClientMessage,
      ClientMessages )
import GameServerConfig ( serverPort )
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
import Control.Concurrent (forkIO)
import GHC.IO.Handle.Text ( hPutStrLn )

numPlayersPerGame :: Int
numPlayersPerGame = 2

type NewPlayer = Handle
type GameLoopCallBack = ServerGameState -> IO ServerGameState

startGameServer :: GameLoopCallBack -> IO ()
startGameServer gameloop = initServer serverPort numPlayersPerGame (validateAndStartGame gameloop)

validateAndStartGame :: GameLoopCallBack -> [NewPlayer] -> IO ()
validateAndStartGame gameloop players
    | length players == numPlayersPerGame = execGame gameloop (head players) (head (tail players)) -- Implicit dry violation?
    | otherwise = putStrLn ("error: Ending Server gracefully. Reason: Game started with " ++ show (length players) ++ " players. Expecting:" ++ show numPlayersPerGame)

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

makeInitialGameState :: Player -> Player -> ServerGameState
makeInitialGameState p1 p2 = GameState p1 p2 Player1

getGameUpdateFromPlayer :: Player -> IO (Cell, GameTurn)
getGameUpdateFromPlayer playerHandle = do
    clientMsg <- getFromClient playerHandle
    case clientMsg of
        Just (ClientStateUpdate c t) -> pure (c, t)
        _ -> error ("Invalid message from client. Expecting cell. Got: " ++ show clientMsg)

sendGameUpdateToPlayer :: Player -> Cell -> GameTurn -> IO ()
sendGameUpdateToPlayer playerHandle c t = sendToClient (ServerStateUpdate c t) playerHandle


--------- SERVER SOCKETS --------

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
sendToClient s h = hPutStrLn h (encodeServerMessage s)

getFromClient :: Handle -> IO (Maybe ClientMessages)
getFromClient h = do
    s <- hGetLine h
    pure (decodeClientMessage s)