{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module GameClient where
import GameServerConfig (serverPort)
import Network.Socket
import ClientInfra
import GHC.IO.Handle
import ServerMessages
import Types
import ClientMessages
import Text.Read (readMaybe)

data LocalGameState = LocalGameState {
    myBoard :: Board,
    oppBoard :: Board,
    amIP1 :: Bool,
    gameTurn :: GameTurn
}

type Server = Handle
type ClientGameLoopCallBack = (LocalGameState -> Server -> IO LocalGameState)

startGameClient :: ClientGameLoopCallBack -> HostName -> IO ()
startGameClient callback hostName = do
    h <- initClientSocket hostName serverPort
    startGame callback h
    pure ()

startGame :: ClientGameLoopCallBack -> Handle -> IO ()
startGame callback h = do
    isP1 <- isPlayerOne h
    playerShips <- getShipsFromClient
    sendToServer (SetShips playerShips) h
    opponentShips <- getOpponentShips h
    let localGameState = LocalGameState (Board playerShips []) (Board opponentShips []) isP1 Player1
    showClient localGameState
    let _ = callback localGameState h
    pure ()

isPlayerOne :: Server -> IO Bool
isPlayerOne serverHandle = do
    response <- getFromServer serverHandle
    case response of
        Just (GetShips b) -> pure b
        _ -> error "Invalid api call to client. Expecting getShips call."


getOpponentShips :: Server -> IO [Ship]
getOpponentShips h =
    do
        response <- getFromServer h
        case response of
            Just (SendShips s) -> pure s
            _ -> error "Invalid api call to client. Expecting getOpponentShips call."


getGameStateUpdate :: Server -> IO (Cell, GameTurn)
getGameStateUpdate h =
    do
        response <- getFromServer h
        case response of
            Just (ServerStateUpdate c t) -> pure (c, t)
            _ -> error "Invalid api call to client. Expecting getServerStatusUpdate call."


sendGameStateUpdate :: Server -> Cell -> GameTurn -> IO ()
sendGameStateUpdate s c t = sendToServer (ClientStateUpdate c t) s

-- TODO: Deprecate and Replace with Bricks
-- Interact with the view
{- Currently hardcoded to get game to run -}
getShipsFromClient :: IO [Ship]
getShipsFromClient = do
    pure [[Cell 0 0, Cell 0 1], [Cell 2 2, Cell 3 2, Cell 4 2], [Cell 7 7, Cell 6 7, Cell 5 7], [Cell 3 4, Cell 3 5, Cell 3 6, Cell 3 7], [Cell 9 1, Cell 9 2, Cell 9 3, Cell 9 4, Cell 9 5]]

-- TODO: Deprecate and Replace with Bricks
-- Interact with the view
getCellFromClient :: IO Cell
getCellFromClient = do
    row <- getRowFromUser
    col <- getColFromUser
    pure (Cell row col)
    where
        getRowFromUser :: IO Int
        getRowFromUser = do
            putStrLn "Enter Attack Row:"
            tmp <- getLine
            maybe getRowFromUser pure (readMaybe tmp :: Maybe Int)
        getColFromUser :: IO Int
        getColFromUser = do
            putStrLn "Enter Attack Col:"
            tmp <- getLine
            maybe getRowFromUser pure (readMaybe tmp :: Maybe Int)

-- TODO: Deprecate and Replace with Bricks
-- Interact with the view
showClient :: LocalGameState -> IO ()
showClient gs = do
    let playerBoard = makeBoard False (myBoard gs)
    let opponentBoard = makeBoard True (myBoard gs)
    putStrLn "Current Board:"
    let boardSep = "    |    "
    let outputList = addSep boardSep playerBoard opponentBoard
    putStrLn (concat outputList)
    where
        addSep :: String -> [String] -> [String] -> [String]
        addSep _ _ [] = []
        addSep _ [] _ = []
        addSep sep (l1:l1s) (l2:l2s) = (l1 ++ sep ++ l2 ++ "\n"):addSep sep l1s l2s


-- TODO: Deprecate and Replace with Bricks
makeBoard :: Bool -> Board -> [String]
makeBoard isOpponentBoard b = boardWithAttacks
    where
        boardWithAttacks = foldr (addCellToBoard 'x') (if isOpponentBoard then newBoard else boardWithPlayerShips) (attackedCells b)
        boardWithPlayerShips = foldr (addCellToBoard 's') newBoard (concat (ships b))
        newBoard = replicate 10 (replicate 10 '.')
        addCellToBoard :: Char -> Cell -> [[Char]] -> [[Char]]
        addCellToBoard c (Cell row col) curBoard = modifyListAtInd row (modifyListAtInd col c (getElemAtInd row [] curBoard)) curBoard
        modifyListAtInd :: Int -> a -> [a] -> [a]
        modifyListAtInd ind newVal oldList = take ind oldList ++ [newVal] ++ drop (ind+1) oldList
        getElemAtInd :: Int -> a -> [a] -> a
        getElemAtInd _ defaultVal [] = defaultVal
        getElemAtInd 0 _ l = head l
        getElemAtInd n defaultVal (_:ls) = getElemAtInd (n-1) defaultVal ls