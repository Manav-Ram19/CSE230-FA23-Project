module Types where
import GHC.IO.Handle (Handle)


numRows :: Int
numRows = 10
numCols :: Int
numCols = 10
data Cell = Cell Int Int deriving(Eq, Show, Read)
type Ship = [Cell]

data Board = Board {
    ships :: [Ship],
    attackedCells :: [Cell]
} deriving (Show)


data Player = Player {
    handle :: Handle,
    board :: Board
}

data GameTurn = Player1 | Player2 | GameOver deriving(Eq, Show, Read)

data GameState = GameState {
    player1 :: Player,
    player2 :: Player,
    gameTurn :: GameTurn
}

type Port = String

data LocalGameState = LocalGameState {
    myBoard :: Board,
    oppBoard :: Board,
    amIP1 :: Bool,
    turn :: GameTurn
} deriving (Show)

type Server = Handle
type ClientGameLoopCallBack = (LocalGameState -> Server -> IO LocalGameState)

