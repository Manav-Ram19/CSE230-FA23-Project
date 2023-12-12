module Types (
    numRows,
    numCols,
    numShipsPerPlayer,
    Cell (..),
    Ship,
    Board (..),
    Player,
    GameTurn (..),
    GameState (..),
    Port,
    LocalGameState (..),
    Server,
    ClientGameLoopCallBack
) where
import GHC.IO.Handle (Handle)


numRows :: Int
numRows = 10
numCols :: Int
numCols = 10
numShipsPerPlayer :: Int
numShipsPerPlayer = 5
data Cell = Cell {
    row :: Int,
    col :: Int
 } deriving(Eq, Show, Read)

type Ship = [Cell]

data Board = Board {
    ships :: [Ship],
    attackedCells :: [Cell]
} deriving (Show, Eq)

type Player = Handle

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
    turn :: GameTurn, 
    server :: Server
} deriving (Show, Eq)

type Server = Handle
type ClientGameLoopCallBack = (LocalGameState -> Server -> IO LocalGameState)

