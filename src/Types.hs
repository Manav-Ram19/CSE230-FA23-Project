module Types (
    numRows,
    numCols,
    numShipsPerPlayer,
    Cell (..),
    Ship,
    Board (..),
    Player,
    GameTurn (..),
    ServerGameState (..),
    Port,
    Server,
    Direction (..),
    ClientGameState (..)
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

data Direction = Left | Right | Up | Down deriving (Eq, Show)

type Ship = [Cell]

data Board = Board {
    ships :: [Ship],
    attackedCells :: [Cell]
} deriving (Show, Eq)

type Player = Handle

data GameTurn = Player1 | Player2 | GameOver deriving(Eq, Show, Read)

data ServerGameState = GameState {
    player1 :: Player,
    player2 :: Player,
    gameTurn :: GameTurn
}

data ClientGameState = SetupGameState {
    _setupships :: [Ship],
    _setupcurrRow :: Int,
    _setupcurrCol :: Int,
    _setupcurrDirection :: Direction,
    _nextShipSize :: Int,
    _isP1 :: Bool
  } | GamePlayState {
    _myBoard :: Board,
    _oppBoard :: Board,
    _amIP1 :: Bool,
    _turn :: GameTurn,
    _currAttackRow :: Int,
    _currAttackCol :: Int
  } | EndGameState {
    _isWinner :: Bool
  } deriving (Eq, Show)

type Port = String

type Server = Handle

