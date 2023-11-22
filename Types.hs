module Types where
import GHC.IO.Handle (Handle)

data Cell = Cell Int Int deriving(Eq)
type Ship = [Cell]

data Board = Board {
    ships :: [Ship],
    attackedCells :: [Cell]
}

data Player = Player {
    handle :: Handle,
    board :: Board
}

data GameTurn = Player1 | Player2 | GameOver deriving(Eq)

data GameState = GameState {
    player1 :: Player,
    player2 :: Player,
    gameTurn :: GameTurn
}

type Port = String