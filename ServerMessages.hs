module ServerMessages where
import Types

type IsPlayerOne = Bool
data ServerMessage =
    GetShips IsPlayerOne |
    SendShips [Ship] |
    ServerStateUpdate Cell GameTurn |
    EndGame deriving (Show)

-- >>> show (GetShips False)
-- "GetShips False"

-- Optimize for server status update header
encodeServerMessage :: ServerMessage -> String
encodeServerMessage sm@(GetShips True) = "a1"
encodeServerMessage sm@(GetShips False) = "a0"
encodeServerMessage sm@(SendShips s) = error "todo"
encodeServerMessage sm@(ServerStateUpdate c t) = "c" ++ show (cellToInt c) ++ turnToStr t
encodeServerMessage sm@EndGame = "d"

cellToInt :: Cell -> Int
cellToInt (Cell x y) = x*10 + y {- TODO: Discuss if this is a dry violation. -}

intToCell :: Int -> Cell
intToCell n = Cell (n `div` 10) (n `mod` 10) {- TODO: Discuss if this is a dry violation. -}

turnToStr :: GameTurn -> String
turnToStr Player1 = "a"
turnToStr Player2 = "b"
turnToStr GameOver = "c"

strToTurn :: String -> Maybe GameTurn
strToTurn "a" = Just Player1
strToTurn "b" = Just Player2
strToTurn "c" = Just GameOver
strToTurn _ = Nothing


decodeServerMessage :: String -> Maybe ServerMessage
decodeServerMessage s = error "todo"
