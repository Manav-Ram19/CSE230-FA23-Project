module ServerMessages where
import Types (Cell, GameState, Ship, GameTurn)

type IsPlayerOne = Bool
data ServerMessage = 
    GetShips IsPlayerOne |
    SendShips [Ship] |
    ServerStateUpdate Cell GameTurn |
    EndGame

encodeServerMessage :: ServerMessage -> String
encodeServerMessage (GetShips b) = error "todo"
encodeServerMessage (SendShips s) = error "todo"
encodeServerMessage (ServerStateUpdate c t) = error "todo"
encodeServerMessage EndGame = error "todo"

decodeServerMessage :: String -> Maybe ServerMessage
decodeServerMessage s = error "todo"