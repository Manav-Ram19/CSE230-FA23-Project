module ClientMessages where
import Types

data ClientMessages = 
    SetShips [Ship] |
    ClientStateUpdate Cell GameTurn

decodeClientMessage :: String -> Maybe ClientMessages
decodeClientMessage s = error "todo"

encodeClientMessage :: ClientMessages -> String
encodeClientMessage (SetShips s) = error "todo"
encodeClientMessage (ClientStateUpdate c t) = error "todo"