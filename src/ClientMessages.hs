module ClientMessages where
import Types
import Text.Read (readMaybe)

data ClientMessages = 
    SetShips [Ship] |
    ClientStateUpdate Cell GameTurn
    deriving (Show, Read)

decodeClientMessage :: String -> Maybe ClientMessages
decodeClientMessage s = readMaybe s :: Maybe ClientMessages

encodeClientMessage :: ClientMessages -> String
encodeClientMessage msg = show msg
