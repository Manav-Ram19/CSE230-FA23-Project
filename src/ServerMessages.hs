module ServerMessages where
import Types
import Text.Read (readMaybe)

type IsPlayerOne = Bool
data ServerMessage =
    GetShips IsPlayerOne |
    SendShips [Ship] |
    ServerStateUpdate Cell GameTurn |
    EndGame
    deriving (Show, Read)

-- >>> show (GetShips False)
-- "GetShips False"

-- Optimize for server status update header
encodeServerMessage :: ServerMessage -> String
encodeServerMessage sm = show sm


decodeServerMessage :: String -> Maybe ServerMessage
decodeServerMessage s = readMaybe s :: Maybe ServerMessage
