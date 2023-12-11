module Main where 

import Prelude 
import UI
import GameClient (getInitialGameState, isPlayerOne)
import ClientInfra (initClientSocket)
import GameServerConfig (serverPort)

main :: IO ()
main = do 
    h <- initClientSocket "127.0.0.1" serverPort
    isP1 <- isPlayerOne h
    startUI h isP1


