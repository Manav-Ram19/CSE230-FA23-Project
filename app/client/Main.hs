module Main where 

import Prelude 
import UI
import GameClient (getInitialGameState)
import ClientInfra (initClientSocket)
import GameServerConfig (serverPort)

main :: IO ()
main = do 
    h <- initClientSocket "127.0.0.1" serverPort
    igs <- getInitialGameState h
    startUI igs


