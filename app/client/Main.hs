module Main (
    main
) where 

import Prelude 
import UI
import GameClient (getIsPlayerOne)
import ClientInfra (initClientSocket)
import GameServerConfig (serverPort, serverIP)

main :: IO ()
main = do 
    h <- initClientSocket serverIP serverPort
    isP1 <- getIsPlayerOne h
    startUI h isP1


