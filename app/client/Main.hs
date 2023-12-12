module Main (
    main
) where 

import Prelude 
import Presenter (present)
import ClientNetwork (initClientSocket, getIsPlayerOne)
import GameServerConfig (serverPort, serverIP)

main :: IO ()
main = do 
    h <- initClientSocket serverIP serverPort
    isP1 <- getIsPlayerOne h
    present h isP1


