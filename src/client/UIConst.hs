module UIConst (
 Name, GameStateForUI(..), RemoteStatusUpdate(..),  Direction(..),  highlight, hit, ship, nothing, cyan, red, green, miss, myattrApp, battleshipText, youLoseText, youWinText
)where
import Types (Ship, Server, LocalGameState)
import Brick (AttrName, AttrMap, attrMap)
import Brick.AttrMap (attrName)
import Graphics.Vty (brightWhite, black)
import Brick.Util (on, fg)
import Graphics.Vty.Attributes (brightYellow, brightRed, brightGreen, brightBlack, brightCyan)

---------- TYPES ----------
type Name = ()

data GameStateForUI =
  SetupGameStateForUI {
    _setupships :: [Ship],
    _setupserver :: Server,
    _setupcurrRow :: Int,
    _setupcurrCol :: Int,
    _setupcurrDirection :: Direction,
    _nextShipSize :: Int,
    _isP1 :: Bool,
    _sentShipsToServer :: Bool
  }
  | GameStateForUI
      { _localGameState :: LocalGameState,
        _currRow :: Int,
        _currCol :: Int
      }
  | EndGameStateForUI
      { _didClientWin :: Bool
      } deriving (Eq)

data Direction = Left | Right | Up | Down deriving (Eq)

data RemoteStatusUpdate = RemoteStatusUpdate

---------- CONST ----------

highlight :: AttrName
highlight = attrName "highlight"

hit :: AttrName
hit = attrName "hit"

ship :: AttrName
ship = attrName "ship"

nothing :: AttrName
nothing = attrName "nothing"

cyan :: AttrName
cyan = attrName "cyan"

red :: AttrName
red = attrName "red"

green :: AttrName
green = attrName "green"

miss :: AttrName
miss = attrName "miss"

myattrApp :: AttrMap
myattrApp =
  attrMap
    (brightWhite `on` black)
    [ (highlight, fg brightYellow),
      (hit, fg brightRed),
      (ship, fg brightGreen),
      (nothing, fg brightBlack),
      (cyan, fg brightCyan),
      (red, fg brightRed),
      (green, fg brightGreen),
      (miss, fg brightCyan),
      (nothing, fg brightBlack)
    ]

battleshipText :: String
battleshipText =
  "\n\
  \.______        ___   .___________.___________. __       _______     _______. __    __   __  .______   \n\
  \|   _  \\      /   \\  |           |           ||  |     |   ____|   /       ||  |  |  | |  | |   _  \n\
  \|  |_)  |    /  ^  \\ `---|  |----`---|  |----`|  |     |  |__     |   (----`|  |__|  | |  | |  |_)  | \n\
  \|   _  <    /  /_\\  \\    |  |        |  |     |  |     |   __|     \\   \\    |   __   | |  | |   ___/ \n\
  \|  |_)  |  /  _____  \\   |  |        |  |     |  `----.|  |____.----)   |   |  |  |  | |  | |  |      \n\
  \|______/  /__/     \\__\\  |__|        |__|     |_______||_______|_______/    |__|  |__| |__| | _|  \n"

youLoseText :: String
youLoseText =
  "\n\
  \____    ____  ______    __    __     __        ______        _______. _______ \n\
  \\\   \\  /   / /  __  \\  |  |  |  |   |  |      /  __  \\      \\/       ||   ____| \n\
  \ \\   \\/   / |  |  |  | |  |  |  |   |  |     |  |  |  |    |   (----`|  |__   \n\
  \  \\_    _/  |  |  |  | |  |  |  |   |  |     |  |  |  |     \\   \\    |   __|  \n\
  \    |  |    |  `--'  | |  `--'  |   |  `----.|  `--'  | .----)   |   |  |____ \n\
  \    |__|     \\______/   \\______/    |_______| \\______/  |_______/    |_______|\n"

youWinText :: String
youWinText =
  "\n\
  \____    ____  ______    __    __    ____    __    ____  __  .__   __. \n\
  \\\   \\  /   / /  __  \\  |  |  |  |   \\   \\  /  \\  /   / |  | |  \\ |  | \n\
  \ \\   \\/   / |  |  |  | |  |  |  |    \\   \\/    \\/   /  |  | |   \\|  | \n\
  \  \\_    _/  |  |  |  | |  |  |  |     \\            /   |  | |  . `  | \n\
  \    |  |    |  `--'  | |  `--'  |      \\    /\\    /    |  | |  |\\   | \n\
  \    |__|     \\______/   \\______/        \\__/  \\__/     |__| |__| \\__| "