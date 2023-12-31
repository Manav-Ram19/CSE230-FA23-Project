module UIConst (
 Name, GameStateForPresenter(..), RemoteStatusUpdate(..),  invalidhighlight, highlight, hit, ship, nothing, cyan, red, green, miss, myattrApp, battleshipText, youLoseText, youWinText
)where
import Types (Server, ClientGameState)
import Brick (AttrName, AttrMap, attrMap)
import Brick.AttrMap (attrName)
import Graphics.Vty (brightWhite, black)
import Brick.Util (on, fg)
import Graphics.Vty.Attributes (brightYellow, brightRed, brightGreen, brightBlack, brightCyan)

---------- TYPES ----------
type Name = ()

data GameStateForPresenter = GameStateForPresenter {
  _clientGameState :: ClientGameState,
  _server :: Server
}  deriving (Eq)

data RemoteStatusUpdate = RemoteStatusUpdate

---------- CONST ----------

highlight :: AttrName
highlight = attrName "highlight"

invalidhighlight :: AttrName
invalidhighlight = attrName "invalidhighlight"

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
      (invalidhighlight, fg brightRed),
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