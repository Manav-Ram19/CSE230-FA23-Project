module UI
  ( startUI,
  )
where

import BattleShipClientLoop (checkForCollision, checkIfPlayerWon, findNextGameTurn, isCellChosenBefore, isInList, isSubset)
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import ClientInfra (sendToServer)
import ClientMessages (ClientMessages (SetShips))
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Conc (threadDelay)
import GameClient (getGameStateUpdate, getOpponentShips, sendGameStateUpdate)
import Graphics.Vty (brightBlack, brightCyan, brightGreen, brightRed, brightWhite, brightYellow)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (black)
import Types

-------------------- TYPES --------------------
data GameStateForUI =
  SetupGameStateForUI {
    _setupships :: [Ship],
    _setupserver :: Server,
    _setupcurrRow :: Int,
    _setupcurrCol :: Int,
    _setupcurrDirection :: KeyDirection,
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
      }

type Name = ()

data KeyDirection = Left | Right | Up | Down

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

data RemoteStatusUpdate = RemoteStatusUpdate

-------------------- DRAWS --------------------

drawCell :: (Char, Bool) -> Widget n
drawCell (c, highlighted) =
  overrideAttr B.borderAttr attr (B.border (str (" " ++ [renderChar c] ++ " ")))
  where
    -- if highlighted then withAttr (attrName "warning") (str (" " ++ "D" ++ " ")) else str (" " ++ [c] ++ " ")
    attr = if highlighted then highlight else getCorrectAttr c
    getCorrectAttr '.' = nothing
    getCorrectAttr 's' = ship
    getCorrectAttr 'x' = hit
    getCorrectAttr 'm' = miss
    getCorrectAttr _ = nothing
    renderChar '.' = ' '
    renderChar 's' = 'S'
    renderChar 'x' = 'X'
    renderChar 'm' = 'M'
    renderChar _ = ' '

makeEmptyBoard :: [String]
makeEmptyBoard = replicate 10 (replicate 10 '.')

makePlayerBoard :: Board -> [String]
makePlayerBoard b = addHitsToBoard b $ addMissesToBoard b $ addShipsToBoard b makeEmptyBoard

makeOpponentBoard :: Board -> [String]
makeOpponentBoard b = addHitsToBoard b $ addMissesToBoard b makeEmptyBoard

addShipsToBoard :: Board -> [String] -> [String]
addShipsToBoard b board = foldr (addCellToBoard 's') board (concat (ships b))

addHitsToBoard :: Board -> [String] -> [String]
addHitsToBoard b board = foldr (addCellToBoard 'x') board (filter (\cell -> isInList cell (attackedCells b)) (concat (ships b)))

addMissesToBoard :: Board -> [String] -> [String]
addMissesToBoard b board = foldr (addCellToBoard 'm') board (attackedCells b)

addCellToBoard :: Char -> Cell -> [String] -> [String]
addCellToBoard c (Cell row col) curBoard = modifyListAtInd row (modifyListAtInd col c (getElemAtInd row [] curBoard)) curBoard

modifyListAtInd :: Int -> a -> [a] -> [a]
modifyListAtInd ind newVal oldList = take ind oldList ++ [newVal] ++ drop (ind + 1) oldList

getElemAtInd :: Int -> a -> [a] -> a
getElemAtInd _ defaultVal [] = defaultVal
getElemAtInd 0 _ l = head l
getElemAtInd n defaultVal (_ : ls) = getElemAtInd (n - 1) defaultVal ls

drawGrid :: [[Char]] -> String -> [(Int, Int)] -> Widget n
drawGrid grid label highLightedCells =
  B.borderWithLabel (str label) $
    vBox $
      map (hBox . map drawCell) gridWithHighLightBool
  where
    gridWithHighLightBool = map convertRow gridWithRowId
    convertRow :: (Int, [(Int, Char)]) -> [(Char, Bool)]
    convertRow (_, []) = []
    convertRow (rowId, (colId, c) : ls) = (if isInList (rowId, colId) highLightedCells then (c, True) else (c, False)) : convertRow (rowId, ls)
    gridWithRowId = zip [0 .. (numRows - 1)] gridWithColId
    gridWithColId = map (zip [0 .. (numCols - 1)]) grid

drawGameTurn :: GameTurn -> Bool -> Widget n
drawGameTurn gt p1 =
  withBorderStyle BS.unicodeRounded $
    withBorderStyle BS.unicodeBold $
      B.border $
        vBox [padAll 1 (str (render myTurn))]
  where
    myTurn = isMyTurn p1 gt
    render False = "Please wait for your opponent to miss..."
    render True = "Please make a move..."

drawShipsRemaining :: [Ship] -> Widget n
drawShipsRemaining myShips =
  hLimit 30 $
  vLimit 3 $
  withBorderStyle BS.unicodeRounded $
    B.borderWithLabel (str "Ships Left") $
      hBox [fill ' ', str (show (numShipsPerPlayer - (length myShips))), fill ' ']
drawShipsRemaining _ = error "should not happen"



drawTutorial :: Widget n
drawTutorial =
  hLimit 50 $
  withBorderStyle BS.unicodeRounded $
        B.borderWithLabel (str "Tutorial") $
          vBox $
            map (\tup -> uncurry drawTutorialRow tup)
              [ ("Left", "←"),
                ("Up", "↑"),
                ("Right", "→"),
                ("Down", "↓"),
                ("Place Ship", "Enter"),
                ("Clockwise Rotate", "x"),
                ("CounterClockwise Rotate ", "y"),
                ("Quit", "q")
              ]
  where
    drawTutorialRow action key =
      (padRight Max $ padLeft (Pad 1) $ str action) <+> (padLeft Max $ padRight (Pad 1) $ str key)

drawTitle :: Widget n
drawTitle =
  overrideAttr B.borderAttr cyan $
    B.border $
      vBox [C.hCenter $ padAll 1 (str (battleshipText))]

drawEndGame :: Bool -> Widget n
drawEndGame False =
  overrideAttr B.borderAttr red $
    B.border $
      vBox [C.hCenter $ padAll 1 (str (youLoseText))]
drawEndGame True =
  overrideAttr B.borderAttr green $
    B.border $
      vBox [C.hCenter $ padAll 1 (str (youWinText))]

draw :: GameStateForUI -> [Widget a]
-- setup game state
draw (SetupGameStateForUI myShips _ curR curC curDir shipSize _ _) = [C.vCenter (C.hCenter drawTitle <=> C.hCenter grid <=> C.hCenter (drawShipsRemaining myShips) <=> C.hCenter drawTutorial)]
  where
    grid = hBox [drawGrid mb "My Board" (getPositionsFromStartDirAndLen (curR, curC) shipSize curDir)]
    mb = makePlayerBoard (Board myShips [])

-- end game state
draw (EndGameStateForUI isw) = [C.vCenter $ C.hCenter (drawEndGame isw)]
--  during game
draw (GameStateForUI lgs curRow curCol) = [C.vCenter $ drawTitle <=> C.hCenter grid <=> C.hCenter (padTop (Pad 4) turnBox)]
  where
    grid = hBox [drawGrid mb "My Board" [], drawGrid ob "Opponents Board" highlightedCells]
    mb = makePlayerBoard (myBoard lgs)
    ob = makeOpponentBoard (oppBoard lgs)
    turnBox = drawGameTurn (turn lgs) (amIP1 lgs)
    highlightedCells = [(curRow, curCol) | isMyTurn (amIP1 lgs) (turn lgs)]

-------------------- EVENTS -------------------

handleEvent :: BrickEvent Name RemoteStatusUpdate -> EventM n GameStateForUI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent e = do
  currState <- get
  case currState of
    SetupGameStateForUI {} -> handleEventSetupGame e
    GameStateForUI {} -> handleEventPlayGame e
    EndGameStateForUI _ -> pure ()

handleEventSetupGame :: BrickEvent Name RemoteStatusUpdate -> EventM n GameStateForUI ()
handleEventSetupGame e = do
  sgsui <- get
  eventHandler e sgsui
  where
    eventHandler :: BrickEvent Name RemoteStatusUpdate -> GameStateForUI -> EventM n GameStateForUI ()
    eventHandler(AppEvent RemoteStatusUpdate) sgsui@(SetupGameStateForUI ss s r c dir nss isP1 sent) = do
      if length ss < numShipsPerPlayer then pure ()
      else do
        -- Construct and return a GSUI
        -- Get ships from opponent
        maybeoppShips <- liftIO $ getOpponentShips s
        case maybeoppShips of
          Just oppShips -> do
          -- Create new local game state
            let lgs = LocalGameState (Board ss []) (Board oppShips []) isP1 Player1 s
            -- Create GSUI
            let gsui = GameStateForUI lgs 0 0
            put gsui
          _ -> pure ()
    eventHandler (VtyEvent (V.EvKey V.KUp [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UI.Up sgsui
    eventHandler (VtyEvent (V.EvKey V.KDown [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UI.Down sgsui
    eventHandler (VtyEvent (V.EvKey V.KLeft [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UI.Left sgsui
    eventHandler (VtyEvent (V.EvKey V.KRight [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UI.Right sgsui
    -- Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'z') [])) sgsui@(SetupGameStateForUI ss s r c dir nss isP1 sent) = do
      let newDir = case dir of
                    UI.Left -> UI.Up
                    UI.Up -> UI.Right
                    UI.Right -> UI.Down
                    UI.Down -> UI.Left
      let newSGSUI = SetupGameStateForUI ss s r c newDir nss isP1 sent
      put newSGSUI
    -- Anti-Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'x') [])) sgsui@(SetupGameStateForUI ss s r c dir nss isP1 sent) = do
      let newDir = case dir of
                    UI.Left -> UI.Down
                    UI.Up -> UI.Left
                    UI.Right -> UI.Up
                    UI.Down -> UI.Right
      let newSGSUI = SetupGameStateForUI ss s r c newDir nss isP1 sent
      put newSGSUI
    eventHandler (VtyEvent (V.EvKey V.KEnter [])) sgsui@(SetupGameStateForUI ss s r c dir nss isP1 sent) = do
      if length ss >= numShipsPerPlayer then pure ()
      else do
        -- Check if cell list crosses any boundaries
        if isShipPlacementOutOfBounds (r, c) nss dir then pure ()
        else do
        -- Generate cell list based on r c dir nss
          let newShipPlacement = map (uncurry Cell) (getPositionsFromStartDirAndLen (r,c) nss dir)
        -- Check if cell list has any cell already in ss
          let isIntersectingWithOtherShips = foldr (\cell acc -> acc || isInList cell (concat ss)) False newShipPlacement
          if isIntersectingWithOtherShips then pure ()
          else do
        -- Generate new sgsui
            let newShips = ss ++ [newShipPlacement]
            case numShipsToNextShipSize $ length newShips of
              Just nextShipSize -> put (SetupGameStateForUI newShips s r c dir nextShipSize isP1 sent)
              Nothing -> do
                -- No ships left to add, so send to server if i haven't sent before
                if not sent then do
                  liftIO $ sendToServer (SetShips newShips) s
                  put (SetupGameStateForUI newShips s r c dir 0 isP1 True)
                else
                  put (SetupGameStateForUI newShips s r c dir 0 isP1 sent)
    eventHandler _ _ = pure ()

isShipPlacementOutOfBounds:: (Int, Int) -> Int -> KeyDirection -> Bool
isShipPlacementOutOfBounds (startR, startC) shipLen UI.Up = startR - shipLen + 1 < 0
isShipPlacementOutOfBounds (startR, startC) shipLen UI.Down = startR + shipLen - 1 >= numRows
isShipPlacementOutOfBounds (startR, startC) shipLen UI.Left = startC - shipLen + 1 < 0
isShipPlacementOutOfBounds (startR, startC) shipLen UI.Right = startC + shipLen - 1 >= numCols

getPositionsFromStartDirAndLen :: (Int, Int) -> Int -> KeyDirection -> [(Int, Int)]
getPositionsFromStartDirAndLen _ 0 _ = []
getPositionsFromStartDirAndLen (r, c) l dir = (r, c) : getPositionsFromStartDirAndLen (nr, nc) (l - 1) dir
  where
    (nr, nc) = (updateRow r dir, updateCol c dir)
    updateRow row UI.Up = (row + numRows - 1) `mod` numRows
    updateRow row UI.Down = (row + 1) `mod` numRows
    updateRow row _ = row
    updateCol col UI.Left = (col + numCols - 1) `mod` numCols
    updateCol col UI.Right = (col + 1) `mod` numCols
    updateCol col _ = col

-- Dry violation
numShipsToNextShipSize :: Int -> Maybe Int
numShipsToNextShipSize n
  | n == 0 = Just 2
  | n == 1 = Just 3
  | n == 2 = Just 3
  | n == 3 = Just 4
  | n == 4 = Just 5
  | otherwise = Nothing

handleEventPlayGame :: BrickEvent Name RemoteStatusUpdate -> EventM n GameStateForUI ()
handleEventPlayGame (AppEvent RemoteStatusUpdate) = do
  currState <- get
  case currState of
    GameStateForUI lgs _ _ -> do
      unless (isMyTurn (amIP1 lgs) (turn lgs)) $ do
        uState <- liftIO (handleRemoteStatusUpdate currState)
        put uState
    _ -> pure ()
handleEventPlayGame e = do
  currState <- get
  case currState of
    GameStateForUI lgs _ _ -> do
      when (isMyTurn (amIP1 lgs) (turn lgs)) $ do
        uState <- liftIO (eventHandler e currState)
        put uState
    _ -> pure ()
  where
    eventHandler :: BrickEvent n e -> GameStateForUI -> IO GameStateForUI
    eventHandler (VtyEvent (V.EvKey V.KUp [])) gsui = pure (moveHighlight UI.Up gsui)
    eventHandler (VtyEvent (V.EvKey V.KDown [])) gsui = pure (moveHighlight UI.Down gsui)
    eventHandler (VtyEvent (V.EvKey V.KLeft [])) gsui = pure (moveHighlight UI.Left gsui)
    eventHandler (VtyEvent (V.EvKey V.KRight [])) gsui = pure (moveHighlight UI.Right gsui)
    eventHandler (VtyEvent (V.EvKey V.KEnter [])) gsui = handleEnter gsui
    eventHandler _ gsui = pure gsui

isMyTurn :: Bool -> GameTurn -> Bool
isMyTurn True Player1 = True
isMyTurn False Player2 = True
isMyTurn _ _ = False

-- uses direction to upate game state to move current highlighted cell
moveHighlight :: KeyDirection -> GameStateForUI -> GameStateForUI
moveHighlight UI.Up (GameStateForUI lgs curRow curCol) = GameStateForUI lgs ((curRow + numRows - 1) `mod` numRows) curCol
moveHighlight UI.Down (GameStateForUI lgs curRow curCol) = GameStateForUI lgs ((curRow + 1) `mod` numRows) curCol
moveHighlight UI.Left (GameStateForUI lgs curRow curCol) = GameStateForUI lgs curRow ((curCol + numCols - 1) `mod` numCols)
moveHighlight UI.Right (GameStateForUI lgs curRow curCol) = GameStateForUI lgs curRow ((curCol + 1) `mod` numCols)

moveHighlight UI.Up (SetupGameStateForUI ss s curRow curCol curDir nss isP1 sent) = SetupGameStateForUI ss s ((curRow + numRows - 1) `mod` numRows) curCol curDir nss isP1 sent
moveHighlight UI.Down (SetupGameStateForUI ss s curRow curCol curDir nss isP1 sent) = SetupGameStateForUI ss s ((curRow + 1) `mod` numRows) curCol curDir nss isP1 sent
moveHighlight UI.Left (SetupGameStateForUI ss s curRow curCol curDir nss isP1 sent) = SetupGameStateForUI ss s curRow ((curCol + numCols - 1) `mod` numCols) curDir nss isP1 sent
moveHighlight UI.Right (SetupGameStateForUI ss s curRow curCol curDir nss isP1 sent) = SetupGameStateForUI ss s curRow ((curCol + 1) `mod` numCols) curDir nss isP1 sent


moveHighlight _ gs = gs

handleEnter :: GameStateForUI -> IO GameStateForUI
handleEnter gs@(GameStateForUI lgs curRow curCol) = do
  if isCellChosenBefore (Cell curRow curCol) (oppBoard lgs)
    then pure gs
    else do
      let attackCell = Cell curRow curCol
      let opponentBoard = oppBoard lgs
      let isHit = checkForCollision attackCell (ships opponentBoard)
      let newAttackedCells = attackCell : attackedCells opponentBoard
      let isGameOver = checkIfPlayerWon newAttackedCells (ships opponentBoard)
      let newOpBoard = Board (ships opponentBoard) newAttackedCells
      let newTurn = findNextGameTurn isHit isGameOver (turn lgs)
      let newgs = LocalGameState (myBoard lgs) newOpBoard (amIP1 lgs) newTurn (server lgs)
      let newgsui = case newTurn of
            GameOver -> EndGameStateForUI (isWinner newgs)
            _ -> GameStateForUI newgs curRow curCol
      sendGameStateUpdate (server newgs) attackCell newTurn
      pure newgsui
handleEnter gs = pure gs

isWinner :: LocalGameState -> Bool
isWinner (LocalGameState _ opb _ GameOver _) = isSubset (concat $ ships opb) (attackedCells opb)
isWinner _ = False

handleRemoteStatusUpdate :: GameStateForUI -> IO GameStateForUI
handleRemoteStatusUpdate gsui@(GameStateForUI lgs r c) = do
  let s = server lgs
  let myb = myBoard lgs
  response <- getGameStateUpdate s
  case response of
    Just (myAttackedCell, turnUpdateFromOpponent) -> do
        let myNewBoard = Board (ships myb) (myAttackedCell : attackedCells myb)
        let newLocalGameState = LocalGameState myNewBoard (oppBoard lgs) (amIP1 lgs) turnUpdateFromOpponent (server lgs)
        let newgsui = case turnUpdateFromOpponent of
              GameOver -> EndGameStateForUI (isWinner newLocalGameState)
              _ -> GameStateForUI newLocalGameState r c
        pure newgsui
    Nothing -> pure gsui
handleRemoteStatusUpdate gs = pure gs

-------------------- APP --------------------

app :: App GameStateForUI RemoteStatusUpdate Name
app =
  App
    { appDraw = draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const myattrApp
    }

startUI :: Server -> Bool -> IO ()
startUI s isP1 = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan RemoteStatusUpdate
    threadDelay 100000

  _ <- customMain initialVty builder (Just chan) app (SetupGameStateForUI [] s 0 0 UI.Right 2 (isP1) False)
  putStrLn ""
