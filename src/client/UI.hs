module UI (
  startUI
) where

import Types

import BattleShipClientLoop (checkForCollision, checkIfPlayerWon, findNextGameTurn, isCellChosenBefore, isSubset, isInList)
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Monad.IO.Class (MonadIO (liftIO))
import GameClient (sendGameStateUpdate, getGameStateUpdate, getOpponentShips)
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever, when, unless)
import Control.Concurrent (forkIO)
import GHC.Conc (threadDelay)
import ClientInfra (sendToServer)
import ClientMessages (ClientMessages(SetShips))

-------------------- TYPES --------------------
data GameStateForUI =
  SetupGameStateForUI {
    _setupships :: [Ship],
    _setupserver :: Server,
    _setupcurrRow :: Int,
    _setupcurrCol :: Int,
    _setupcurrDirection :: KeyDirection,
    _nextShipSize :: Int,
    _isP1 :: Bool
  }
  | GameStateForUI
  { _localGameState :: LocalGameState,
    _currRow :: Int,
    _currCol :: Int
  }
  | EndGameStateForUI {
    _didClientWin :: Bool
  }

type Name = ()

data KeyDirection = Left | Right | Up | Down

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    []

data RemoteStatusUpdate = RemoteStatusUpdate

-------------------- DRAWS --------------------
drawCell :: (Char, Bool) -> Widget n
drawCell (c, highlighted) =
  if highlighted then str (" " ++ "D" ++ " ") else str (" " ++ [c] ++ " ")

makeBoard :: Board -> Bool -> [[Char]]
makeBoard b isOpponentBoard = boardWithAttacks
  where
    boardWithAttacks = foldr (addCellToBoard 'x') (if isOpponentBoard then newBoard else boardWithPlayerShips) (attackedCells b)
    boardWithPlayerShips = foldr (addCellToBoard 's') newBoard (concat (ships b))
    newBoard = replicate 10 (replicate 10 '.')
    addCellToBoard :: Char -> Cell -> [[Char]] -> [[Char]]
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
    gridWithRowId = zip [0 .. (numRows-1)] gridWithColId
    gridWithColId = map (zip [0 .. (numCols-1)]) grid

draw :: GameStateForUI -> [Widget a]
draw (SetupGameStateForUI myShips _ curR curC curDir shipSize _) = [C.vCenter (C.hCenter grid)]
  where
    grid = hBox [drawGrid mb "My Board" (getPositionsFromStartDirAndLen (curR, curC) shipSize curDir)]
    mb = makeBoard (Board myShips []) False
draw (EndGameStateForUI isw) = [str $ endGameMessage isw]
  where
    endGameMessage True = "You Won!"
    endGameMessage False = "You Lost."
draw (GameStateForUI lgs curRow curCol) = [C.vCenter $ C.hCenter grid]
  where
    grid = hBox [drawGrid mb "My Board" [], drawGrid ob "Opponents Board" [(curRow, curCol)]]
    mb = makeBoard (myBoard lgs) False
    ob = makeBoard (oppBoard lgs) True

-------------------- EVENTS -------------------

handleEvent :: BrickEvent Name RemoteStatusUpdate -> EventM n GameStateForUI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent e = do
  currState <- get
  case currState of
    SetupGameStateForUI {} -> handleEventSetupGame e
    GameStateForUI {}  -> handleEventPlayGame e
    EndGameStateForUI _ -> pure ()

handleEventSetupGame :: BrickEvent Name RemoteStatusUpdate -> EventM n GameStateForUI ()
handleEventSetupGame e = do
  sgsui <- get
  eventHandler e sgsui
  where
    eventHandler :: BrickEvent Name e -> GameStateForUI -> EventM n GameStateForUI ()
    eventHandler (VtyEvent (V.EvKey V.KUp [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UI.Up sgsui
    eventHandler (VtyEvent (V.EvKey V.KDown [])) sgsui@(SetupGameStateForUI {}) = do put $  moveHighlight UI.Down sgsui
    eventHandler (VtyEvent (V.EvKey V.KLeft [])) sgsui@(SetupGameStateForUI {}) = do put $  moveHighlight UI.Left sgsui
    eventHandler (VtyEvent (V.EvKey V.KRight [])) sgsui@(SetupGameStateForUI {}) = do put $  moveHighlight UI.Right sgsui
    -- Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'z') [])) sgsui@(SetupGameStateForUI ss s r c dir nss isP1) = do
      let newDir = case dir of
                    UI.Left -> UI.Up
                    UI.Up -> UI.Right
                    UI.Right -> UI.Down
                    UI.Down -> UI.Left
      let newSGSUI = SetupGameStateForUI ss s r c newDir nss isP1
      put newSGSUI
    -- Anti-Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'x') [])) sgsui@(SetupGameStateForUI ss s r c dir nss isP1) = do
      let newDir = case dir of
                    UI.Left -> UI.Down
                    UI.Up -> UI.Left
                    UI.Right -> UI.Up
                    UI.Down -> UI.Right
      let newSGSUI = SetupGameStateForUI ss s r c newDir nss isP1
      put newSGSUI
    eventHandler (VtyEvent (V.EvKey V.KEnter [])) sgsui@(SetupGameStateForUI ss s r c dir nss isP1) = do
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
            Just nextShipSize -> put (SetupGameStateForUI newShips s r c dir nextShipSize isP1)
            Nothing -> do
      -- Change to gsui if all ships have been placed
              -- Send ships to opponent
              _ <- liftIO $ sendToServer (SetShips newShips) s
              -- Get ships from opponent
              oppShips <- liftIO $ getOpponentShips s
              -- Create new local game state
              let lgs = LocalGameState (Board newShips []) (Board oppShips []) isP1 Player1 s
              -- Create GSUI
              let gsui = GameStateForUI lgs 0 0
              put gsui
    eventHandler _ _ = pure ()

isShipPlacementOutOfBounds:: (Int, Int) -> Int -> KeyDirection -> Bool
isShipPlacementOutOfBounds (startR, startC) shipLen UI.Up = startR - shipLen + 1 < 0
isShipPlacementOutOfBounds (startR, startC) shipLen UI.Down = startR + shipLen - 1 >= numRows
isShipPlacementOutOfBounds (startR, startC) shipLen UI.Left = startC - shipLen + 1 < 0
isShipPlacementOutOfBounds (startR, startC) shipLen UI.Right = startC + shipLen - 1 >= numCols

getPositionsFromStartDirAndLen :: (Int, Int) -> Int -> KeyDirection -> [(Int, Int)]
getPositionsFromStartDirAndLen _ 0 _ = []
getPositionsFromStartDirAndLen (r, c) l dir = (r, c):getPositionsFromStartDirAndLen (nr, nc) (l-1) dir
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
  -- | n == 2 = Just 3
  -- | n == 3 = Just 4
  -- | n == 4 = Just 5
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
    eventHandler (VtyEvent (V.EvKey V.KUp [])) gsui    = pure (moveHighlight UI.Up gsui)
    eventHandler (VtyEvent (V.EvKey V.KDown [])) gsui  = pure (moveHighlight UI.Down gsui)
    eventHandler (VtyEvent (V.EvKey V.KLeft [])) gsui  = pure (moveHighlight UI.Left gsui)
    eventHandler (VtyEvent (V.EvKey V.KRight [])) gsui = pure (moveHighlight UI.Right gsui)
    eventHandler (VtyEvent (V.EvKey V.KEnter [])) gsui = handleEnter gsui
    eventHandler _ gsui = pure gsui

isMyTurn :: Bool -> GameTurn -> Bool
isMyTurn True Player1 = True
isMyTurn False Player2 = True
isMyTurn _ _ = False

-- uses direction to upate game state to move current highlighted cell
-- TODO: Check why there is an extra row in output when moving highlight cell
moveHighlight :: KeyDirection -> GameStateForUI -> GameStateForUI
moveHighlight UI.Up (GameStateForUI lgs curRow curCol) = GameStateForUI lgs ((curRow + numRows - 1) `mod` numRows) curCol
moveHighlight UI.Down (GameStateForUI lgs curRow curCol) = GameStateForUI lgs ((curRow + 1) `mod` numRows) curCol
moveHighlight UI.Left (GameStateForUI lgs curRow curCol) = GameStateForUI lgs curRow ((curCol + numCols - 1) `mod` numCols)
moveHighlight UI.Right (GameStateForUI lgs curRow curCol) = GameStateForUI lgs curRow ((curCol + 1) `mod` numCols)

moveHighlight UI.Up (SetupGameStateForUI ss s curRow curCol curDir nss isP1) = SetupGameStateForUI ss s ((curRow + numRows - 1) `mod` numRows) curCol curDir nss isP1
moveHighlight UI.Down (SetupGameStateForUI ss s curRow curCol curDir nss isP1) = SetupGameStateForUI ss s ((curRow + 1) `mod` numRows) curCol curDir nss isP1
moveHighlight UI.Left (SetupGameStateForUI ss s curRow curCol curDir nss isP1) = SetupGameStateForUI ss s curRow ((curCol + numCols - 1) `mod` numCols) curDir nss isP1
moveHighlight UI.Right (SetupGameStateForUI ss s curRow curCol curDir nss isP1) = SetupGameStateForUI ss s curRow ((curCol + 1) `mod` numCols) curDir nss isP1


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
handleRemoteStatusUpdate (GameStateForUI lgs r c) = do
  let s = server lgs
  let myb = myBoard lgs
  (myAttackedCell, turnUpdateFromOpponent) <- getGameStateUpdate s
  let myNewBoard = Board (ships myb) (myAttackedCell : attackedCells myb)
  let newLocalGameState = LocalGameState myNewBoard (oppBoard lgs) (amIP1 lgs) turnUpdateFromOpponent (server lgs)
  let newgsui = case turnUpdateFromOpponent of
                      GameOver -> EndGameStateForUI (isWinner newLocalGameState)
                      _ -> GameStateForUI newLocalGameState r c
  pure newgsui
handleRemoteStatusUpdate gs = pure gs

-------------------- APP --------------------

app :: App GameStateForUI RemoteStatusUpdate Name
app =
  App
    { appDraw = draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const theMap
    }

startUI :: Server -> Bool -> IO ()
startUI s isP1 = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan RemoteStatusUpdate
    threadDelay 100000

  _ <- customMain initialVty builder (Just chan) app (SetupGameStateForUI [] s 0 0 UI.Right 2 (isP1))
  putStrLn ""
