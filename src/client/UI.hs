module UI
  ( startUI,
  )
where

import GameLogic
    ( addShip,
      getPositionsFromStartDirAndLen,
      execPlayerTurn,
      execOpponentTurn )
import Common (modifyListAtInd, getElemAtInd, contains)
import UIConst (RemoteStatusUpdate (..), GameStateForUI (..), Name, Direction (..), myattrApp)
import UIDraw
    ( drawEndGamePhase, drawSetupPhase, drawGamePhase )
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Conc (threadDelay)
import GameClient (getGameStateUpdate, getOpponentShips, sendGameStateUpdate, sendPlayerShips)
import qualified Graphics.Vty as V
import Types
    ( numCols,
      numRows,
      numShipsPerPlayer,
      Board(..),
      Cell(Cell),
      GameTurn(..),
      LocalGameState(LocalGameState, server, myBoard, oppBoard, amIP1,
                     turn),
      Server )
import Brick
    ( BrickEvent(..),
      EventM,
      App(..),
      Widget,
      neverShowCursor,
      customMain )
import Brick.Main (halt)
import Brick.Types (get, put)

---------- ADAPTER FOR UI ----------

makeEmptyBoard :: [String]
makeEmptyBoard = replicate 10 (replicate 10 '.')

makePlayerBoard :: Board -> [String]
makePlayerBoard b = addHitsToBoard b $ addMissesToBoard b $ addShipsToBoard b makeEmptyBoard

makeOpponentBoard :: Board -> [String]
makeOpponentBoard b = addHitsToBoard b $ addMissesToBoard b makeEmptyBoard

addShipsToBoard :: Board -> [String] -> [String]
addShipsToBoard b board = foldr (addCellToBoard 's') board (concat (ships b))

addHitsToBoard :: Board -> [String] -> [String]
addHitsToBoard b board = foldr (addCellToBoard 'x') board (filter (\cell -> contains cell (attackedCells b)) (concat (ships b)))

addMissesToBoard :: Board -> [String] -> [String]
addMissesToBoard b board = foldr (addCellToBoard 'm') board (attackedCells b)

addCellToBoard :: Char -> Cell -> [String] -> [String]
addCellToBoard ch (Cell r c) curBoard = modifyListAtInd r (modifyListAtInd c ch (getElemAtInd r [] curBoard)) curBoard

---------- BRICK DRAW ----------

draw :: GameStateForUI -> [Widget a]
-- setup game state
draw (SetupGameStateForUI myShips _ curR curC curDir shipSize _ _) = drawSetupPhase mb hcells numShipsRemaining
  where
    numShipsRemaining = numShipsPerPlayer - length myShips
    hcells = getPositionsFromStartDirAndLen (curR, curC) shipSize curDir
    mb = makePlayerBoard (Board myShips [])
-- end game state
draw (EndGameStateForUI isw) = drawEndGamePhase isw
--  during game
draw (GameStateForUI lgs curRow curCol) = drawGamePhase mb ob highlightedCells isPTurn
  where
    mb = makePlayerBoard (myBoard lgs)
    ob = makeOpponentBoard (oppBoard lgs)
    isPTurn = isMyTurn (amIP1 lgs) (turn lgs)
    highlightedCells = [(curRow, curCol) | isPTurn]

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
    eventHandler(AppEvent RemoteStatusUpdate) sgsui@(SetupGameStateForUI {}) = do
      if length (_setupships sgsui) < numShipsPerPlayer then pure ()
      else do
        maybeoppShips <- liftIO $ getOpponentShips (_setupserver sgsui)
        case maybeoppShips of
          Just oppShips -> do
            let lgs = LocalGameState (Board (_setupships sgsui) []) (Board oppShips []) (_isP1 sgsui) Player1 (_setupserver sgsui)
            let gsui = GameStateForUI lgs 0 0
            put gsui
          _ -> pure ()
    eventHandler (VtyEvent (V.EvKey V.KUp [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UIConst.Up sgsui
    eventHandler (VtyEvent (V.EvKey V.KDown [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UIConst.Down sgsui
    eventHandler (VtyEvent (V.EvKey V.KLeft [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UIConst.Left sgsui
    eventHandler (VtyEvent (V.EvKey V.KRight [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UIConst.Right sgsui
    -- Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'z') [])) (SetupGameStateForUI ss s r c dir nss isP1 sent) = do
      let newDir = case dir of
                    UIConst.Left -> UIConst.Up
                    UIConst.Up -> UIConst.Right
                    UIConst.Right -> UIConst.Down
                    UIConst.Down -> UIConst.Left
      let newSGSUI = SetupGameStateForUI ss s r c newDir nss isP1 sent
      put newSGSUI
    -- Anti-Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'x') [])) (SetupGameStateForUI ss s r c dir nss isP1 sent) = do
      let newDir = case dir of
                    UIConst.Left -> UIConst.Down
                    UIConst.Up -> UIConst.Left
                    UIConst.Right -> UIConst.Up
                    UIConst.Down -> UIConst.Right
      let newSGSUI = SetupGameStateForUI ss s r c newDir nss isP1 sent
      put newSGSUI
    eventHandler (VtyEvent (V.EvKey V.KEnter [])) sgsui@(SetupGameStateForUI _ s r c dir _ isP1 sent) = do
      if sent then pure ()
      else do
        let newsgsui = addShip sgsui
        if length (_setupships newsgsui) < numShipsPerPlayer then put newsgsui
        else do
          liftIO $ sendPlayerShips s (_setupships newsgsui)
          put $ SetupGameStateForUI (_setupships newsgsui) s r c dir (_nextShipSize newsgsui) isP1 True
    eventHandler _ _ = pure ()

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
    eventHandler (VtyEvent (V.EvKey V.KUp [])) gsui = pure (moveHighlight UIConst.Up gsui)
    eventHandler (VtyEvent (V.EvKey V.KDown [])) gsui = pure (moveHighlight UIConst.Down gsui)
    eventHandler (VtyEvent (V.EvKey V.KLeft [])) gsui = pure (moveHighlight UIConst.Left gsui)
    eventHandler (VtyEvent (V.EvKey V.KRight [])) gsui = pure (moveHighlight UIConst.Right gsui)
    eventHandler (VtyEvent (V.EvKey V.KEnter [])) gsui = handleEnter gsui
    eventHandler _ gsui = pure gsui

isMyTurn :: Bool -> GameTurn -> Bool
isMyTurn True Player1 = True
isMyTurn False Player2 = True
isMyTurn _ _ = False

-- uses direction to upate game state to move current highlighted cell
moveHighlight :: Direction -> GameStateForUI -> GameStateForUI
moveHighlight UIConst.Up (GameStateForUI lgs curRow curCol) = GameStateForUI lgs ((curRow + numRows - 1) `mod` numRows) curCol
moveHighlight UIConst.Down (GameStateForUI lgs curRow curCol) = GameStateForUI lgs ((curRow + 1) `mod` numRows) curCol
moveHighlight UIConst.Left (GameStateForUI lgs curRow curCol) = GameStateForUI lgs curRow ((curCol + numCols - 1) `mod` numCols)
moveHighlight UIConst.Right (GameStateForUI lgs curRow curCol) = GameStateForUI lgs curRow ((curCol + 1) `mod` numCols)

moveHighlight UIConst.Up (SetupGameStateForUI ss s curRow curCol curDir nss isP1 sent) = SetupGameStateForUI ss s ((curRow + numRows - 1) `mod` numRows) curCol curDir nss isP1 sent
moveHighlight UIConst.Down (SetupGameStateForUI ss s curRow curCol curDir nss isP1 sent) = SetupGameStateForUI ss s ((curRow + 1) `mod` numRows) curCol curDir nss isP1 sent
moveHighlight UIConst.Left (SetupGameStateForUI ss s curRow curCol curDir nss isP1 sent) = SetupGameStateForUI ss s curRow ((curCol + numCols - 1) `mod` numCols) curDir nss isP1 sent
moveHighlight UIConst.Right (SetupGameStateForUI ss s curRow curCol curDir nss isP1 sent) = SetupGameStateForUI ss s curRow ((curCol + 1) `mod` numCols) curDir nss isP1 sent


moveHighlight _ gs = gs

handleEnter :: GameStateForUI -> IO GameStateForUI
handleEnter oldgs@(GameStateForUI lgs curRow curCol) = do
  let s = server lgs
  let newgsui = execPlayerTurn oldgs
  if newgsui == oldgs then pure newgsui
  else do
    case newgsui of
      EndGameStateForUI {} -> sendGameStateUpdate s (Cell curRow curCol) GameOver >> pure newgsui
      GameStateForUI {} -> sendGameStateUpdate s (Cell curRow curCol) (turn $ _localGameState newgsui) >> pure newgsui
      _ -> pure newgsui
handleEnter gs = pure gs

handleRemoteStatusUpdate :: GameStateForUI -> IO GameStateForUI
handleRemoteStatusUpdate gsui@(GameStateForUI lgs _ _) = do
  let s = server lgs
  response <- getGameStateUpdate s
  case response of
    Just (myAttackedCell, turnUpdateFromOpponent) -> do
        pure $ execOpponentTurn myAttackedCell turnUpdateFromOpponent gsui
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

  _ <- customMain initialVty builder (Just chan) app (SetupGameStateForUI [] s 0 0 UIConst.Right 2 isP1 False)
  putStrLn ""
