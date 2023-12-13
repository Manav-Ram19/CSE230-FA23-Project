module Presenter
  ( present,
  )
where

import GameLogic
    ( addShip,
      getPositionsFromStartDirAndLen,
      execPlayerTurn,
      execOpponentTurn, isShipPlacementOutOfBounds, isShipCollidingWithExistingShip, turnDirectionClockWise, turnDirectionAntiClockWise, moveSelectedCell )
import Common (modifyListAtInd, getElemAtInd, contains)
import UIConst (RemoteStatusUpdate (..), Name, myattrApp, GameStateForPresenter (..))
import UIDraw
    ( drawEndGamePhase, drawSetupPhase, drawGamePhase )
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Conc (threadDelay)
import ClientNetwork (getGameStateUpdate, getOpponentShips, sendGameStateUpdate, sendPlayerShips)
import qualified Graphics.Vty as V
import Types
    ( numShipsPerPlayer,
      Board(..),
      Cell(Cell),
      GameTurn(..),
      Server, ClientGameState (..), Direction (..) )
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

draw :: GameStateForPresenter -> [Widget a]
-- setup game state
draw (GameStateForPresenter (SetupGameState myShips curR curC curDir shipSize _) _) = drawSetupPhase mb hcells numShipsRemaining isValidHighlight
  where
    numShipsRemaining = numShipsPerPlayer - length myShips
    hcells = getPositionsFromStartDirAndLen (curR, curC) shipSize curDir
    mb = makePlayerBoard (Board myShips [])
    isValidHighlight = not (isShipOutOfBounds || isShipCollidingWithOldShip)
    isShipOutOfBounds = isShipPlacementOutOfBounds (curR, curC) shipSize curDir
    isShipCollidingWithOldShip = isShipCollidingWithExistingShip shipLocation myShips
    shipLocation = map (uncurry Cell) hcells

-- end game state
draw (GameStateForPresenter (EndGameState isw) _) = drawEndGamePhase isw
--  during game
draw (GameStateForPresenter (GamePlayState mb ob isP1 t r c) _) = drawGamePhase myBoard opBoard highlightedCells isPTurn
  where
    myBoard = makePlayerBoard mb
    opBoard = makeOpponentBoard ob
    isPTurn = isMyTurn isP1 t
    highlightedCells = [(r, c) | isPTurn]

-------------------- EVENTS -------------------

handleEvent :: BrickEvent Name RemoteStatusUpdate -> EventM n GameStateForPresenter ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent e = do
  currState <- get
  case _clientGameState currState of
    SetupGameState {} -> handleEventSetupGame e
    GamePlayState {} -> handleEventPlayGame e
    EndGameState _ -> pure ()

handleEventSetupGame :: BrickEvent Name RemoteStatusUpdate -> EventM n GameStateForPresenter ()
handleEventSetupGame e = do
  sgsui <- get
  eventHandler e sgsui
  where
    eventHandler :: BrickEvent Name RemoteStatusUpdate -> GameStateForPresenter -> EventM n GameStateForPresenter ()
    eventHandler(AppEvent RemoteStatusUpdate) (GameStateForPresenter gs@(SetupGameState {}) server) = do
      if length (_setupships gs) < numShipsPerPlayer then pure ()
      else do
        maybeoppShips <- liftIO $ getOpponentShips server
        case maybeoppShips of
          Just oppShips -> do
            let newgs = GamePlayState (Board (_setupships gs) []) (Board oppShips []) (_isP1 gs) Player1 0 0
            let gsui = GameStateForPresenter newgs server
            put gsui
          _ -> pure ()
    eventHandler (VtyEvent (V.EvKey V.KUp [])) (GameStateForPresenter gs@(SetupGameState {}) server) = 
      do put $ GameStateForPresenter (moveSelectedCell Types.Up gs) server
    eventHandler (VtyEvent (V.EvKey V.KDown [])) (GameStateForPresenter gs@(SetupGameState {}) server) = 
      do put $ GameStateForPresenter (moveSelectedCell Types.Down gs) server
    eventHandler (VtyEvent (V.EvKey V.KLeft [])) (GameStateForPresenter gs@(SetupGameState {}) server) = 
      do put $ GameStateForPresenter (moveSelectedCell Types.Left gs) server
    eventHandler (VtyEvent (V.EvKey V.KRight [])) (GameStateForPresenter gs@(SetupGameState {}) server) = 
      do put $ GameStateForPresenter (moveSelectedCell Types.Right gs) server
    -- Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'z') [])) (GameStateForPresenter (SetupGameState ss r c d nss isp1) server) = do
      let newdir = turnDirectionClockWise d
      put $ GameStateForPresenter (SetupGameState ss r c newdir nss isp1) server
    -- Anti-Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'x') [])) (GameStateForPresenter (SetupGameState ss r c d nss isp1) server) = do
      let newdir = turnDirectionAntiClockWise d
      put $ GameStateForPresenter (SetupGameState ss r c newdir nss isp1) server
    -- User wants to add a ship
    eventHandler (VtyEvent (V.EvKey V.KEnter [])) (GameStateForPresenter gs@(SetupGameState {}) server) = do
      if _nextShipSize gs == 0 then pure ()
      else do
        let newgs = addShip gs
        when (length (_setupships newgs) == numShipsPerPlayer) $ liftIO $ sendPlayerShips server (_setupships newgs)
        put (GameStateForPresenter newgs server)
    eventHandler _ _ = pure ()

handleEventPlayGame :: BrickEvent Name RemoteStatusUpdate -> EventM n GameStateForPresenter ()
handleEventPlayGame (AppEvent RemoteStatusUpdate) = do
  currState <- get
  case currState of
    GameStateForPresenter gs@(GamePlayState {}) _ -> do
      unless (isMyTurn (_amIP1 gs) (_turn gs)) $ do
        uState <- liftIO (handleRemoteStatusUpdate currState)
        put uState
    _ -> pure ()
handleEventPlayGame e = do
  currState <- get
  case currState of
    GameStateForPresenter gs@(GamePlayState {}) _ -> do
      when (isMyTurn (_amIP1 gs) (_turn gs)) $ do
        uState <- liftIO (eventHandler e currState)
        put uState
    _ -> pure ()
  where
    eventHandler :: BrickEvent n e -> GameStateForPresenter -> IO GameStateForPresenter
    eventHandler (VtyEvent (V.EvKey V.KUp [])) (GameStateForPresenter gs@(GamePlayState {}) server) = 
      pure $ GameStateForPresenter (moveSelectedCell Types.Up gs) server
    eventHandler (VtyEvent (V.EvKey V.KDown [])) (GameStateForPresenter gs@(GamePlayState {}) server) = 
      pure $ GameStateForPresenter (moveSelectedCell Types.Down gs) server
    eventHandler (VtyEvent (V.EvKey V.KLeft [])) (GameStateForPresenter gs@(GamePlayState {}) server) = 
      pure $ GameStateForPresenter (moveSelectedCell Types.Left gs) server
    eventHandler (VtyEvent (V.EvKey V.KRight [])) (GameStateForPresenter gs@(GamePlayState {}) server) = 
      pure $ GameStateForPresenter (moveSelectedCell Types.Right gs) server
    eventHandler (VtyEvent (V.EvKey V.KEnter [])) gsp@(GameStateForPresenter (GamePlayState {}) _) = handleEnter gsp
    eventHandler _ gsui = pure gsui

isMyTurn :: Bool -> GameTurn -> Bool
isMyTurn True Player1 = True
isMyTurn False Player2 = True
isMyTurn _ _ = False

handleEnter :: GameStateForPresenter -> IO GameStateForPresenter
handleEnter (GameStateForPresenter oldgs@(GamePlayState {}) s) = do
  let newgs = execPlayerTurn oldgs
  if newgs == oldgs then pure $ GameStateForPresenter oldgs s
  else do
    case newgs of
      EndGameState {} -> do
        sendGameStateUpdate s (Cell (_currAttackRow oldgs) (_currAttackCol oldgs)) GameOver
        pure $ GameStateForPresenter newgs s
      GamePlayState {} -> do
        sendGameStateUpdate s (Cell (_currAttackRow oldgs) (_currAttackCol oldgs)) (_turn newgs)
        pure $ GameStateForPresenter newgs s
      _ -> pure (GameStateForPresenter newgs s)
handleEnter gs = pure gs

handleRemoteStatusUpdate :: GameStateForPresenter -> IO GameStateForPresenter
handleRemoteStatusUpdate gsui@(GameStateForPresenter oldgs@(GamePlayState {}) s) = do
  response <- getGameStateUpdate s
  case response of
    Just (myAttackedCell, turnUpdateFromOpponent) -> do
        let newgs = execOpponentTurn myAttackedCell turnUpdateFromOpponent oldgs
        pure $ GameStateForPresenter newgs s
    Nothing -> pure gsui
handleRemoteStatusUpdate gs = pure gs

-------------------- APP --------------------

app :: App GameStateForPresenter RemoteStatusUpdate Name
app =
  App
    { appDraw = draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const myattrApp
    }

present :: Server -> Bool -> IO ()
present s isP1 = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan RemoteStatusUpdate
    threadDelay 100000

  _ <- customMain initialVty builder (Just chan) app (GameStateForPresenter (SetupGameState [] 0 0 Types.Right 2 isP1) s)
  putStrLn ""
