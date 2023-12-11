module UI
  ( startUI,
  )
where
import UIConst (RemoteStatusUpdate (..), GameStateForUI (..), Name, KeyDirection (..), myattrApp)
import UIDraw
    ( drawGrid,
      drawTitle,
      drawShipsRemaining,
      drawTutorial,
      drawGameTurn,
      drawEndGame )
import BattleShipClientLoop (checkForCollision, checkIfPlayerWon, findNextGameTurn, isCellChosenBefore, isInList, isSubset)
import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Center as C
import ClientInfra (sendToServer)
import ClientMessages (ClientMessages (SetShips))
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Conc (threadDelay)
import GameClient (getGameStateUpdate, getOpponentShips, sendGameStateUpdate)
import qualified Graphics.Vty as V
import Types

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


draw :: GameStateForUI -> [Widget a]
-- setup gamee state
draw (UIConst.SetupGameStateForUI myShips _ curR curC curDir shipSize _ _) = [C.vCenter (C.hCenter drawTitle <=> C.hCenter grid <=> C.hCenter (drawShipsRemaining (numShipsPerPlayer - length myShips)) <=> C.hCenter drawTutorial)]
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
    turnBox = drawGameTurn (isMyTurn (amIP1 lgs) (turn lgs))
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
    eventHandler (VtyEvent (V.EvKey V.KUp [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UIConst.Right sgsui
    eventHandler (VtyEvent (V.EvKey V.KDown [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UIConst.Down sgsui
    eventHandler (VtyEvent (V.EvKey V.KLeft [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UIConst.Left sgsui
    eventHandler (VtyEvent (V.EvKey V.KRight [])) sgsui@(SetupGameStateForUI {}) = do put $ moveHighlight UIConst.Right sgsui
    -- Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'z') [])) sgsui@(SetupGameStateForUI ss s r c dir nss isP1 sent) = do
      let newDir = case dir of
                    UIConst.Left -> UIConst.Up
                    UIConst.Up -> UIConst.Right
                    UIConst.Right -> UIConst.Down
                    UIConst.Down -> UIConst.Left
      let newSGSUI = SetupGameStateForUI ss s r c newDir nss isP1 sent
      put newSGSUI
    -- Anti-Clockwise Rotation
    eventHandler (VtyEvent (V.EvKey (V.KChar 'x') [])) sgsui@(SetupGameStateForUI ss s r c dir nss isP1 sent) = do
      let newDir = case dir of
                    UIConst.Left -> UIConst.Down
                    UIConst.Up -> UIConst.Left
                    UIConst.Right -> UIConst.Up
                    UIConst.Down -> UIConst.Right
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
isShipPlacementOutOfBounds (startR, startC) shipLen UIConst.Up = startR - shipLen + 1 < 0
isShipPlacementOutOfBounds (startR, startC) shipLen UIConst.Down = startR + shipLen - 1 >= numRows
isShipPlacementOutOfBounds (startR, startC) shipLen UIConst.Left = startC - shipLen + 1 < 0
isShipPlacementOutOfBounds (startR, startC) shipLen UIConst.Right = startC + shipLen - 1 >= numCols

getPositionsFromStartDirAndLen :: (Int, Int) -> Int -> KeyDirection -> [(Int, Int)]
getPositionsFromStartDirAndLen _ 0 _ = []
getPositionsFromStartDirAndLen (r, c) l dir = (r, c) : getPositionsFromStartDirAndLen (nr, nc) (l - 1) dir
  where
    (nr, nc) = (updateRow r dir, updateCol c dir)
    updateRow row UIConst.Up = (row + numRows - 1) `mod` numRows
    updateRow row UIConst.Down = (row + 1) `mod` numRows
    updateRow row _ = row
    updateCol col UIConst.Left = (col + numCols - 1) `mod` numCols
    updateCol col UIConst.Right = (col + 1) `mod` numCols
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
moveHighlight :: KeyDirection -> GameStateForUI -> GameStateForUI
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

  _ <- customMain initialVty builder (Just chan) app (SetupGameStateForUI [] s 0 0 UIConst.Right 2 (isP1) False)
  putStrLn ""
