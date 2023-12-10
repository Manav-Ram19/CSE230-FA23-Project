{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import Types
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import BattleShipClientLoop (checkForCollision, checkIfPlayerWon, findNextGameTurn, isCellChosenBefore)
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Control.Monad.IO.Class (MonadIO (liftIO))
import GameClient (isPlayerOne, sendGameStateUpdate, getGameStateUpdate)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import qualified Types
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import GHC.Conc (threadDelay)

-------------------- TYPES --------------------
data GameStateForUI = GameStateForUI
  { localGameState :: LocalGameState,
    currRow :: Int,
    currCol :: Int
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

drawGrid :: [[Char]] -> String -> Int -> Int -> Widget n
drawGrid grid label hRowId hColId =
  B.borderWithLabel (str label) $
    vBox $
      map (hBox . map drawCell) gridWithHighLightBool
  where
    gridWithHighLightBool = map (\row -> convertRow row) gridWithRowId
    convertRow :: (Int, [(Int, Char)]) -> [(Char, Bool)]
    convertRow (rowId, []) = []
    convertRow (rowId, (colId, c) : ls) = (if rowId == hRowId && colId == hColId then (c, True) else (c, False)) : (convertRow (rowId, ls))
    gridWithRowId = zip [0 .. numRows] gridWithColId
    gridWithColId = map (\row -> zip ([0 .. numCols]) (row)) grid

draw :: GameStateForUI -> [Widget a]
draw (GameStateForUI lgs curRow curCol) = [C.vCenter $ C.hCenter grid]
  where
    grid = hBox [drawGrid mb "My Board" (-1) (-1), drawGrid ob "Opponents Board" curRow curCol]
    mb = makeBoard (myBoard lgs) False
    ob = makeBoard (oppBoard lgs) True

-------------------- EVENTS -------------------

handleEvent :: BrickEvent Name RemoteStatusUpdate -> EventM n GameStateForUI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (AppEvent RemoteStatusUpdate) = do
  currState <- get
  let lgs = (localGameState currState)
  if (not (isMyTurn (amIP1 lgs) (turn lgs)))
    then do 
      uState <- liftIO (handleRemoteStatusUpdate currState)
      put uState 
  else pure()

handleEvent e = do
  currState <- get
  let lgs = (localGameState currState)
  if (isMyTurn (amIP1 lgs) (turn lgs))
    then do
      uState <- liftIO (eventHandler e currState)
      put uState
    else pure ()

handleEvent _ = pure ()

eventHandler :: BrickEvent n e -> GameStateForUI -> IO (GameStateForUI)
eventHandler (VtyEvent (V.EvKey V.KUp [])) gsui    = pure (moveHighlight UI.Up gsui)
eventHandler (VtyEvent (V.EvKey V.KDown [])) gsui  = pure (moveHighlight UI.Down gsui)
eventHandler (VtyEvent (V.EvKey V.KLeft [])) gsui  = pure (moveHighlight UI.Left gsui)
eventHandler (VtyEvent (V.EvKey V.KRight [])) gsui = pure (moveHighlight UI.Right gsui)
eventHandler (VtyEvent (V.EvKey V.KEnter [])) gsui = handleEnter gsui

eventHandler _ gsui = pure(gsui)

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

handleEnter :: GameStateForUI -> IO (GameStateForUI)
handleEnter gs@(GameStateForUI lgs curRow curCol) = do
  if isCellChosenBefore (Cell curRow curCol) (oppBoard lgs)
    then pure (gs)
    else do
      let attackCell = Cell curRow curCol
      let opponentBoard = (oppBoard lgs)
      let isHit = checkForCollision attackCell (ships opponentBoard)
      let newAttackedCells = attackCell : attackedCells opponentBoard
      let isGameOver = checkIfPlayerWon newAttackedCells (ships opponentBoard)
      let newOpBoard = Board (ships opponentBoard) newAttackedCells
      let newTurn = (findNextGameTurn isHit isGameOver (turn lgs))
      let newgs = LocalGameState (myBoard lgs) newOpBoard (amIP1 lgs) newTurn (server lgs)
      let newgsui = GameStateForUI newgs curRow curCol
      sendGameStateUpdate (server newgs) attackCell newTurn
      pure (newgsui)

handleRemoteStatusUpdate :: GameStateForUI -> IO GameStateForUI
handleRemoteStatusUpdate (GameStateForUI lgs r c) = do 
  let s = (server lgs)
  let myb = (myBoard lgs)
  (myAttackedCell, turnUpdateFromOpponent) <- getGameStateUpdate s
  let myNewBoard = Board (ships myb) (myAttackedCell : attackedCells myb)
  let newLocalGameState = LocalGameState myNewBoard (oppBoard lgs) (amIP1 lgs) turnUpdateFromOpponent (server lgs)
  pure (GameStateForUI newLocalGameState r c)

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

getInitalState :: LocalGameState -> GameStateForUI
getInitalState lgs =
  GameStateForUI lgs 1 1

startUI :: LocalGameState -> IO ()
startUI lgs = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan RemoteStatusUpdate
    threadDelay 100000 
  
  f' <- customMain initialVty builder (Just chan) app (getInitalState lgs)
  putStrLn ""