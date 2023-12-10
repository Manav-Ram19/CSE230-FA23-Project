{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import Lens.Micro ((^.))
import Lens.Micro.TH
import Types
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import Brick
import Brick.Focus
import Brick.Forms
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import qualified Types

-------------------- TYPES --------------------
data GameStateForUI = GameStateForUI
  {
    localGameState :: LocalGameState, 
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
        modifyListAtInd ind newVal oldList = take ind oldList ++ [newVal] ++ drop (ind+1) oldList
        getElemAtInd :: Int -> a -> [a] -> a
        getElemAtInd _ defaultVal [] = defaultVal
        getElemAtInd 0 _ l = head l
        getElemAtInd n defaultVal (_:ls) = getElemAtInd (n-1) defaultVal ls

-- uses direction to upate game state to move current highlighted cell
moveHighlight ::  KeyDirection -> GameStateForUI -> GameStateForUI
moveHighlight UI.Up (GameStateForUI lgs curRow curCol)    = GameStateForUI lgs ((curRow + numRows - 1) `mod` numRows) curCol
moveHighlight UI.Down  (GameStateForUI lgs curRow curCol) = GameStateForUI lgs ((curRow + 1) `mod` numRows) curCol 
moveHighlight UI.Left (GameStateForUI lgs curRow curCol)  = GameStateForUI lgs curRow ((curCol + numCols - 1) `mod` numCols) 
moveHighlight UI.Right (GameStateForUI lgs curRow curCol)  = GameStateForUI lgs curRow ((curCol + 1) `mod` numCols)


drawGrid :: [[Char]] -> String -> Int -> Int -> Widget n
drawGrid grid label hRowId hColId = B.borderWithLabel (str label) $
    vBox $ map (hBox . map drawCell) gridWithHighLightBool
    where
      gridWithHighLightBool = map (\row -> map tmp row) gridWithRowId
      tmp (rowId, (colId, c)) = if rowId == hRowId && colId == hColId then (c, True) else (c, False)
      gridWithRowId = map (\row -> zip ([0..numRows]) (row)) gridWithColId
      gridWithColId = map (\row -> zip ([0..numRows]) (row)) grid

draw :: GameStateForUI  -> [Widget a]
draw (GameStateForUI lgs curRow curCol) = [C.vCenter $ C.hCenter grid]
  where
    grid = hBox[drawGrid mb "My Board" curRow curCol, drawGrid ob "Opponents Board" (-1) (-1)] 
    mb = makeBoard (myBoard lgs) False
    ob = makeBoard (oppBoard lgs) True

-------------------- EVENTS -------------------

handleEvent :: BrickEvent n e -> EventM n GameStateForUI()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') []))   = halt
handleEvent (VtyEvent (V.EvKey V.KUp         []))   = modify $ moveHighlight UI.Up 
handleEvent (VtyEvent (V.EvKey V.KDown       []))   = modify $ moveHighlight UI.Down 
handleEvent (VtyEvent (V.EvKey V.KLeft       []))   = modify $ moveHighlight UI.Left
handleEvent (VtyEvent (V.EvKey V.KRight       []))   = modify $ moveHighlight UI.Right
handleEvent _ = pure () 


-------------------- APP --------------------

app :: App GameStateForUI e Name 
app = App { appDraw = draw
                  , appChooseCursor = neverShowCursor
                  , appHandleEvent = handleEvent
                  , appStartEvent = pure() 
                  , appAttrMap = const theMap
                  }

-- handleEvent :: BrickEvent ResourceNames e -> EventM ResourceNames GameStateForUI ()
-- handleEvent n  = halt

getInitalState :: GameStateForUI
getInitalState = 
  GameStateForUI localGameState 1 1 
  where 
    localGameState = LocalGameState (Board []  []) (Board [] []) False Player1 


_main :: IO ()
_main = do
  let builder = V.mkVty V.defaultConfig
      

  initialVty <- builder
  f' <- customMain initialVty builder Nothing app getInitalState
  putStrLn "esdjklfsdkl"

  -- putStrLn "The starting form state was:"
  -- print initialUserInfo

  -- putStrLn "The final form state was:"
  -- print $ formState f'