module GameLogic (
  addShip, execPlayerTurn, getPositionsFromStartDirAndLen, execOpponentTurn
) where
import UIConst (Direction (..), GameStateForUI (..))
import Types (numRows, numCols, numShipsPerPlayer, Cell (Cell), Board (..), LocalGameState (..), GameTurn (..), Ship)
import Common (contains, containsAll)
import qualified Data.Maybe

---------- GAME STATE MANIPULATION LOGIC ----------

-- >>> map (uncurry Cell) (getPositionsFromStartDirAndLen (0,1) 3 UIConst.Right)
-- [Cell {row = 0, col = 1},Cell {row = 0, col = 2},Cell {row = 0, col = 3}]

addShip :: GameStateForUI -> GameStateForUI
addShip sgsui@(SetupGameStateForUI ss s r c dir curShipSize isP1 sent)
  | length ss >= numShipsPerPlayer = sgsui
  | isShipPlacementOutOfBounds (r, c) curShipSize dir = sgsui
  | foldr (\cell acc -> acc || contains cell (concat ss)) False newShipPlacement = sgsui
  | otherwise = SetupGameStateForUI newShipList s r c dir nextShipSize isP1 sent
  where
      newShipList = ss ++ [newShipPlacement]
      newShipPlacement = map (uncurry Cell) (getPositionsFromStartDirAndLen (r,c) curShipSize dir)
      nextShipSize = Data.Maybe.fromMaybe 0 (numShipsToNextShipSize $ length newShipList)
addShip gs = gs

execPlayerTurn :: GameStateForUI -> GameStateForUI
execPlayerTurn gsui@(GameStateForUI lgs r c) =
  if isCellChosenBefore (Cell r c) (oppBoard lgs) then gsui
  else case findNextGameTurn isHit isGameOver (turn lgs) of
        GameOver -> EndGameStateForUI (isWinner newgs)
        _ -> GameStateForUI newgs r c
    where
      attackCell = Cell r c
      oldopponentBoard = oppBoard lgs
      isHit = checkForCollision attackCell (ships oldopponentBoard)
      newAttackedCells = attackCell : attackedCells oldopponentBoard
      isGameOver = checkIfPlayerWon newAttackedCells (ships oldopponentBoard)
      newOpBoard = Board (ships oldopponentBoard) newAttackedCells
      nextTurn = findNextGameTurn isHit isGameOver (turn lgs)
      newgs = LocalGameState (myBoard lgs) newOpBoard (amIP1 lgs) nextTurn (server lgs)
execPlayerTurn gs = gs

execOpponentTurn :: Cell -> GameTurn -> GameStateForUI -> GameStateForUI
execOpponentTurn attackedCell@(Cell r c) turnUpdateFromOpponent gsui@(GameStateForUI {}) =
  case turnUpdateFromOpponent of
    GameOver -> EndGameStateForUI (isWinner newLocalGameState)
    _ -> GameStateForUI newLocalGameState r c
  where
    newLocalGameState = LocalGameState myNewBoard (oppBoard lgs) (amIP1 lgs) turnUpdateFromOpponent (server lgs)
    myNewBoard = Board (ships myb) (attackedCell : attackedCells myb)
    myb = myBoard lgs
    lgs = _localGameState gsui
execOpponentTurn _ _ gs = gs

---------- HELPERS ----------

isShipPlacementOutOfBounds:: (Int, Int) -> Int -> Direction -> Bool
isShipPlacementOutOfBounds (startR, _) shipLen UIConst.Up = startR - shipLen + 1 < 0
isShipPlacementOutOfBounds (startR, _) shipLen UIConst.Down = startR + shipLen - 1 >= numRows
isShipPlacementOutOfBounds (_, startC) shipLen UIConst.Left = startC - shipLen + 1 < 0
isShipPlacementOutOfBounds (_, startC) shipLen UIConst.Right = startC + shipLen - 1 >= numCols

getPositionsFromStartDirAndLen :: (Int, Int) -> Int -> Direction -> [(Int, Int)]
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

isCellChosenBefore :: Cell -> Board -> Bool
isCellChosenBefore c (Board _ ac) = contains c ac

checkIfPlayerWon :: [Cell] -> [Ship] -> Bool
checkIfPlayerWon attackedcells opponentships = containsAll (concat opponentships) attackedcells

checkForCollision :: Cell -> [Ship] -> Bool
checkForCollision cell s = contains cell (concat s)

findNextGameTurn :: Bool -> Bool -> GameTurn -> GameTurn
findNextGameTurn isHit isGameOver curTurn
    | isGameOver || (curTurn == GameOver) = GameOver
    | isHit = curTurn
    | curTurn == Player1 = Player2
    | otherwise = Player1

isWinner :: LocalGameState -> Bool
isWinner (LocalGameState _ opb _ GameOver _) = containsAll (concat $ ships opb) (attackedCells opb)
isWinner _ = False
