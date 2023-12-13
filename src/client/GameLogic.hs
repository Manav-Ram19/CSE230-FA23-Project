module GameLogic (
  addShip, execPlayerTurn, getPositionsFromStartDirAndLen, execOpponentTurn, isShipPlacementOutOfBounds, isShipCollidingWithExistingShip, turnDirectionClockWise, turnDirectionAntiClockWise, moveCell, moveSelectedCell
) where
import Types (numRows, numCols, numShipsPerPlayer, Cell (..), Board (..), ClientGameState (..), GameTurn (..), Ship, Direction(..))
import Common (contains, containsAll)
import qualified Data.Maybe

---------- GAME STATE MANIPULATION LOGIC ----------

addShip :: ClientGameState -> ClientGameState
addShip sgsui@(SetupGameState ss r c dir curShipSize isP1)
  | length ss >= numShipsPerPlayer = sgsui
  | isShipPlacementOutOfBounds (r, c) curShipSize dir = sgsui
  | foldr (\cell acc -> acc || contains cell (concat ss)) False newShipPlacement = sgsui
  | otherwise = SetupGameState newShipList r c dir nextShipSize isP1
  where
      newShipList = ss ++ [newShipPlacement]
      newShipPlacement = map (uncurry Cell) (getPositionsFromStartDirAndLen (r,c) curShipSize dir)
      nextShipSize = Data.Maybe.fromMaybe 0 (numShipsToNextShipSize $ length newShipList)
addShip gs = gs

execPlayerTurn :: ClientGameState -> ClientGameState
execPlayerTurn gs@(GamePlayState myb oldopponentBoard isP1 t r c) =
  if isCellAttackedBefore (Cell r c) oldopponentBoard then gs
  else case findNextGameTurn isHit isGameOver t of
        GameOver -> EndGameState (isWinner newgs)
        _ -> newgs
    where
      attackCell = Cell r c
      isHit = checkForCollision attackCell (ships oldopponentBoard)
      newAttackedCells = attackCell : attackedCells oldopponentBoard
      isGameOver = checkIfPlayerWon newAttackedCells (ships oldopponentBoard)
      newOpBoard = Board (ships oldopponentBoard) newAttackedCells
      nextTurn = findNextGameTurn isHit isGameOver t
      newgs = GamePlayState myb newOpBoard isP1 nextTurn r c
execPlayerTurn gs = gs

execOpponentTurn :: Cell -> GameTurn -> ClientGameState -> ClientGameState
execOpponentTurn attackedCell turnUpdateFromOpponent (GamePlayState myb opb isP1 _ curAttRow curAttCol) =
  case turnUpdateFromOpponent of
    GameOver -> EndGameState (isWinner newgs)
    _ -> newgs
  where
    newgs = GamePlayState myNewBoard opb isP1 turnUpdateFromOpponent curAttRow curAttCol
    myNewBoard = Board (ships myb) (attackedCell : attackedCells myb)
execOpponentTurn _ _ gs = gs

---------- HELPERS ----------

moveSelectedCell :: Direction -> ClientGameState -> ClientGameState
moveSelectedCell dir (SetupGameState ss r c d nss isp1) = SetupGameState ss newr newc d nss isp1
  where
    newr = row newCell
    newc = col newCell
    newCell =  moveCell dir (Cell r c)
moveSelectedCell dir (GamePlayState mb opb isp1 t r c) = GamePlayState mb opb isp1 t newr newc
  where
    newr = row newCell
    newc = col newCell
    newCell =  moveCell dir (Cell r c)
moveSelectedCell _ gs = gs

moveCell :: Direction -> Cell -> Cell
moveCell dir (Cell curRow curCol) = uncurry Cell (changeRC dir)
  where
    changeRC Types.Left = (curRow, (curCol + numCols - 1) `mod` numCols)
    changeRC Types.Right = (curRow, (curCol + 1) `mod` numCols)
    changeRC Types.Up = ((curRow + numRows - 1) `mod` numRows, curCol)
    changeRC Types.Down= ((curRow + 1) `mod` numRows, curCol)

isShipCollidingWithExistingShip :: Ship -> [Ship] -> Bool
isShipCollidingWithExistingShip _ [] = False
isShipCollidingWithExistingShip [] _ = False
isShipCollidingWithExistingShip s ss = foldr (\c acc -> acc || contains c sscs) False s
  where
    sscs = concat ss

isShipPlacementOutOfBounds:: (Int, Int) -> Int -> Direction -> Bool
isShipPlacementOutOfBounds (startR, _) shipLen Types.Up = startR - shipLen + 1 < 0
isShipPlacementOutOfBounds (startR, _) shipLen Types.Down = startR + shipLen - 1 >= numRows
isShipPlacementOutOfBounds (_, startC) shipLen Types.Left = startC - shipLen + 1 < 0
isShipPlacementOutOfBounds (_, startC) shipLen Types.Right = startC + shipLen - 1 >= numCols

getPositionsFromStartDirAndLen :: (Int, Int) -> Int -> Direction -> [(Int, Int)]
getPositionsFromStartDirAndLen _ 0 _ = []
getPositionsFromStartDirAndLen (r, c) l dir = (r, c) : getPositionsFromStartDirAndLen (nr, nc) (l - 1) dir
  where
    (nr, nc) = (updateRow r dir, updateCol c dir)
    updateRow oldRow Types.Up = (oldRow + numRows - 1) `mod` numRows
    updateRow oldRow Types.Down = (oldRow + 1) `mod` numRows
    updateRow oldRow _ = oldRow
    updateCol oldCol Types.Left = (oldCol + numCols - 1) `mod` numCols
    updateCol oldCol Types.Right = (oldCol + 1) `mod` numCols
    updateCol oldCol _ = oldCol

-- Dry violation
numShipsToNextShipSize :: Int -> Maybe Int
numShipsToNextShipSize n
  | n == 0 = Just 2
  | n == 1 = Just 3
  | n == 2 = Just 3
  | n == 3 = Just 4
  | n == 4 = Just 5
  | otherwise = Nothing

isCellAttackedBefore :: Cell -> Board -> Bool
isCellAttackedBefore c (Board _ ac) = contains c ac

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

isWinner :: ClientGameState -> Bool
isWinner (GamePlayState _ opb _ GameOver _ _) = containsAll (concat $ ships opb) (attackedCells opb)
isWinner (EndGameState isw) = isw
isWinner _ = False

turnDirectionClockWise :: Direction -> Direction
turnDirectionClockWise Types.Left = Types.Up
turnDirectionClockWise Types.Up = Types.Right
turnDirectionClockWise Types.Right = Types.Down
turnDirectionClockWise Types.Down = Types.Left

turnDirectionAntiClockWise :: Direction -> Direction
turnDirectionAntiClockWise dir = turnDirectionClockWise $ turnDirectionClockWise $ turnDirectionClockWise dir