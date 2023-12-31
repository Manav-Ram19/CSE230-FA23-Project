module UIDraw (drawSetupPhase, drawEndGamePhase, drawGrid, drawTitle, drawShipsRemaining, drawTutorial, drawGameTurn, drawGamePhase) where
import Brick (Widget)
import Brick.Widgets.Core
    ( overrideAttr,
      str,
      vLimit,
      withBorderStyle,
      hBox,
      fill,
      padRight,
      Padding(..),
      padLeft,
      (<+>),
      vBox,
      padAll,
      hLimit,
      (<=>),
      padTop )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Common (contains)
import UIConst (highlight, nothing, ship, miss, hit, battleshipText, cyan, red, youLoseText, green, youWinText, invalidhighlight)
import Types (numRows, numCols)

---------- DRAWING PHASES ----------

drawSetupPhase :: [String] -> [(Int, Int)] -> Int -> Bool -> [Widget a]
drawSetupPhase b highLightedCells numShipsRemaining isValidHighlight = [C.vCenter (C.hCenter drawTitle <=> C.hCenter grid <=> C.hCenter (drawShipsRemaining numShipsRemaining) <=> C.hCenter drawTutorial)]
  where
    grid = hBox [drawGrid b "My Board" highLightedCells isValidHighlight]

drawEndGamePhase :: Bool -> [Widget a]
drawEndGamePhase isw = [C.vCenter $ C.hCenter (drawEndGame isw)]
  where
    drawEndGame :: Bool -> Widget n
    drawEndGame False =
      overrideAttr B.borderAttr red $
        B.border $
          vBox [C.hCenter $ padAll 1 (str youLoseText)]
    drawEndGame True =
      overrideAttr B.borderAttr green $
        B.border $
          vBox [C.hCenter $ padAll 1 (str youWinText)]

drawGamePhase :: [String] -> [String] -> [(Int, Int)] -> Bool -> [Widget a]
drawGamePhase mb ob highlightedCells isPTurn = [C.vCenter $ drawTitle <=> C.hCenter grid <=> C.hCenter (padTop (Pad 4) turnBox)]
  where
    turnBox = drawGameTurn isPTurn
    grid = hBox [drawGrid mb "My Board" [] True, drawGrid ob "Opponents Board" highlightedCells True]

---------- DRAWINGS GRIDS ----------
drawCell :: Bool ->  (Char, Bool) -> Widget n
drawCell isValidHighlight (c, highlighted) =
  overrideAttr B.borderAttr attr (B.border (str (" " ++ [renderChar c] ++ " ")))
  where
    -- if highlighted then withAttr (attrName "warning") (str (" " ++ "D" ++ " ")) else str (" " ++ [c] ++ " ")
    attr = if highlighted then if isValidHighlight then highlight else invalidhighlight else getCorrectAttr c
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

drawGrid :: [String] -> String -> [(Int, Int)] -> Bool -> Widget n
drawGrid grid label highLightedCells isValidHighlight =
  B.borderWithLabel (str label) $
    vBox $
      map (hBox . map (drawCell isValidHighlight)) gridWithHighLightBool
  where
    gridWithHighLightBool = map convertRow gridWithRowId
    convertRow :: (Int, [(Int, Char)]) -> [(Char, Bool)]
    convertRow (_, []) = []
    convertRow (rowId, (colId, c) : ls) = (if contains (rowId, colId) highLightedCells then (c, True) else (c, False)) : convertRow (rowId, ls)
    gridWithRowId = zip [0 .. (numRows - 1)] gridWithColId
    gridWithColId = map (zip [0 .. (numCols - 1)]) grid

---------- DRAWING INFO FOR USER -----------

drawTitle :: Widget n
drawTitle =
  overrideAttr B.borderAttr cyan $
    B.border $
      vBox [C.hCenter $ padAll 1 (str battleshipText)]

---------- Put Ships ----------

drawShipsRemaining :: Int -> Widget n
drawShipsRemaining numShipsRemaining =
  hLimit 30 $
  vLimit 3 $
  withBorderStyle BS.unicodeRounded $
    B.borderWithLabel (str "Ships Left") $
      hBox [fill ' ', str (show numShipsRemaining), fill ' ']

drawTutorial :: Widget n
drawTutorial =
  hLimit 50 $
  withBorderStyle BS.unicodeRounded $
        B.borderWithLabel (str "Tutorial") $
          vBox $
            map (uncurry drawTutorialRow)
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
      padRight Max (padLeft (Pad 1) $ str action) <+> padLeft Max (padRight (Pad 1) $ str key)


---------- Actual Game ----------

drawGameTurn :: Bool -> Widget n
drawGameTurn myTurn =
  withBorderStyle BS.unicodeRounded $
    withBorderStyle BS.unicodeBold $
      B.border $
        vBox [padAll 1 (str (render myTurn))]
  where
    render False = "Please wait for your opponent to miss..."
    render True = "Please make a move..."
