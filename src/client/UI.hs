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

-------------------- TYPES --------------------
data ResourceNames
  = RowField
  | ColField
  deriving (Eq, Ord, Show)

data RowColFormState = RowColFormState
  { _row :: Int,
    _col :: Int
  }
  deriving (Show)

makeLenses ''RowColFormState

data GameStateForUI = GameStateForUI
  { localGameState :: LocalGameState,
    rowColFormState :: RowColFormState
  } deriving (Show)

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow)
    ]

-------------------- DRAWS --------------------

drawCell :: Char -> Widget n
drawCell c = str [' ',c, ' ']

drawGrid :: [[Char]] -> String -> Widget n
drawGrid grid label = B.borderWithLabel (str label) $
    vBox $ map (hBox . map drawCell) grid

mkForm :: RowColFormState -> Form RowColFormState e ResourceNames
mkForm =
  let label s w =
        padBottom (Pad 1) $
          (vLimit 1 $ hLimit 10 $ str s <+> fill ' ') <+> w
   in newForm
        [ label "Row"
            @@= editShowableField row RowField,
          label "Col"
            @@= editShowableField col ColField
        ]


draw :: GameStateForUI  -> [Widget ResourceNames]
draw st = [C.vCenter $ C.hCenter grid <=> C.hCenter form <=> C.hCenter help]
  where
    grid = hBox[drawGrid (replicate 10 (replicate 10 '.')) "My Board", drawGrid (replicate 10 (replicate 10 'X')) "Opponents Board"] 
    form = B.border $ padTop (Pad 1) $ hLimit 20 $ renderForm (mkForm (rowColFormState st))
    help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
    body = str $ "Enter the Row and Col of where you want to hit\n" 
-- draw :: Form RowColFormState e ResourceNames -> [Widget ResourceNames]
-- draw f = [C.vCenter $ C.hCenter grid <=> C.hCenter form <=> C.hCenter help]
--   where
--     grid = hBox[drawGrid (replicate 10 (replicate 10 '.')) "My Board", drawGrid (replicate 10 (replicate 10 'X')) "Opponents Board"] 
--     form = B.border $ padTop (Pad 1) $ hLimit 20 $ renderForm (mkForm f)
--     help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
--     body = str $ "Enter the Row and Col of where you want to hit\n"


-------------------- EVENTS -------------------



-------------------- APP --------------------

app :: App GameStateForUI e ResourceNames
app =
  App
    { appDraw = draw,
      appHandleEvent = handleEvent,
      -- appHandleEvent = \ev -> do
      --   f <- gets formFocus
      --   case ev of
      --     VtyEvent (V.EvResize {}) -> return ()
      --     VtyEvent (V.EvKey V.KEsc []) -> halt
      --     _ -> do
      --       handleFormEvent ev

      --       -- Example of external validation:
      --       st <- gets formState
      --       modify $ setFieldValid (st ^. row >= 0) RowField,
      appChooseCursor = focusRingCursor formFocus (mkForm (rowColFormState st)),
      appStartEvent = return (),
      appAttrMap = const theMap
    }

handleEvent :: BrickEvent ResourceNames e -> EventM ResourceNames GameStateForUI ()
handleEvent n  = halt

getInitalState :: GameStateForUI
getInitalState = 
  GameStateForUI localGameState initialForm 
  where 
    initialForm = RowColFormState
          { _row = 1,
            _col = 1
          }  
    localGameState = LocalGameState (Board []  []) (Board [] []) False Player1 


_main :: IO ()
_main = do
  let buildVty = do
        v <- V.mkVty V.defaultConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
      

  initialVty <- buildVty
  f' <- customMain initialVty buildVty Nothing app getInitalState
  print $ f'

  -- putStrLn "The starting form state was:"
  -- print initialUserInfo

  -- putStrLn "The final form state was:"
  -- print $ formState f'