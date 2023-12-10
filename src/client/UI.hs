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

data Name
  = RowField
  | ColField
  deriving (Eq, Ord, Show)

data RowColFormState = RowColFormState
  { _row :: Int,
    _col :: Int
  }
  deriving (Show)

makeLenses ''RowColFormState

data GameState = GameState
  { localGameState :: LocalGameState,
    rowColFormState :: RowColFormState
  }

app :: App (Form RowColFormState e Name) e Name
app =
  App
    { appDraw = draw,
      appHandleEvent = \ev -> do
        f <- gets formFocus
        case ev of
          VtyEvent (V.EvResize {}) -> return ()
          VtyEvent (V.EvKey V.KEsc []) -> halt
          -- Enter quits only when we aren't in the multi-line editor.
          VtyEvent (V.EvKey V.KEnter []) -> halt
          _ -> do
            handleFormEvent ev

            -- Example of external validation:
            st <- gets formState
            modify $ setFieldValid (st ^. row >= 0) RowField,
      appChooseCursor = focusRingCursor formFocus,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

mkForm :: RowColFormState -> Form RowColFormState e Name
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

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow)
    ]

-- Function to draw a single cell
drawCell :: Char -> Widget n
drawCell c = str [' ',c, ' ']

drawGrid :: [[Char]] -> Widget n
drawGrid grid = B.borderWithLabel (str "My Board") $
    vBox $ map (hBox . map drawCell) grid

draw :: Form RowColFormState e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter grid <=> C.hCenter form <=> C.hCenter help]
  where
    grid = hBox[drawGrid (replicate 10 (replicate 10 '.')), drawGrid (replicate 10 (replicate 10 'X'))] 
    form = B.border $ padTop (Pad 1) $ hLimit 20 $ renderForm f
    help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
    body = str $ "Enter the Row and Col of where you want to hit\n"

_main :: IO ()
_main = do
  let buildVty = do
        v <- V.mkVty V.defaultConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

      initialUserInfo =
        RowColFormState
          { _row = 1,
            _col = 1
          }
      f = mkForm initialUserInfo

  initialVty <- buildVty
  f' <- customMain initialVty buildVty Nothing app f

  putStrLn "The starting form state was:"
  print initialUserInfo

  putStrLn "The final form state was:"
  print $ formState f'