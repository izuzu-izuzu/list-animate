{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Misc.MultiApp where

import Control.Lens (makeLenses, (^.))
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..), handleEventLensed
  )
import Brick.Widgets.Core
  ( padAll
  , str
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T
import Graphics.Vty (Modifier(MCtrl))
import Brick.Widgets.Dialog (handleDialogEvent)

data Choice = Red | Blue | Green
            deriving Show

data Mode = Page1 | Page2
    deriving (Eq, Enum, Show)

data AppState = AppState
    { _dialog :: D.Dialog Choice
    , _mode :: Mode
    }

makeLenses ''AppState

switchMode :: AppState -> AppState
switchMode (AppState d Page1) = AppState d Page2
switchMode (AppState d Page2) = AppState d Page1

drawUI :: AppState -> [Widget ()]
drawUI state
    | state ^. mode == Page1 = drawUI1 $ state ^. dialog
    | otherwise = drawUI2 $ state ^. dialog

drawUI1 :: D.Dialog Choice -> [Widget ()]
drawUI1 d = [ui]
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "This is the dialog body."

drawUI2 :: D.Dialog Choice -> [Widget ()]
drawUI2 d = [ui]
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "This is the other dialog body."

appEvent :: AppState -> BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent state (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt state
        V.EvKey V.KEnter [] -> M.halt state
        V.EvKey (V.KChar 'n') [MCtrl] -> M.continue $ switchMode state 
        _ -> M.continue =<< handleEventLensed state dialog handleDialogEvent ev
appEvent state _ = M.continue state

initialState :: AppState
initialState = AppState (D.dialog (Just "Title") (Just (0, choices)) 50) Page1
    where
        choices = [ ("Red", Red)
                  , ("Blue", Blue)
                  , ("Green", Green)
                  ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
    s <- M.defaultMain theApp initialState
    putStrLn $ "You chose: " <> show (D.dialogSelection $ s ^. dialog)
