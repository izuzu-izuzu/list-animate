{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Interactive.TUI.Main where

import Control.Lens (makeLenses, (^.))
import Graphics.Vty (Event (EvKey), Key (KEsc, KEnter), defAttr, blue, black, white, yellow)
import Brick (Widget, str, padAll, BrickEvent (VtyEvent), EventM, Next, halt, continue, handleEventLensed, AttrMap, attrMap, fg, on, bg, App (App, appDraw, appChooseCursor, appHandleEvent, appStartEvent, appAttrMap), showFirstCursor, defaultMain)
import Brick.Widgets.Dialog (Dialog, renderDialog, dialogSelection, handleDialogEvent, dialog, dialogAttr, buttonAttr, buttonSelectedAttr)
import Brick.Widgets.Center (hCenter)

data NavChoice = NavPrev | NavNext | NavHome | NavQuit
    deriving Show

data AppMode
    = Home
    | FnAppend
    | FnHead
    | FnTail
    | FnInit
    | FnLast
    deriving (Bounded, Enum, Eq, Show)

data AppState = AppState
    { _navDialog :: Dialog NavChoice
    , _mode :: AppMode
    }

makeLenses ''AppState

main :: IO ()
main = do
    s <- defaultMain app initialState
    putStrLn $ "App ended on page " <> show (s ^. mode) <> "."

navigateModes :: NavChoice -> AppMode -> AppMode
navigateModes NavPrev Home = FnLast
navigateModes NavPrev m = pred m
navigateModes NavNext FnLast = Home
navigateModes NavNext m = succ m
navigateModes NavHome _ = Home
navigateModes NavQuit m = m

switchMode :: NavChoice -> AppState -> AppState
switchMode nav state@AppState{_mode = m} = state {_mode = navigateModes nav m}

drawUI :: AppState -> [Widget ()]
drawUI state = [ui]
    where
        ui =
            renderDialog d
            . hCenter
            . padAll 1
            . str
            $ "You are at " <> show m <> "."
        d = state ^. navDialog
        m = state ^. mode

handleNavChoice :: AppState -> EventM () (Next AppState)
handleNavChoice state@AppState{_navDialog = d} = case dialogSelection d of
    Nothing -> continue state
    Just NavQuit -> halt state
    Just nav -> continue $ switchMode nav state

appEvent :: AppState -> BrickEvent () e -> EventM () (Next AppState)
appEvent state (VtyEvent (EvKey KEsc [])) = halt state
appEvent state (VtyEvent (EvKey KEnter [])) = handleNavChoice state
appEvent state (VtyEvent event) =
    continue =<< handleEventLensed state navDialog handleDialogEvent event
appEvent state _ = continue state

initialState :: AppState
initialState = AppState {_navDialog = d, _mode = Home}
    where
        d = dialog (Just "Title") (Just (0, navChoices)) 50
        navChoices = zip
            ["Previous", "Next", "Home", "Quit"]
            [NavPrev, NavNext, NavHome, NavQuit]

themeMap :: AttrMap
themeMap = attrMap defAttr
    [ (dialogAttr, bg blue)
    , (buttonAttr, black `on` white)
    , (buttonSelectedAttr, bg yellow)
    ]

app :: App AppState e ()
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = appEvent
    , appStartEvent = pure
    , appAttrMap = const themeMap
    }
