{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
    NOTE: This app is supposed to have multiple pages, each with its own form.
    It seems non-trivial to separate the pages into multiple different forms,
    because then each form would be of a different type.

    The quicker solution for now is to have a single record type holding all
    fields for all forms. Not ideal.
-}
module Interactive.TUI.Main (main) where

import Control.Lens ((.~), (^.))
import Data.Maybe (fromMaybe)
import Graphics.Vty
    ( Output (setMode)
    , Vty (outputIface)
    , black
    , blue
    , brightBlack
    , defAttr
    , green
    , mkVty
    , red
    , standardIOConfig
    , white
    , yellow
    )
import qualified Graphics.Vty as Vty
import Text.Pretty.Simple (pPrint)

import Brick
import Brick.Focus (focusGetCurrent, focusRingCursor)
import Brick.Forms
    ( Form (formFocus, formState)
    , focusedFormInputAttr
    , handleFormEvent
    , invalidFormInputAttr
    , newForm
    , renderForm
    )
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Brick.Widgets.List (listAttr, listSelectedAttr)

import qualified Interactive.TUI.Append as Append
import Interactive.TUI.Core
import qualified Interactive.TUI.Head as Head
import Interactive.TUI.Home
import qualified Interactive.TUI.Home as Home

main :: IO ()
main = do
    let
        buildVty = do
            v <- mkVty =<< standardIOConfig
            setMode (outputIface v) Vty.Mouse True
            pure v

    initialVty <- buildVty
    finalState <- customMain initialVty buildVty Nothing app initialState

    putStrLn "Starting input:"
    pPrint . formState . (^. form) $ initialState

    putStrLn "Final input:"
    pPrint . formState . (^. form) $ finalState

    putStrLn "App ends here."

drawUI :: State e -> [Widget Name]
drawUI s = [renderForm f <=> str o]
    where
        f = s ^. form
        o = s ^. output

makeModeForm :: Mode -> Input -> Form Input e Name
makeModeForm Home = Home.makeForm
makeModeForm FnAppend = Append.makeForm
makeModeForm FnHead = Head.makeForm
makeModeForm _ = newForm []

modePreviewEvent :: Mode -> State e -> EventM Name (Next (State e))
modePreviewEvent Home = continue . (output .~ "<preview>")
modePreviewEvent FnAppend = Append.previewEvent
modePreviewEvent FnHead = Head.previewEvent
modePreviewEvent _ = continue

modeAnimateEvent :: Mode -> State e -> EventM Name (Next (State e))
modeAnimateEvent FnAppend = Append.animateEvent
modeAnimateEvent FnHead = Head.animateEvent
modeAnimateEvent _ = continue . (output .~ "<animate>")

themeMap :: AttrMap
themeMap = attrMap defAttr
    [ (editAttr, white `on` brightBlack)
    , (editFocusedAttr, black `on` yellow)
    , (invalidFormInputAttr, white `on` red)
    , (focusedFormInputAttr, black `on` yellow)
    , (listAttr, bg blue)
    , (listSelectedAttr, white `on` green)
    ]

appEvent :: State e -> BrickEvent Name e -> EventM Name (Next (State e))
appEvent state ResizeEvent = continue state
appEvent state (KEscEvent []) = halt state
appEvent state (KEnterEvent []) = do
    let
        focus =
            fromMaybe NavCurrentField
            . focusGetCurrent
            . formFocus
            . (^. form)
            $ state
    case focus of
        NavCurrentField -> continue state
        NavHomeField -> homeEvent state
        NavQuitField -> halt state
        NavPreviewField -> modePreviewEvent (state ^. mode) state
        NavAnimateField -> modeAnimateEvent (state ^. mode) state
        SelectFnAppendField ->
            continue
            . (output .~ "<prompt>")
            . (form .~ (makeModeForm FnAppend . formState . (^. form) $ state))
            . (mode .~ FnAppend)
            $ state
        SelectFnHeadField ->
            continue
            . (output .~ "<prompt>")
            . (form .~ (makeModeForm FnHead . formState . (^. form) $ state))
            . (mode .~ FnHead)
            $ state
        _ -> continue
            =<< handleEventLensed state form handleFormEvent (KEnterEvent [])
appEvent state event =
    continue =<< handleEventLensed state form handleFormEvent event

app :: App (State e) e Name
app = App
    { appDraw = drawUI
    , appHandleEvent = appEvent
    , appChooseCursor = focusRingCursor formFocus . (^. form)
    , appStartEvent = pure
    , appAttrMap = const themeMap
    }

initialState :: State e
initialState = State
   { _mode = Home
   , _form = makeModeForm Home initialInput
   , _output = "<prompt>"
   }
   where initialInput = Input "" "" "" "" "" ()
