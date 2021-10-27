{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
    NOTE: This app is supposed to have multiple pages, each with its own form.
    It seems non-trivial to separate the pages into multiple different forms,
    because then each form would be of a different type.

    The quicker solution for now is to have a single record type holding all
    fields for all forms. Not ideal.
-}
module Interactive.TUI.Main (main) where

import Control.Lens (makeLenses, (^.), (.~), (%~))
import Data.Text (Text)

import Brick
import Brick.Forms (Form (formState, formFocus), editTextField, focusedFormInputAttr, invalidFormInputAttr, newForm, radioField, renderForm, handleFormEvent, listField, setFormFocus, setFieldConcat, radioCustomField, FormFieldState)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Graphics.Vty (Event (EvKey, EvResize), Key (KEnter, KEsc, KChar), black, defAttr, red, white, yellow, mkVty, standardIOConfig, Output (setMode), Vty (outputIface), blue, green, brightBlack)
import Data.Vector (fromList)
import Brick.Focus (focusRingCursor, focusGetCurrent)
import qualified Graphics.Vty as Vty
import Text.Pretty.Simple (pPrint)
import Brick.Widgets.List (listAttr, listSelectedAttr)
import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (border)
import Interactive.TUI.Core
import Interactive.TUI.Interpreter
import Interactive.TUI.Append
import qualified Interactive.TUI.Append as Append

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
        m = s ^. mode
        f = s ^. form
        o= s ^. output

makeModeForm :: Mode -> Input -> Form Input e Name
makeModeForm Home = newForm
    [ radioField
        emptyInputField
        [ ((), SelectFnAppendField, "(++) :: [a] -> [a] -> [a]")
        ]
    ]
makeModeForm FnAppend = Append.makeForm
makeModeForm _ = newForm []

modePreviewEvent :: Mode -> State e -> EventM Name (Next (State e))
modePreviewEvent Home = continue . (output .~ "<preview>")
modePreviewEvent FnAppend = Append.previewEvent
modePreviewEvent _ = continue

modeAnimateEvent :: Mode -> State e -> EventM Name (Next (State e))
modeAnimateEvent FnAppend = Append.animateEvent
modeAnimateEvent _ = continue . (output .~ "<animate>")

{-
syncToMode :: State e -> State e
syncToMode s@State{_mode = m, _form = f} = s
    { _form = makeForm m $ formState f
    , _output = makePrompt m $ formState f
    }
-}

{-
navigateMode :: NavChoice -> State e -> State e
navigateMode nav s@State{_mode = m, _form = f} = s
    { _mode = m'
    , _form = makeForm m' $ formState f
    , _prompt = makePrompt m' $ formState f
    }
    where m' = navByFrom nav m
-}
{-
updateMode :: State e -> State e
updateMode s@State{_mode = m, _form = f, _output = o} = s
    { _mode = m'
    , _form = f'
    , _output = o'
    }
    where
        nav = (^. navChoice) $ formState f
        m' = navByFrom nav m
        f' = case nav of
            NavCurrent -> f
            _ -> f
        o' = case nav of
            NavCurrent -> o
            _ -> o
-}

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
        NavHomeField ->
            continue
            . (output .~ "<prompt>")
            . (form .~ (makeModeForm Home . formState . (^. form) $ state))
            . (mode .~ Home)
            $ state
        NavQuitField -> halt state
        NavPreviewField -> modePreviewEvent (state ^. mode) state
        NavAnimateField -> modeAnimateEvent (state ^. mode) state
        SelectFnAppendField ->
            continue
            . (output .~ "<prompt>")
            . (form .~ (makeModeForm FnAppend . formState . (^. form) $ state))
            . (mode .~ FnAppend)
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
