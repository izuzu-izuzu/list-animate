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
    , yellow, cyan, brightCyan, brightRed, brightYellow, withStyle, bold, italic, magenta, brightMagenta
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
    , renderForm, updateFormState
    )
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Brick.Widgets.List (listAttr, listSelectedAttr)

import qualified Interactive.TUI.Append as Append
import Interactive.TUI.Core
import qualified Interactive.TUI.Head as Head
import Interactive.TUI.Home
import qualified Interactive.TUI.Home as Home
import qualified Interactive.TUI.Tail as Tail
import Interactive.TUI.Tail
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Border (borderWithLabel)
import Utilities.Main ((-<))
import Brick.Widgets.Border.Style (borderStyleFromChar, unicodeRounded)

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
drawUI State{_mode = m, _form = f, _note = n, _output = o} = [ui]
    where
        ui =
            vCenter
            . foldl1 (<=>)
            . fmap hCenter
            $ [formWidget, noteWidget, outputWidget]
        formWidget = borderWithLabel
            (padLeftRight 1 . withAttr "bold" . str $ makeModeTitle m)
            (hCenter $ renderForm f)
        noteWidget = borderWithLabel
            (padLeftRight 1 . withAttr "bold" . str $ "Note")
            (hCenter n)
        outputWidget = case m of
            Home -> emptyWidget
            _ -> borderWithLabel
                (padLeftRight 1 . withAttr "bold" . str $ "Preview")
                (hCenter o)

makeModeTitle :: Mode -> String
makeModeTitle Home = Home.makeTitle
makeModeTitle FnAppend = Append.makeTitle
makeModeTitle FnHead = Head.makeTitle
makeModeTitle FnTail = Tail.makeTitle
makeModeTitle _ = ""

makeModeForm :: Mode -> Input -> Form Input e Name
makeModeForm Home = Home.makeForm
makeModeForm FnAppend = Append.makeForm
makeModeForm FnHead = Head.makeForm
makeModeForm FnTail = Tail.makeForm
makeModeForm _ = newForm []

makeModeNote :: Mode -> Widget Name
makeModeNote Home = Home.makeNote
makeModeNote FnAppend = Append.makeNote
makeModeNote FnHead = Head.makeNote
makeModeNote _ = emptyWidget

makeDefaultOutput :: Widget Name
makeDefaultOutput = strWrap
    "Select [Preview] to evaluate and view all arguments."

modePreviewEvent :: Mode -> State e -> EventM Name (Next (State e))
modePreviewEvent Home = continue . (output .~ str "<preview>")
modePreviewEvent FnAppend = Append.previewEvent
modePreviewEvent FnHead = Head.previewEvent
modePreviewEvent FnTail = Tail.previewEvent
modePreviewEvent _ = continue

modeAnimateEvent :: Mode -> State e -> EventM Name (Next (State e))
modeAnimateEvent FnAppend = Append.animateEvent
modeAnimateEvent FnHead = Head.animateEvent
modeAnimateEvent FnTail = Tail.animateEvent
modeAnimateEvent _ = continue . (output .~ str "<animate>")

selectModeEvent :: Mode -> State e -> EventM Name (Next (State e))
selectModeEvent m =
    continue
    . (output .~ makeDefaultOutput)
    . (note .~ makeModeNote m)
    . (form .~ makeModeForm m initialInput)
    . (mode .~ m)

themeMap :: AttrMap
themeMap = attrMap defAttr
    [ (editAttr, white `on` brightBlack)
    , (editFocusedAttr, black `on` yellow)
    , (invalidFormInputAttr, white `on` red)
    , (focusedFormInputAttr, black `on` cyan)
    , (attrName "actionAvailable", fg cyan)
    , (attrName "error", fg red)
    , (attrName "fgRed", fg red)
    , (attrName "fgYellow", fg yellow)
    , (attrName "fgMagenta", fg brightMagenta)
    , (attrName "bold", withStyle defAttr bold)
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
        SelectFnAppendField -> selectModeEvent FnAppend state
        SelectFnHeadField -> selectModeEvent FnHead state
        SelectFnTailField -> selectModeEvent FnTail state
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
   , _note = makeModeNote Home
   , _output = emptyWidget
   }

initialInput :: Input
initialInput = Input "" "" "" "" "" ()
