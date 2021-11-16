{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
    This app contains multiple pages:

    * A home page for the user to select the function they want animated
    * A different page for each function for the user to enter arguments

    The home page contains a simple selection menu.

    Each function-specific app page contains the following:

    * The type signature and function expression in the page title
    * As many text input fields as there are arguments
    * Instructions for the user, including what constitutes a valid argument
    * A preview box that shows either the value of each argument or an error
      message if the argument is invalid
-}
module Interactive.TUI.Main where

import Control.Lens ((.~), (^.))
import Data.Maybe (fromMaybe)
import Graphics.Vty
    ( Output (setMode)
    , Vty (outputIface)
    , black
    , bold
    , brightBlack
    , brightMagenta
    , cyan
    , defAttr
    , mkVty
    , red
    , standardIOConfig
    , white
    , withStyle
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
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)

import qualified Interactive.TUI.Append as Append
import Interactive.TUI.Core
import qualified Interactive.TUI.Head as Head
import Interactive.TUI.Home
import qualified Interactive.TUI.Home as Home
import qualified Interactive.TUI.Tail as Tail

{-|
    Open the app.
-}
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

{-|
    Render the UI from the given app state.
-}
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

{-|
    Make the app page title, given the current mode.
-}
makeModeTitle :: Mode -> String
makeModeTitle Home = Home.makeTitle
makeModeTitle FnAppend = Append.makeTitle
makeModeTitle FnHead = Head.makeTitle
makeModeTitle FnTail = Tail.makeTitle
makeModeTitle _ = ""

{-|
    Render the input form, given the current mode and recorded user input.
-}
makeModeForm :: Mode -> Input -> Form Input e Name
makeModeForm Home = Home.makeForm
makeModeForm FnAppend = Append.makeForm
makeModeForm FnHead = Head.makeForm
makeModeForm FnTail = Tail.makeForm
makeModeForm _ = newForm []

{-|
    Render mode-specific instructions for the user.
-}
makeModeNote :: Mode -> Widget Name
makeModeNote Home = Home.makeNote
makeModeNote FnAppend = Append.makeNote
makeModeNote FnHead = Head.makeNote
makeModeNote FnTail = Tail.makeNote
makeModeNote _ = emptyWidget

{-|
    Render the default user prompt.
-}
makeDefaultOutput :: Widget Name
makeDefaultOutput = strWrap
    "Select [Preview] to evaluate and view all arguments."

{-|
    Handle the event when the user requests to preview the arguments (e.g. by
    selecting the [Preview] button).
-}
modePreviewEvent :: Mode -> State e -> EventM Name (Next (State e))
modePreviewEvent FnAppend = Append.previewEvent
modePreviewEvent FnHead = Head.previewEvent
modePreviewEvent FnTail = Tail.previewEvent
modePreviewEvent _ = continue . (output .~ str "<preview>")

{-|
    Handle the event when the user requests to view the animation (e.g. by
    selecting the [Animate] button).
-}
modeAnimateEvent :: Mode -> State e -> EventM Name (Next (State e))
modeAnimateEvent FnAppend = Append.animateEvent
modeAnimateEvent FnHead = Head.animateEvent
modeAnimateEvent FnTail = Tail.animateEvent
modeAnimateEvent _ = continue . (output .~ str "<animate>")

{-|
    Handle the event when the user switches to a different app mode (usually
    the home page).
-}
selectModeEvent :: Mode -> State e -> EventM Name (Next (State e))
selectModeEvent m =
    continue
    . (output .~ makeDefaultOutput)
    . (note .~ makeModeNote m)
    . (form .~ makeModeForm m initialInput)
    . (mode .~ m)

{-|
    UI theme attributes.
-}
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

{-|
    The event handler for the entire app.
-}
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

{-|
    The app.
-}
app :: App (State e) e Name
app = App
    { appDraw = drawUI
    , appHandleEvent = appEvent
    , appChooseCursor = focusRingCursor formFocus . (^. form)
    , appStartEvent = pure
    , appAttrMap = const themeMap
    }

{-|
    The initial app state.
-}
initialState :: State e
initialState = State
   { _mode = Home
   , _form = makeModeForm Home initialInput
   , _note = makeModeNote Home
   , _output = emptyWidget
   }

{-|
    The initial user input state (all blank).
-}
initialInput :: Input
initialInput = Input "" "" "" "" "" ()
