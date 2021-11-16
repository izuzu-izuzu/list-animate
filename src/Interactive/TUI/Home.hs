{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Interactive.TUI.Home where

import Control.Lens ((.~), (^.))

import Brick
import Brick.Forms (Form (formState), newForm, radioCustomField)

import Interactive.TUI.Core

{-|
    App page title.
-}
makeTitle :: String
makeTitle = "Home"

{-|
    Render the function selection menu.
-}
makeForm :: Input -> Form Input e Name
makeForm = newForm
    [ radioCustomField
        ' '
        ' '
        '•'
        emptyInputField
        [ ((), SelectFnAppendField, "(++) :: [a] -> [a] -> [a]" <> rPad)
        , ((), SelectFnHeadField, "head :: [a] -> a" <> rPad)
        , ((), SelectFnTailField, "tail :: [a] -> [a]" <> rPad)
        ]
    ]
    where rPad = "  "

{-|
    Render instructions for the user.
-}
makeNote :: Widget n
makeNote = strWrap
    "Use the arrow keys, Tab and Shift-Tab, or mouse to navigate the menu.\n\
    \Press Enter to select. Press Esc to quit the app.\n\
    \\n\
    \Select a function to continue."

{-|
    When the user selects [Home] from any other page, switch the app mode and
    display the home page.
-}
homeEvent :: State e -> EventM Name (Next (State e))
homeEvent state =
    continue
    . (output .~ emptyWidget)
    . (note .~ makeNote)
    . (form .~ (makeForm . formState . (^. form) $ state))
    . (mode .~ Home)
    $ state
