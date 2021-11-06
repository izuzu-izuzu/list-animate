{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Interactive.TUI.Home where

import Control.Lens ((.~), (^.))
import Data.List (intersperse)

import Brick
import Brick.Forms

import Interactive.TUI.Core

makeTitle :: String
makeTitle = "Home"

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

makeNote :: Widget n
makeNote = strWrap
    "Use the arrow keys, Tab and Shift-Tab, or mouse to navigate the menu.\n\
    \Press Enter to select. Press Esc to quit the app.\n\
    \\n\
    \Select a function to continue."

homeEvent :: State e -> EventM Name (Next (State e))
homeEvent state =
    continue
    . (output .~ emptyWidget)
    . (note .~ makeNote)
    . (form .~ (makeForm . formState . (^. form) $ state))
    . (mode .~ Home)
    $ state
