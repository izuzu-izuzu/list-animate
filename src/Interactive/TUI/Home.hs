{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Interactive.TUI.Home where

import Control.Lens ((.~), (^.))
import Data.List (intersperse)

import Brick
import Brick.Forms

import Interactive.TUI.Core

makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . intersperse (vLimit 1 $ fill '·'))
    . newForm
        [ radioField
            emptyInputField
            [ ((), SelectFnAppendField, "(++) :: [a] -> [a] -> [a]")
            , ((), SelectFnHeadField, "head :: [a] -> a")
            , ((), SelectFnTailField, "tail :: [a] -> [a]")
            ]
        ]

homeEvent :: State e -> EventM Name (Next (State e))
homeEvent state =
    continue
    . (output .~ "<prompt>")
    . (form .~ (makeForm . formState . (^. form) $ state))
    . (mode .~ Home)
    $ state
