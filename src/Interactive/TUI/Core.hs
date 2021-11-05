{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Interactive.TUI.Core where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Graphics.Vty
    ( Event (EvKey, EvResize)
    , Key (KChar, KEnter, KEsc)
    , Modifier
    )

import Brick
import Brick.Forms (Form, FormFieldState, radioCustomField, setFieldConcat)
import Brick.Widgets.Center (vCenter, hCenter)

data Input = Input
    { _arg1 :: Text
    , _arg2 :: Text
    , _arg3 :: Text
    , _arg4 :: Text
    , _arg5 :: Text
    , _emptyInputField :: ()
    }
    deriving Show

data Name
    = Arg1Field
    | Arg2Field
    | Arg3Field
    | Arg4Field
    | Arg5Field
    | NavCurrentField
    | NavHomeField
    | NavQuitField
    | NavPreviewField
    | NavAnimateField
    | SelectFnAppendField
    | SelectFnHeadField
    | SelectFnTailField
    deriving (Bounded, Enum, Eq, Ord, Show)

data NavChoice = NavCurrent | NavHome | NavQuit | NavPreview | NavAnimate
    deriving (Bounded, Enum, Eq, Show)

data Mode
    = Home
    | FnAppend
    | FnHead
    | FnTail
    | FnInit
    | FnLast
    deriving (Bounded, Enum, Eq, Show)

data State e = State
    { _mode :: Mode
    , _form :: Form Input e Name
    , _note :: Widget Name
    , _output :: Widget Name
    }

pattern ResizeEvent :: BrickEvent n e
pattern ResizeEvent <- VtyEvent EvResize{}

pattern KEscEvent :: [Modifier] -> BrickEvent n e
pattern KEscEvent mods <- VtyEvent (EvKey KEsc mods)

pattern KEnterEvent :: [Modifier] -> BrickEvent n e
pattern KEnterEvent mods = VtyEvent (EvKey KEnter mods)

pattern KCharEvent :: Char -> [Modifier] -> BrickEvent n e
pattern KCharEvent char mods = VtyEvent (EvKey (KChar char) mods)

makeLenses ''Input
makeLenses ''State

navFieldNames :: [Name]
navFieldNames =
    [ NavCurrentField 
    , NavHomeField
    , NavQuitField
    , NavPreviewField
    , NavAnimateField
    ]

makeNavField :: Input -> FormFieldState Input e Name
makeNavField =
    setFieldConcat
        (\[x1, x2, x3, x4] -> x1 <+> x2 <+> vLimit 1 (fill ' ') <+> x3 <+> x4)
    . radioCustomField
        ' '
        ' '
        ' '
        emptyInputField
        [ ((), NavPreviewField, "[Preview]" <> rPad)
        , ((), NavAnimateField, "[Animate]" <> rPad)
        , ((), NavHomeField, "[Home]" <> rPad)
        , ((), NavQuitField, "[Quit]" <> rPad)
        ]
    where rPad = "    "

navByFrom :: NavChoice -> Mode -> Mode
navByFrom NavHome _ = Home
navByFrom _ m = m
