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
    , _output :: String
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
navFieldNames = [NavCurrentField .. NavAnimateField]

makeNavField :: Text -> Input -> FormFieldState Input e Name
makeNavField currentLabel =
    setFieldConcat (\(x:xs) -> x <+> fill ' ' <+> hBox xs)
    . radioCustomField
        ' '
        ' '
        ' '
        emptyInputField
        [ ((), NavCurrentField, "*" <> currentLabel <> "*" <> rPad)
        , ((), NavHomeField, "[Home]" <> rPad)
        , ((), NavQuitField, "[Quit]" <> rPad)
        , ((), NavPreviewField, "[Preview]" <> rPad)
        , ((), NavAnimateField, "[Animate]" <> rPad)
        ]
    where rPad = "    "

navByFrom :: NavChoice -> Mode -> Mode
navByFrom NavHome _ = Home
navByFrom _ m = m
