{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Interactive.TUI.Core where

import Control.Lens (makeLenses, (%~))
import Data.Text (Text, pack, unpack)
import Graphics.Vty
    ( Event (EvKey, EvResize)
    , Key (KChar, KEnter, KEsc)
    , Modifier
    )
import Language.Haskell.Interpreter (InterpreterError, parens)

import Brick
import Brick.Forms (Form, FormFieldState, radioCustomField, setFieldConcat)

import Interactive.TUI.Interpreter

{-|
    The user input record type, which hold 5 text input fields and a
    placeholder field for buttons. Each text input field corresponds to a
    function argument, so the app can receive a maximum of 5 arguments to
    supply to a function.
-}
data Input = Input
    { _arg1 :: Text
    , _arg2 :: Text
    , _arg3 :: Text
    , _arg4 :: Text
    , _arg5 :: Text
    , _emptyInputField :: ()
    }
    deriving Show

{-|
    Resource names for input fields and buttons.

    * Names beginning with @Arg@ correspond to input fields.
    * Names beginning with @Nav@ correspond to navigation buttons.
    * Names beginning with @Select@ correspond to choices on the home page.
-}
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

{-|
    Navigation choices.
-}
data NavChoice = NavCurrent | NavHome | NavQuit | NavPreview | NavAnimate
    deriving (Bounded, Enum, Eq, Show)

{-|
    App modes. Each mode corresponds to a different app page, and the app can
    only be in one mode at a time.
-}
data Mode
    = Home
    | FnAppend
    | FnHead
    | FnTail
    | FnInit
    | FnLast
    deriving (Bounded, Enum, Eq, Show)

{-|
    The app state record type, which holds information on the current app mode,
    the current state of the user input form, and how to render user
    instructions and prompts.
-}
data State e = State
    { _mode :: Mode
    , _form :: Form Input e Name
    , _note :: Widget Name
    , _output :: Widget Name
    }

{-|
    The resizing event.
-}
pattern ResizeEvent :: BrickEvent n e
pattern ResizeEvent <- VtyEvent EvResize{}

{-|
    The event when the user presses the Esc key, possibly with some modifiers.
-}
pattern KEscEvent :: [Modifier] -> BrickEvent n e
pattern KEscEvent mods <- VtyEvent (EvKey KEsc mods)

{-|
    The event when the user presses the Enter key, possibly with some
    modifiers.
-}
pattern KEnterEvent :: [Modifier] -> BrickEvent n e
pattern KEnterEvent mods = VtyEvent (EvKey KEnter mods)

{-|
    The event when the user presses a character key, possibly with some
    modifiers.
-}
pattern KCharEvent :: Char -> [Modifier] -> BrickEvent n e
pattern KCharEvent char mods = VtyEvent (EvKey (KChar char) mods)

makeLenses ''Input
makeLenses ''State

{-|
    Resource names corresponding to navigation buttons.
-}
navFieldNames :: [Name]
navFieldNames =
    [ NavCurrentField
    , NavHomeField
    , NavQuitField
    , NavPreviewField
    , NavAnimateField
    ]

{-|
    Render navigation buttons as part of the user input form.
-}
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

{-|
    Switch the app mode based on the given navigation choice.
-}
navByFrom :: NavChoice -> Mode -> Mode
navByFrom NavHome _ = Home
navByFrom _ m = m

{-|
    Parenthesize all input fields to make them easier to parse as function
    arguments.
-}
parensInput :: Input -> Input
parensInput =
    foldl1 (.)
    . fmap (%~ parensText)
    $ [arg1, arg2, arg3, arg4, arg5]
    where parensText = pack . parens . unpack

makeErrorWidget :: InterpreterError -> Widget n
makeErrorWidget = withAttr "error" . strWrap . makeErrorMessage
