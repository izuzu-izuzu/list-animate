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

import Control.Lens (makeLenses, (^.), (.~))
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

data Input = Input
    { _mode1Edit :: Text
    , _mode2Edit1 :: Text
    , _mode2Edit2 :: Text
    , _mode3Edit :: Text
    , _navChoice :: NavChoice
    }
    deriving Show

data Name
    = Mode1EditField
    | Mode2Edit1Field
    | Mode2Edit2Field
    | Mode3EditField
    | NavChoiceField
    | NavCurrField
    | NavHomeField
    | NavPrevField
    | NavNextField
    | NavQuitField
    deriving (Bounded, Enum, Eq, Ord, Show)

data NavChoice = NavCurr | NavHome | NavPrev | NavNext | NavQuit
    deriving (Bounded, Enum, Eq, Show)

data Mode = Mode1 | Mode2 | Mode3
    deriving (Bounded, Enum, Eq, Show)

data State e = State
    { _mode :: Mode
    , _form :: Form Input e Name
    , _prompt :: String
    }

makeLenses ''Input
makeLenses ''State

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

makeNavField :: Text -> Input -> FormFieldState Input e Name
makeNavField currentLabel =
    setFieldConcat (\(x:xs) -> x <+> fill ' ' <+> hBox xs)
    . radioCustomField
        ' '
        ' '
        ' '
        navChoice
        (zip3
            [NavCurr .. NavQuit]
            [NavCurrField .. NavQuitField]
            (fmap
                (<> "    ")
                [ "*" <> currentLabel <> "*"
                , "[Home]"
                , "[Previous]"
                , "[Next]"
                , "[Quit]"
                ]))
       
makeForm :: Mode -> Input -> Form Input e Name
makeForm Mode1 = newForm
    [ editTextField mode1Edit Mode1EditField (Just 3)
    , makeNavField "Mode1"
    ]
makeForm Mode2 = newForm
    [ editTextField mode2Edit1 Mode2Edit1Field (Just 2)
    , editTextField mode2Edit2 Mode2Edit2Field (Just 1)
    , makeNavField "Mode2"
    ]
makeForm Mode3 = newForm
    [ editTextField mode3Edit Mode3EditField (Just 3)
    , makeNavField "Mode3"
    ]

makePrompt :: Mode -> Input -> String
makePrompt _ _ = "<prompt>"

drawUI :: State e -> [Widget Name]
drawUI s = [renderForm f <=> str p]
    where
        m = s ^. mode
        f = s ^. form
        p = s ^. prompt

syncToMode :: State e -> State e
syncToMode s@State{_mode = m, _form = f} = s
    { _form = makeForm m $ formState f
    , _prompt = makePrompt m $ formState f
    }

navByFrom :: NavChoice -> Mode -> Mode
navByFrom NavCurr m = m
navByFrom NavPrev Mode1 = Mode3
navByFrom NavPrev m = pred m
navByFrom NavNext Mode3 = Mode1
navByFrom NavNext m = succ m
navByFrom NavHome _ = Mode1
navByFrom NavQuit m = m

navigateMode :: NavChoice -> State e -> State e
navigateMode nav s@State{_mode = m, _form = f} = s
    { _mode = m'
    , _form = makeForm m' $ formState f
    , _prompt = makePrompt m' $ formState f
    }
    where m' = navByFrom nav m

updateMode :: State e -> State e
updateMode s@State{_mode = m, _form = f, _prompt = p} = s
    { _mode = m'
    , _form = f'
    , _prompt = p'
    }
    where
        nav = (^. navChoice) $ formState f
        m' = navByFrom nav m
        f' = case nav of
            NavCurr -> f
            _ -> makeForm m' $ formState f
        p' = case nav of
            NavCurr -> p
            _ -> makePrompt m' $ formState f

themeMap :: AttrMap
themeMap = attrMap defAttr
    [ (editAttr, white `on` brightBlack)
    , (editFocusedAttr, black `on` yellow)
    , (invalidFormInputAttr, white `on` red)
    , (focusedFormInputAttr, black `on` yellow)
    , (listAttr, bg blue)
    , (listSelectedAttr, white `on` green)
    ]

appEvent
    :: State e
    -> BrickEvent Name e
    -> EventM Name (Next (State e))
appEvent state (VtyEvent EvResize{}) = continue state
appEvent state (VtyEvent (EvKey KEsc [])) = halt state
appEvent state (VtyEvent (EvKey KEnter [])) = do
    let
        focus = 
            fromMaybe NavCurrField
            . focusGetCurrent
            . formFocus
            . (^. form)
            $ state
    state' <-
        if focus `elem` [NavCurrField .. NavQuitField]
        then updateMode <$> handleEventLensed
             state
             form
             handleFormEvent
             (VtyEvent (EvKey (KChar ' ') []))
        else pure $ updateMode state
    let form' = setFormFocus focus $ state' ^. form
    case focus of
        NavQuitField -> halt state'
        _ -> continue . (form .~ form') $ state'
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
   { _mode = Mode1
   , _form = makeForm Mode1 initialInput
   , _prompt = makePrompt Mode1 initialInput
   }
   where initialInput = Input "m1" "m2.1" "m2.2" "m3" NavCurr
