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
import Brick.Forms (Form (formState, formFocus), editTextField, focusedFormInputAttr, invalidFormInputAttr, newForm, radioField, renderForm, handleFormEvent, listField, setFormFocus, setFieldConcat)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Graphics.Vty (Event (EvKey, EvResize), Key (KEnter, KEsc), black, defAttr, red, white, yellow, mkVty, standardIOConfig, Output (setMode), Vty (outputIface), blue, green, brightBlack)
import Data.Vector (fromList)
import Brick.Focus (focusRingCursor, focusGetCurrent)
import qualified Graphics.Vty as Vty
import Text.Pretty.Simple (pPrint)
import Brick.Widgets.List (listAttr, listSelectedAttr)
import Data.Maybe (fromMaybe)
import Data.List (intersperse)

data Input = Input
    { _mode1Edit :: Text
    , _mode2Edit1 :: Text
    , _mode2Edit2 :: Text
    , _mode3Edit :: Text
    , _navChoice :: Maybe NavChoice
    }
    deriving Show

data Name
    = Mode1EditField
    | Mode2Edit1Field
    | Mode2Edit2Field
    | Mode3EditField
    | NavChoiceField
    | NavPrevField
    | NavNextField
    | NavHomeField
    | NavQuitField
    deriving (Bounded, Enum, Eq, Ord, Show)

data NavChoice = NavPrev | NavNext | NavHome | NavQuit
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

makeForm :: Mode -> Input -> Form Input e Name
makeForm Mode1 = newForm
    [ editTextField mode1Edit Mode1EditField (Just 3)
    , setFieldConcat (hBox . intersperse (str "  ")) . listField
        (const $ fromList [NavPrev .. NavQuit])
        navChoice
        (\b i -> str $ (if b then "> " else "  ") <> show i)
        1
        NavChoiceField
    ]
makeForm Mode2 = newForm
    [ editTextField mode2Edit1 Mode2Edit1Field (Just 2)
    , editTextField mode2Edit2 Mode2Edit2Field (Just 1)
    , setFieldConcat (hBox . intersperse (str "  ")) . listField
        (const $ fromList [NavPrev .. NavQuit])
        navChoice
        (\b i -> str $ (if b then "> " else "  ") <> show i)
        1
        NavChoiceField
    ]
makeForm Mode3 = newForm
    [ editTextField mode3Edit Mode3EditField (Just 3)
    , setFieldConcat (hBox . intersperse (str "  ")) . listField
        (const $ fromList [NavPrev .. NavQuit])
        navChoice
        (\b i -> str $ (if b then "> " else "  ") <> show i)
        1
        NavChoiceField
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
        m' = case nav of
            Nothing -> m
            Just n -> navByFrom n m
        f' = case nav of
            Nothing -> f
            Just _ -> makeForm m' $ formState f
        p' = case nav of
            Nothing -> p
            Just _ -> makePrompt m' $ formState f

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
        state' = updateMode state
        focus =
            fromMaybe NavChoiceField
            . focusGetCurrent
            . formFocus
            . (^. form)
            $ state
        form' = setFormFocus focus $ state' ^. form
    continue . (form .~ form') $ state'
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
   where initialInput = Input "m1" "m2.1" "m2.2" "m3" Nothing
