{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NumericUnderscores #-}

module Misc.InterpreterApp where

import Data.Text (Text, unpack)
import Control.Lens (makeLenses, (^.), (.~))
import Graphics.Vty (yellow, black, defAttr, white, red, Event (EvResize, EvKey), Key (KEsc, KEnter, KChar), mkVty, Output (setMode), Vty (outputIface), Mode (Mouse), Modifier (MCtrl))
import Brick ((<+>), padLeftRight, hLimit, vLimit, fill, str, AttrMap, on, attrMap, Widget, (<=>), Padding (Max), padRight, strWrap, BrickEvent (VtyEvent), EventM, Next, continue, halt, handleEventLensed, App (appDraw, App, appHandleEvent, appStartEvent, appChooseCursor, appAttrMap), customMain, strWrapWith, vBox)
import Language.Haskell.Interpreter (eval, runInterpreter, setImports, runStmt, parens, InterpreterError (UnknownError))
import Brick.Forms (Form (formFocus, formState), setFormConcat, newForm, editTextField, (@@=), invalidFormInputAttr, focusedFormInputAttr, renderForm, handleFormEvent)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Focus (focusRingCursor, focusGetCurrent)
import Graphics.Vty.Config (standardIOConfig)
import Text.Wrap (WrapSettings(WrapSettings, preserveIndentation, breakLongWords))
import Data.List (intersperse)

data Name = XsArgField | FArgField
    deriving (Eq, Ord, Show)

data Input = Input
    { _xsArg :: Text
    , _fArg :: Text
    }
    deriving (Eq, Show)

data State e = State
    { _form :: Form Input e Name
    , _prompt :: Widget Name
    }

makeLenses ''Input
makeLenses ''State

main :: IO ()
main = openApp

makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . intersperse (vLimit 1 $ fill '·'))
    . newForm
        [ formatWithLabel "xs:" @@= editTextField xsArg XsArgField (Just 3)
        , formatWithLabel "f:" @@= editTextField fArg FArgField (Just 3)
        ]
    where
        formatWithLabel label widget =
            padLeftRight 1
            $ (hLimit 10 . vLimit 1 . (<+> fill ' ') $ str label)
            <+> hLimit 100 widget

themeMap :: AttrMap
themeMap = attrMap defAttr
    [ (editAttr, yellow `on` black)
    , (editFocusedAttr, black `on` yellow)
    , (invalidFormInputAttr, white `on` red)
    , (focusedFormInputAttr, black `on` yellow)
    ]

customWrapSettings :: WrapSettings
customWrapSettings = WrapSettings
    { preserveIndentation = True
    , breakLongWords = True
    }

customStrWrap :: String -> Widget n
customStrWrap = strWrapWith customWrapSettings

drawUI :: State e -> [Widget Name]
drawUI state = [ui]
    where
        ui = hCenter formBox <=> hCenter noteBox <=> hCenter promptBox
        form' = state ^. form
        prompt' = state ^. prompt
        formBox =
            borderWithLabel (str "Input")
            . hLimit 100
            . renderForm
            $ form'
        promptBox =
            borderWithLabel (str "Preview")
            . vLimit 2
            . hLimit 100
            $ prompt'
        noteBox =
            borderWithLabel (str "Note")
            . hLimit 100
            . padRight Max
            . strWrap
            $ "Enter the arguments \"f\" and \"xs\" for the animation \
              \\"map f xs\".\n\
              \\"xs\" should be a list with no more than 8 elements; each \
              \element should not appear longer than 7 characters. For \
              \example, [1, 23, 456, 7890] and [3 .. 10] are okay.\n\
              \\"f\" should be a function that can be applied to each element \
              \of \"xs\", like (+ 1) for Ints or (\\x -> not x) for Bools, \
              \etc.\n\
              \At any point, press Ctrl + P to preview the argument you're \
              \entering."

loadArgs :: State e -> EventM Name (Either InterpreterError String)
loadArgs state = do
    let
        currentForm = state ^. form
        Input xs f = formState currentForm
        focus = focusGetCurrent $ formFocus currentForm
        evalArg = case focus of
            Just XsArgField -> unpack xs
            Just FArgField -> unpack f
            Nothing ->
                "No preview available.\n\
                \Choose an argument field first, then try again."
    result <- runInterpreter $ do
        setImports ["Prelude", "Text.Show.Functions"]
        eval evalArg
    pure $ take 200 <$> result

appEvent :: State e -> BrickEvent Name e -> EventM Name (Next (State e))
appEvent state (VtyEvent EvResize{}) = continue state
appEvent state (VtyEvent (EvKey KEsc [])) = halt state
appEvent state (VtyEvent (EvKey KEnter [])) = do
    result <- loadArgs state
    case result of
        Left (UnknownError "Timeout") ->
            continue
            . (prompt .~ customStrWrap
                "Timed out. No preview available.\n\
                \Perhaps you entered an infinite list or an infinitely \
                \recursive function.")
            $ state
        Left err ->
            continue . (prompt .~ customStrWrap (show err)) $ state
        Right _ -> halt state
appEvent state (VtyEvent (EvKey (KChar 'p') [MCtrl])) = do
    result <- loadArgs state
    case result of
        Left err ->
            continue . (prompt .~ customStrWrap (show err)) $ state
        Right expr -> continue . (prompt .~ customStrWrap expr) $ state
appEvent state event =
    continue . (prompt .~ vLimit 1 (fill ' '))
    =<< handleEventLensed state form handleFormEvent event


app :: App (State e) e Name
app = App
    { appDraw = drawUI
    , appHandleEvent = appEvent
    , appChooseCursor = focusRingCursor formFocus . (^. form)
    , appStartEvent = pure
    , appAttrMap = const themeMap
    }

openApp :: IO ()
openApp = do
    let
        buildVty = do
            v <- mkVty =<< standardIOConfig
            setMode (outputIface v) Mouse True
            pure v
        initialState = State
            { _form = makeForm (Input "[1 .. 5]" "(+ 1)")
            , _prompt = vLimit 1 $ fill ' '
            }
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty Nothing app initialState
    let
        finalForm = finalState ^. form
        Input finalXs finalF = formState finalForm
    finalResult <- runInterpreter $ do
        setImports ["Prelude", "Text.Show.Functions"]
        runStmt $ "let xs = " <> parens (unpack finalXs)
        runStmt $ "let f = " <> parens (unpack finalF)
        eval "map f xs"
    putStrLn $ either show (take 200) finalResult
