{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interactive.TUI.Head where

import Brick
import Interactive.TUI.Core
import Interactive.TUI.Interpreter
import Interactive.TUI.Home
import Brick.Forms
import Language.Haskell.Interpreter (InterpreterError (UnknownError), eval, runInterpreter, setImports, MonadIO (liftIO), runStmt, parens, MonadInterpreter)
import Data.List (intersperse)
import Control.Lens ((^.), (.~))
import Brick.Focus
import Data.Text (unpack)
import System.Timeout (timeout)
import Data.Maybe (fromMaybe)
import Graphics.Vty hiding (Input)
import Text.Printf (printf)
import Control.Monad ((<=<))
import Animations.Append (appendAnimation)
import Reanimate (reanimate)
import Animations.Head (headAnimation)

makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . intersperse (vLimit 1 $ fill '·'))
    . newForm
        [ (str "xs: " <+>) @@= editTextField arg1 Arg1Field (Just 3)
        , makeNavField "head"
        ]

loadXs :: MonadIO m => State e -> m (Either InterpreterError String)
loadXs state = do
    let
        currentForm = state ^. form
        Input{_arg1 = xs} = formState currentForm
    runLimitedInterpreter . eval . unpack $ xs

validateXs :: MonadIO m => State e -> m (Either InterpreterError String)
validateXs = either (pure . Left) validateListStr <=< loadXs

loadResult :: MonadIO m => State e -> m (Either InterpreterError String)
loadResult state = do
    let
        currentForm = state ^. form
        Input{_arg1 = xs} = formState currentForm
    runLimitedInterpreter $ do
        runStmt . ("xs <- " <>) . parens . unpack $ xs
        eval "head xs"

previewEvent :: State e -> EventM Name (Next (State e))
previewEvent state = do
    let
        currentForm = state ^. form
        focus = focusGetCurrent . formFocus $ currentForm
    xsStr <- either makeErrorMessage id <$> validateXs state
    let
        prompt = case focus of
            Just Arg1Field -> "xs: " <> xsStr
            Just NavPreviewField -> printf "xs: %v" xsStr
            Just NavAnimateField -> printf "xs: %v" xsStr
            _ -> ""
    continue . (output .~ prompt) $ state

animateEvent :: State e -> EventM Name (Next (State e))
animateEvent state = do
    let
        currentForm = state ^. form
    xs <-
        either (pure . Left) splitListStr
        <=< either (pure . Left) validateListStr
        <=< loadXs
        $ state
    case xs of
        Right _ -> do
            liftIO $ reanimate headAnimation
            homeEvent state
        _ -> previewEvent state
