{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interactive.TUI.Append where

import Brick
import Interactive.TUI.Core
import Interactive.TUI.Interpreter
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
import Interactive.TUI.Core

makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . intersperse (vLimit 1 $ fill '·'))
    . newForm
        [ (str "xs: " <+>) @@= editTextField arg1 Arg1Field (Just 3)
        , (str "ys: " <+>) @@= editTextField arg2 Arg2Field (Just 3)
        , makeNavField "(++)"
        ]

loadXs :: MonadIO m => State e -> m (Either InterpreterError String)
loadXs state = do
    let
        currentForm = state ^. form
        Input{_arg1 = xs} = formState currentForm
    runLimitedInterpreter . eval . unpack $ xs

validateXs :: MonadIO m => State e -> m (Either InterpreterError String)
validateXs = either (pure . Left) validateListStr <=< loadXs

loadYs :: MonadIO m => State e -> m (Either InterpreterError String)
loadYs state = do
    let
        currentForm = state ^. form
        Input{_arg2 = ys} = formState currentForm
    runLimitedInterpreter . eval . unpack $ ys

validateYs :: MonadIO m => State e -> m (Either InterpreterError String)
validateYs = either (pure . Left) validateListStr <=< loadYs

loadResult :: MonadIO m => State e -> m (Either InterpreterError [String])
loadResult state = do
    let
        currentForm = state ^. form
        Input{_arg1 = xs, _arg2 = ys} = formState currentForm
    result <- runLimitedInterpreter $ do
        runStmt . ("xs <- " <>) . parens . unpack $ xs
        runStmt . ("ys <- " <>) . parens . unpack $ ys
        eval "xs ++ ys"
    either (pure . Left) splitListStr result

previewEvent :: State e -> EventM Name (Next (State e))
previewEvent state = do
    let
        currentForm = state ^. form
        focus = focusGetCurrent . formFocus $ currentForm
    xsStr <- either makeErrorMessage id <$> validateXs state
    ysStr <- either makeErrorMessage id <$> validateYs state
    let
        prompt = case focus of
            Just Arg1Field -> "xs: " <> xsStr
            Just Arg2Field -> "ys: " <> ysStr
            Just NavPreviewField -> printf "xs: %v\n\nys: %v" xsStr ysStr
            Just NavAnimateField -> printf "xs: %v\n\nys: %v" xsStr ysStr
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
    ys <-
        either (pure . Left) splitListStr
        <=< either (pure . Left) validateListStr
        <=< loadYs
        $ state
    case (xs, ys) of
        (Right xs', Right ys') -> do
            halt state
            liftIO $ print xs'
            liftIO $ print ys'
            halt state
        _ -> previewEvent state
