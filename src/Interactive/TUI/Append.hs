{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interactive.TUI.Append where

import Control.Lens ((.~), (^.))
import Control.Monad ((<=<))
import Data.List (intersperse)
import Data.Text (unpack)
import Language.Haskell.Interpreter
    ( InterpreterError
    , MonadIO (liftIO)
    , eval
    , parens
    , runStmt
    )
import Text.Printf (printf)

import Reanimate (reanimate)

import Brick
import Brick.Focus (focusGetCurrent)
import Brick.Forms (Form (..), editTextField, newForm, setFormConcat, (@@=))

import Animations.Append (appendDynamicAnimation)

import Interactive.TUI.Core
import Interactive.TUI.Home
import Interactive.TUI.Interpreter

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
        let
            xs' = parens . unpack $ xs
            ys' = parens . unpack $ ys
        eval $ printf "%v ++ %v" xs' ys'
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
    result <- loadResult state
    case (xs, ys, result) of
        (Right xs', Right ys', Right _) -> do
            liftIO . reanimate $ appendDynamicAnimation xs' ys'
            homeEvent state
        _ -> previewEvent state
