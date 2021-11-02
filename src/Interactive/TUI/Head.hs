{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Interactive.TUI.Head where

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

import Animations.Head

import Interactive.TUI.Core
import Interactive.TUI.Home
import Interactive.TUI.Interpreter

makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . intersperse (vLimit 1 $ fill '·'))
    . newForm
        [ (str "xs: " <+>) @@= editTextField arg1 Arg1Field (Just 3)
        , makeNavField "head"
        ]

loadValidateXs :: MonadIO m => State e -> m (Either InterpreterError String)
loadValidateXs state = do
    let Input{_arg1 = xs} = formState $ state ^. form
    xs' <- runLimitedEvalWithType $ unpack xs
    either (pure . Left) validateListStr xs'

loadResult :: MonadIO m => State e -> m (Either InterpreterError String)
loadResult state = do
    let
        Input{_arg1 = xs} = formState $ state ^. form
        xs' = parens $ unpack xs
    runLimitedEvalWithType $ printf "head %v" xs'

previewEvent :: State e -> EventM Name (Next (State e))
previewEvent state = do
    xs <- loadValidateXs state
    result <- loadResult state
    let
        focus = focusGetCurrent . formFocus . (^. form) $ state
        xsStr = either makeErrorMessage id xs
        animateResultMessage = case (xs, result) of
            (Right _, Right _) ->
                "\n\n\
                \Select [Animate] to view the animation."
            (Right _, Left _) ->
                "\n\n\
                \Something went wrong when preparing the animation.\n\
                \Ensure your arguments have the correct types, then try again."
            _ -> "" :: String
        previewResultMessage = case (xs, result) of
            (Right _, Right _) ->
                "\n\n\
                \Select [Animate] to view the animation."
            _ -> "" :: String
        prompt = case focus of
            Just Arg1Field -> "xs: " <> xsStr
            Just NavPreviewField -> printf
                "xs: %v%v"
                xsStr
                previewResultMessage
            Just NavAnimateField -> printf
                "xs: %v%v"
                xsStr
                animateResultMessage
            _ -> ""
    continue . (output .~ prompt) $ state

animateEvent :: State e -> EventM Name (Next (State e))
animateEvent state = do
    xs <- either (pure . Left) splitListStr =<< loadValidateXs state
    result <- loadResult state
    case (xs, result) of
        (Right xs', Right _) -> do
            liftIO . reanimate $ headDynamicAnimation xs'
            homeEvent state
        _ -> previewEvent state
