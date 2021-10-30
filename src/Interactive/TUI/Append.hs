{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interactive.TUI.Append where

import Control.Lens ((.~), (^.))
import Data.List (intersperse)
import Data.Text (unpack)
import Language.Haskell.Interpreter
    ( InterpreterError
    , MonadIO (liftIO)
    , parens
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

loadValidateXs :: MonadIO m => State e -> m (Either InterpreterError String)
loadValidateXs state = do
    let Input{_arg1 = xs} = formState $ state ^. form
    xs' <- runLimitedEvalWithType $ unpack xs
    either (pure . Left) validateListStr xs'

loadValidateYs :: MonadIO m => State e -> m (Either InterpreterError String)
loadValidateYs state = do
    let Input{_arg2 = ys} = formState $ state ^. form
    ys' <- runLimitedEvalWithType $ unpack ys
    either (pure . Left) validateListStr ys'

loadResult :: MonadIO m => State e -> m (Either InterpreterError [String])
loadResult state = do
    let
        Input{_arg1 = xs, _arg2 = ys} = formState $ state ^. form
        xs' = parens $ unpack xs
        ys' = parens $ unpack ys
    result <- runLimitedEvalWithType $ printf "%v ++ %v" xs' ys'
    either (pure . Left) splitListStr result

previewEvent :: State e -> EventM Name (Next (State e))
previewEvent state = do
    xs <- loadValidateXs state
    ys <- loadValidateYs state
    result <- loadResult state
    let
        focus = focusGetCurrent . formFocus . (^. form) $ state
        xsStr = either makeErrorMessage id xs
        ysStr = either makeErrorMessage id ys
        animateResultMessage = case (xs, ys, result) of
            (Right _, Right _, Right _) ->
                "\n\n\
                \Select [Animate] to view the animation."
            (Right _, Right _, Left _) ->
                "\n\n\
                \Something went wrong when preparing the animation.\n\
                \Ensure your arguments have the correct types, then try again."
            _ -> "" :: String
        previewResultMessage = case (xs, ys, result) of
            (Right _, Right _, Right _) ->
                "\n\n\
                \Select [Animate] to view the animation."
            _ -> "" :: String
        prompt = case focus of
            Just Arg1Field -> "xs: " <> xsStr
            Just Arg2Field -> "ys: " <> ysStr
            Just NavPreviewField -> printf
                "xs: %v\n\nys: %v%v"
                xsStr
                ysStr
                previewResultMessage
            Just NavAnimateField -> printf
                "xs: %v\n\nys: %v%v"
                xsStr
                ysStr
                animateResultMessage
            _ -> ""
    continue . (output .~ prompt) $ state

animateEvent :: State e -> EventM Name (Next (State e))
animateEvent state = do
    xs <- either (pure . Left) splitListStr =<< loadValidateXs state
    ys <- either (pure . Left) splitListStr =<< loadValidateYs state
    result <- loadResult state
    case (xs, ys, result) of
        (Right xs', Right ys', Right _) -> do
            liftIO . reanimate $ appendDynamicAnimation xs' ys'
            homeEvent state
        _ -> previewEvent state
