{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interactive.TUI.Tail where

import Control.Lens ((.~), (^.), view)
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

import Animations.Tail

import Interactive.TUI.Core
import Interactive.TUI.Home
import Interactive.TUI.Interpreter
import Brick.Widgets.Center (hCenter)

makeTitle :: String
makeTitle = "tail :: [a] -> [a]"

funcDef :: String
funcDef = "tail xs"

makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . (funcDefWidget :))
    . newForm
        [ (str "xs: " <+>) @@= editTextField arg1 Arg1Field (Just 2)
        , makeNavField
        ]
    where funcDefWidget = hCenter $ str funcDef

makeNote :: Widget n
makeNote = strWrap $ printf
    "Ensure the list xs contains between 1 and %v elements, and that each \
    \element can be shown in no more than %v characters.\n\
    \(Note that Strings are shown with quotation marks.)"
    maxListLength
    maxElementLength

loadValidateXs :: MonadIO m => State e -> m (Either InterpreterError String)
loadValidateXs state = do
    let Input{_arg1} = parensInput . formState . (^. form) $ state
    xs <- runLimitedEvalWithType $ unpack _arg1
    either (pure . Left) validateListStr xs

loadResult :: MonadIO m => State e -> m (Either InterpreterError String)
loadResult state = do
    xs <- fmap parens <$> loadValidateXs state
    either (pure . Left) (runLimitedEvalWithType . printf "tail %v") xs

previewEvent :: State e -> EventM Name (Next (State e))
previewEvent state = do
    xs <- loadValidateXs state
    result <- loadResult state
    let
        focus = focusGetCurrent . formFocus . (^. form) $ state
        xsStr = either makeErrorMessage id xs
        resultStr = either makeErrorMessage id result
        animateResultPrompt =
            withAttr "bold" $ str (funcDef <> ": ") <+> strWrap resultStr
        animatePrompt = case (xs, result) of
            (Right _, Right _) -> animateResultPrompt
            (Right _, Left _) -> animateErrorPrompt
            _ -> emptyWidget
        previewPrompt = case (xs, result) of
            (Right _, Right _) -> animateAvailablePrompt
            _ -> emptyWidget
        prompt = case focus of
            Just Arg1Field -> str "xs: " <+> strWrap xsStr
            Just NavPreviewField ->
                (str "xs: " <+> strWrap xsStr)
                <=> previewPrompt
            Just NavAnimateField ->
                (str "xs: " <+> strWrap xsStr)
                <=> animatePrompt
            _ -> emptyWidget
    continue . (output .~ prompt) $ state

animateEvent :: State e -> EventM Name (Next (State e))
animateEvent state = do
    xs <- either (pure . Left) splitListStr =<< loadValidateXs state
    result <- loadResult state
    case (xs, result) of
        (Right xs', Right _) ->
            liftIO . reanimate $ tailDynamicAnimation xs'
        _ -> pure ()
    previewEvent state

animateAvailablePrompt :: Widget Name
animateAvailablePrompt = withAttr "actionAvailable"
    $ strWrap "Select [Animate] to view the animation."

animateErrorPrompt :: Widget Name
animateErrorPrompt = withAttr "error"
    $ strWrap
        "Something went wrong when preparing the animation.\n\
        \Ensure your argument has the correct type, then try again."
