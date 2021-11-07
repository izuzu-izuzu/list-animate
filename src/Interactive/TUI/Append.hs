{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Interactive.TUI.Append where

import Control.Lens ((.~), (^.))
import Data.List (intersperse)
import Data.Text (unpack)
import Language.Haskell.Interpreter
    ( InterpreterError
    , MonadIO (liftIO)
    , parens, setImports, typeOf
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
import Brick.Widgets.Center (hCenter)
import Text.RawString.QQ (r)

makeTitle :: String
makeTitle = "(++) :: [a] -> [a] -> [a]"

funcName :: String
funcName = "(++)"

funcDef :: String
funcDef = "xs ++ ys"

makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . (funcDefWidget :))
    . newForm
        [ (str "xs: " <+>) @@= editTextField arg1 Arg1Field (Just 2)
        , (str "ys: " <+>) @@= editTextField arg2 Arg2Field (Just 2)
        , makeNavField
        ]
    where funcDefWidget = hCenter $ str funcDef

makeNote :: Widget n
makeNote = strWrap $ printf
    "Ensure the lists xs and ys each contain between 1 and %v elements, and \
    \that each element can be shown in no more than %v characters.\n\
    \(Note that Strings are shown with quotation marks.)"
    maxListLength
    maxElementLength

loadValidateXs :: MonadIO m => State e -> m (Either InterpreterError String)
loadValidateXs state = do
    let Input{_arg1} = parensInput . formState . (^. form) $ state
    xs <- runLimitedEvalWithType $ unpack _arg1
    either (pure . Left) validateListStr xs

loadValidateYs :: MonadIO m => State e -> m (Either InterpreterError String)
loadValidateYs state = do
    let Input{_arg2} = parensInput . formState . (^. form) $ state
    ys <- runLimitedEvalWithType $ unpack _arg2
    either (pure . Left) validateListStr ys

resultTypeExpr :: String -> String -> String
resultTypeExpr = printf
    [r|
    let
        proxy :: t -> Proxy t
        proxy _ = Proxy
        appendProxy :: Proxy [a] -> Proxy [a] -> [a] -> [a] -> [a]
        appendProxy proxyXs proxyYs = (++)
    in
        appendProxy (proxy %v) (proxy %v)
    |]

loadFuncType :: MonadIO m => State e -> m (Either InterpreterError String)
loadFuncType state = do
    xs <- fmap parens <$> loadValidateXs state
    ys <- fmap parens <$> loadValidateYs state
    case (xs, ys) of
        (Right xs', Right ys') -> runLimitedInterpreter $ do
            setImports
                [ "Prelude"
                , "Data.List"
                , "Text.Show.Functions"
                , "Data.Proxy"
                ]
            typeOf $ resultTypeExpr xs' ys'
        (Left xsErr, _) -> pure $ Left xsErr
        (_, Left ysErr) -> pure $ Left ysErr

loadRawResult :: MonadIO m => State e -> m (Either InterpreterError String)
loadRawResult state = do
    let
        Input{_arg1, _arg2} = parensInput . formState . (^. form) $ state
        xs = parens $ unpack _arg1
        ys = parens $ unpack _arg2
    runLimitedEvalWithType $ printf "%v ++ %v" xs ys

loadResult :: MonadIO m => State e -> m (Either InterpreterError [String])
loadResult state = do
    rawResult <- fmap parens <$> loadRawResult state
    either (pure . Left) splitListStr rawResult

previewEvent :: State e -> EventM Name (Next (State e))
previewEvent state = do
    xs <- loadValidateXs state
    ys <- loadValidateYs state
    rawResult <- loadRawResult state
    result <- loadResult state
    let
        focus = focusGetCurrent . formFocus . (^. form) $ state
        xsStr = either makeErrorMessage id xs
        ysStr = either makeErrorMessage id ys
        resultStr = either makeErrorMessage id rawResult
        animateResultPrompt =
            withAttr "bold" $ str (funcDef <> ": ") <+> strWrap resultStr
        animatePrompt = case (xs, ys, result) of
            (Right _, Right _, Right _) -> animateResultPrompt
            (Right _, Right _, Left _) -> animateErrorPrompt
            _ -> emptyWidget
        previewPrompt = case (xs, ys, result) of
            (Right _, Right _, Right _) ->
                animateAvailablePrompt
            _ -> emptyWidget
        prompt = case focus of
            Just Arg1Field -> str "xs: " <+> strWrap xsStr
            Just Arg2Field -> str "ys: " <+> strWrap ysStr
            Just NavPreviewField ->
                (str "xs: " <+> strWrap xsStr)
                <=> (str "ys: " <+> strWrap ysStr)
                <=> previewPrompt
            Just NavAnimateField ->
                (str "xs: " <+> strWrap xsStr)
                <=> (str "ys: " <+> strWrap ysStr)
                <=> animatePrompt
            _ -> emptyWidget
    continue . (output .~ prompt) $ state

animateEvent :: State e -> EventM Name (Next (State e))
animateEvent state = do
    xs <- either (pure . Left) splitListStr =<< loadValidateXs state
    ys <- either (pure . Left) splitListStr =<< loadValidateYs state
    funcType <- loadFuncType state
    result <- loadResult state
    case (xs, ys, funcType, result) of
        (Right xs', Right ys', Right funcType', Right _) ->
            liftIO . reanimate $ appendDynamicAnimation funcType' xs' ys'
        _ -> pure ()
    previewEvent state

animateAvailablePrompt :: Widget Name
animateAvailablePrompt = withAttr "actionAvailable"
    $ strWrap "Select [Animate] to view the animation."

animateErrorPrompt :: Widget Name
animateErrorPrompt = withAttr "error"
    $ strWrap
        "Something went wrong when preparing the animation.\n\
        \Ensure your arguments have the correct types, then try again."

