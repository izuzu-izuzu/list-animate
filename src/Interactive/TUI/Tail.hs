{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Interactive.TUI.Tail where

import Control.Lens ((.~), (^.))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (unpack)
import Language.Haskell.Interpreter
    ( InterpreterError
    , MonadIO (liftIO)
    , parens
    , typeOf
    )
import Text.Printf (printf)
import Text.RawString.QQ (r)

import Reanimate

import Brick
import Brick.Focus (focusGetCurrent)
import Brick.Forms
    ( Form (formFocus, formState)
    , editTextField
    , newForm
    , setFormConcat
    , (@@=)
    )
import Brick.Widgets.Center (hCenter)

import Animations.Tail

import Interactive.TUI.Core
import Interactive.TUI.Interpreter

import Utilities.Main

data LoadResult = LoadResult
    { _xs :: Either InterpreterError (String, [String])
    , _funcType :: Either InterpreterError String
    , _result :: Either InterpreterError (String, [String])
    }

{-|
    App page title.
-}
makeTitle :: String
makeTitle = "tail :: [a] -> [a]"

{-|
    Function expression.
-}
funcDef :: String
funcDef = "tail xs"

{-|
    Render the user input form.
-}
makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . (funcDefWidget :))
    . newForm
        [ (str "xs: " <+>) @@= editTextField arg1 Arg1Field (Just 2)
        , makeNavField
        ]
    where funcDefWidget = hCenter $ str funcDef

{-|
    Render instructions for the user.
-}
makeNote :: Widget n
makeNote = strWrap $ printf
    "Ensure the list xs contains between 1 and %v elements, and that each \
    \element can be shown in no more than %v characters.\n\
    \(Note that Strings are shown with quotation marks.)"
    maxListLength
    maxElementLength

{-|
    Expression used to determine the instantiated type of 'tail', given a valid
    string for @xs@.
-}
funcTypeEvalStr :: String -> String
funcTypeEvalStr xs = printf
    [r|
    let
        proxy :: t -> Proxy t
        proxy _ = Proxy
        tailProxy :: Proxy [a] -> [a] -> [a]
        tailProxy proxyXs = tail
    in
        tailProxy (proxy %v)
    |]
    (parens xs)

load :: (MonadIO m, MonadMask m) => State e -> m LoadResult
load state = do
    let
        Input{_arg1} = parensInput . formState . (^. form) $ state
        xsStr = unpack _arg1
        criteria =
            [ (not . null, EmptyListError)
            , (not . isLongerThan 6, ListTooLongError)
            , (not . any (isLongerThan 6), ElementTooLongError)
            ]
    xs <- runExceptT $ do
        xsExpr <- validateListStrWith criteria =<< runLimitedEvalWithType xsStr
        xsVal <- splitListStr xsExpr
        pure (xsExpr, xsVal)
    funcType <-
        runExceptT
        . runLimitedInterpreter
        . typeOf
        $ funcTypeEvalStr xsStr
    result <- runExceptT $ do
        resultExpr <- runLimitedEvalWithType $ printf "tail %v" xsStr
        resultVal <- splitListStr resultExpr
        pure (resultExpr, resultVal)
    pure $ LoadResult xs funcType result

{-|
    Preview the argument and display any error prompts, either when the user
    selects [Preview] or right after an animation is rendered.
-}
previewEvent :: State e -> EventM Name (Next (State e))
previewEvent state = do
    loadResult <- load state
    previewEvent' state loadResult

previewEvent' :: State e -> LoadResult -> EventM Name (Next (State e))
previewEvent' state loadResult = do
    let
        focus = focusGetCurrent . formFocus . (^. form) $ state
        LoadResult{_xs, _funcType, _result} = loadResult
        xsWidget = either makeErrorWidget (strWrapBreak . fst) _xs
        resultWidget = either makeErrorWidget (strWrapBreak . fst) _result
        animateResultPrompt =
            withAttr "bold" $ str (funcDef <> ": ") <+> resultWidget
        animatePrompt = case (_xs, _result) of
            (Right _, Right _) -> animateResultPrompt
            (Right _, Left _) -> animateErrorPrompt
            _ -> emptyWidget
        previewPrompt = case (_xs, _result) of
            (Right _, Right _) -> animateAvailablePrompt
            _ -> emptyWidget
        prompt = case focus of
            Just Arg1Field -> str "xs: " <+> xsWidget
            Just NavPreviewField -> vBox
                [ str "xs: " <+> xsWidget
                , previewPrompt
                ]
            Just NavAnimateField -> vBox
                [ str "xs: " <+> xsWidget
                , animatePrompt
                ]
            _ -> emptyWidget
    continue . (output .~ prompt) $ state

{-|
    When the user selects [Animate], render an animation using the given
    argument if possible, then display either the result or any error
    messages.
-}
animateEvent :: State e -> EventM Name (Next (State e))
animateEvent state = do
    loadResult@LoadResult{_xs, _funcType, _result} <- load state
    case (_xs, _funcType, _result) of
        (Right (_, xs'), Right funcType', Right _) ->
            liftIO . reanimate $ tailDynamicAnimation funcType' xs'
        _ -> pure ()
    previewEvent' state loadResult

{-|
    Prompt for when the given argument is valid and an animation is available.
-}
animateAvailablePrompt :: Widget Name
animateAvailablePrompt = withAttr "actionAvailable"
    $ strWrap "Select [Animate] to view the animation."

{-|
    Prompt for when the given argument is valid but the result cannot be
    calculated (perhaps due to a type mismatch or some other error).
-}
animateErrorPrompt :: Widget Name
animateErrorPrompt = withAttr "error"
    $ strWrap
        "Something went wrong when preparing the animation.\n\
        \Ensure your argument has the correct type, then try again."
