{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Interactive.TUI.Append where

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

import Animations.Append

import Interactive.TUI.Core
import Interactive.TUI.Interpreter

data LoadResult = LoadResult
    { _xs :: Either InterpreterError (String, [String])
    , _ys :: Either InterpreterError (String, [String])
    , _funcType :: Either InterpreterError String
    , _result :: Either InterpreterError (String, [String])
    }

{-|
    App page title.
-}
makeTitle :: String
makeTitle = "(++) :: [a] -> [a] -> [a]"

{-|
    Function name.
-}
funcName :: String
funcName = "(++)"

{-|
    Function expression.
-}
funcDef :: String
funcDef = "xs ++ ys"

{-|
    Render the user input form.
-}
makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . (funcDefWidget :))
    . newForm
        [ (str "xs: " <+>) @@= editTextField arg1 Arg1Field (Just 2)
        , (str "ys: " <+>) @@= editTextField arg2 Arg2Field (Just 2)
        , makeNavField
        ]
    where funcDefWidget = hCenter $ str funcDef

{-|
    Render instructions for the user.
-}
makeNote :: Widget n
makeNote = strWrap $ printf
    "Ensure the lists xs and ys each contain between 1 and %v elements, and \
    \that each element can be shown in no more than %v characters.\n\
    \(Note that Strings are shown with quotation marks.)"
    maxListLength
    maxElementLength

{-|
    Expression used to determine the instantiated type of '(++)', given valid
    strings for @xs@ and @ys@.
-}
funcTypeEvalStr :: String -> String -> String
funcTypeEvalStr xs ys = printf
    [r|
    let
        proxy :: t -> Proxy t
        proxy _ = Proxy
        appendProxy :: Proxy [a] -> Proxy [a] -> [a] -> [a] -> [a]
        appendProxy proxyXs proxyYs = (++)
    in
        appendProxy (proxy %v) (proxy %v)
    |]
    (parens xs)
    (parens ys)

loadAll :: (MonadIO m, MonadMask m) => State e -> m LoadResult
loadAll state = do
    let
        Input{_arg1, _arg2} = parensInput . formState . (^. form) $ state
        xsStr = unpack _arg1
        ysStr = unpack _arg2
        criteria =
            [ (not . isLongerThan 6, ListTooLongError)
            , (not . any (isLongerThan 6), ElementTooLongError)
            ]
    xs <- runExceptT $ do
        xsExpr <- validateListStrWith criteria =<< runLimitedEvalWithType xsStr
        xsVal <- splitListStr xsExpr
        pure (xsExpr, xsVal)
    ys <- runExceptT $ do
        ysExpr <- validateListStrWith criteria =<< runLimitedEvalWithType ysStr
        ysVal <- splitListStr ysExpr
        pure (ysExpr, ysVal)
    funcType <-
        runExceptT
        . runLimitedInterpreter
        . typeOf
        $ funcTypeEvalStr xsStr ysStr
    result <- runExceptT $ do
        resultExpr <- runLimitedEvalWithType $ printf "%v ++ %v" xsStr ysStr
        resultVal <- splitListStr resultExpr
        pure (resultExpr, resultVal)
    pure $ LoadResult xs ys funcType result

{-|
    Preview the arguments and display any error prompts, either when the user
    selects [Preview] or right after an animation is rendered.
-}
previewEvent :: State e -> EventM Name (Next (State e))
previewEvent state = do
    loadResult <- loadAll state
    previewEvent' state loadResult

previewEvent' :: State e -> LoadResult -> EventM Name (Next (State e))
previewEvent' state loadResult = do
    let
        focus = focusGetCurrent . formFocus . (^. form) $ state
        LoadResult{_xs, _ys, _funcType, _result} = loadResult
        ~[xsWidget, ysWidget, resultWidget] =
            either
                (withAttr "error" . strWrap . makeErrorMessage)
                (strWrap . fst)
            <$> [_xs, _ys, _result]
        animateResultPrompt =
            withAttr "bold" $ str (funcDef <> ": ") <+> resultWidget
        animatePrompt = case (_xs, _ys, _result) of
            (Right _, Right _, Right _) -> animateResultPrompt
            (Right _, Right _, Left _) -> animateErrorPrompt
            _ -> emptyWidget
        previewPrompt = case (_xs, _ys, _result) of
            (Right _, Right _, Right _) -> animateAvailablePrompt
            _ -> emptyWidget
        prompt = case focus of
            Just Arg1Field -> str "xs: " <+> xsWidget
            Just Arg2Field -> str "ys: " <+> ysWidget
            Just NavPreviewField -> vBox
                [ str "xs: " <+> xsWidget
                , str "ys: " <+> ysWidget
                , previewPrompt
                ]
            Just NavAnimateField -> vBox
                [ str "xs: " <+> xsWidget
                , str "ys: " <+> ysWidget
                , animatePrompt
                ]
            _ -> emptyWidget
    continue . (output .~ prompt) $ state

{-|
    When the user selects [Animate], render an animation using the given
    arguments if possible, then display either the result or any error
    messages.
-}
animateEvent :: State e -> EventM Name (Next (State e))
animateEvent state = do
    loadResult@LoadResult{_xs, _ys, _funcType, _result} <- loadAll state
    case (_xs, _ys, _funcType, _result) of
        (Right (_, xs'), Right (_, ys'), Right funcType', Right _) ->
            liftIO . reanimate $ appendDynamicAnimation funcType' xs' ys'
        _ -> pure ()
    previewEvent' state loadResult

{-|
    Prompt for when all arguments are valid and an animation is available.
-}
animateAvailablePrompt :: Widget Name
animateAvailablePrompt = withAttr "actionAvailable"
    $ strWrap "Select [Animate] to view the animation."

{-|
    Prompt for when each argument is valid but the result cannot be calculated
    (perhaps due to a type mismatch).
-}
animateErrorPrompt :: Widget Name
animateErrorPrompt = withAttr "error"
    $ strWrap
        "Something went wrong when preparing the animation.\n\
        \Ensure your arguments have the correct types, then try again."
