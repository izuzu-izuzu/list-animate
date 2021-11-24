{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}

module Interactive.TUI.Interpreter where

import Control.Exception (evaluate)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import Data.Foldable (find)
import Language.Haskell.Interpreter
    ( InterpreterError (UnknownError)
    , MonadIO (liftIO)
    , eval
    , parens
    , runInterpreter
    , setImports
    , typeOf, InterpreterT
    )
import System.Timeout (timeout)
import Text.Printf (printf)

{-|
    Run an interpreter with a maximum duration and output length.
-}
runLimitedInterpreter
    :: (MonadIO m, MonadMask m)
    => InterpreterT m String
    -> ExceptT InterpreterError m String
runLimitedInterpreter task = do
    result <- ExceptT . runInterpreter $ do
        setImports
            [ "Prelude"
            , "Data.List"
            , "Data.Proxy"
            , "Text.Show.Functions"
            ]
        task
    timeoutResult <-
        maybeToExceptT TimeoutError
        . MaybeT
        . liftIO
        . timeout maxTimeout
        . evaluate
        $ result
    case drop maxOutputLength timeoutResult of
        [] -> pure result
        _ -> throwE OutputTooLongError

{-|
    'eval' a string using a limited interpreter.
-}
runLimitedEval
    :: (MonadIO m, MonadMask m)
    => String
    -> ExceptT InterpreterError m String
runLimitedEval = runLimitedInterpreter . eval

{-|
    'eval' a string using a limited interpreter, with the type signature
    included.
-}
runLimitedEvalWithType
    :: (MonadIO m, MonadMask m)
    => String
    -> ExceptT InterpreterError m String
runLimitedEvalWithType arg = runLimitedInterpreter $ do
    expr <- eval arg
    ty <- typeOf arg
    pure $ printf "%v :: %v" expr ty

{-|
    Read an expression as a list, then return a list of strings, each
    corresponding to an element. This is somewhat similar to
    @fmap show . read@.
-}
splitListStr
    :: (MonadIO m, MonadMask m)
    => String
    -> ExceptT InterpreterError m [String]
splitListStr =
    fmap read
    . runLimitedInterpreter
    . eval
    . ("show <$> " <>)
    . parens

{-|
    Verify that the given expression corresponds to a list that contains
    between 1 and 'maxListLength' elements, with each element spanning no more
    than 'maxElementLength' characters.
-}
validateListStr
    :: (MonadIO m, MonadMask m)
    => String
    -> ExceptT InterpreterError m String
validateListStr listStr = do
    list <- splitListStr listStr
    if
        | null list -> throwE EmptyListError
        | isLongList list -> throwE ListTooLongError
        | hasLongElement list -> throwE ElementTooLongError
        | otherwise -> pure listStr
    where
        isLongList = isLongerThan maxListLength
        hasLongElement = any (isLongerThan maxElementLength)

{-|
    Verify that the given expression corresponds to a list, and that the list
    satisfies the given predicates; otherwise, throw the first error
    encountered.
-}
validateListStrWith
    :: (MonadIO m, MonadMask m)
    => [([String] -> Bool, InterpreterError)]
    -> String
    -> ExceptT InterpreterError m String
validateListStrWith criteria listStr = do
    list <- splitListStr listStr
    case find (not . ($ list) . fst) criteria of
        Just (_, err) -> throwE err
        Nothing -> pure listStr

{-|
    Validate the list expression, then split it.
-}
validateSplitListStr
    :: (MonadIO m, MonadMask m)
    => String
    -> ExceptT InterpreterError m [String]
validateSplitListStr listStr =
    splitListStr =<< validateListStr listStr

isNoLongerThan :: Int -> [a] -> Bool
isNoLongerThan maxLength = null . drop maxLength

isLongerThan :: Int -> [a] -> Bool
isLongerThan maxLength = not . null . drop maxLength

{-|
    Maximum list length that can fit within the animation canvas.
-}
maxListLength :: Int
maxListLength = 6

{-|
    Maximum element length that can be legibly animated.
-}
maxElementLength :: Int
maxElementLength = 6

{-|
    Make user-friendly error messages upon encountering an 'InterpreterError'.
-}
makeErrorMessage :: InterpreterError -> String
makeErrorMessage TimeoutError = timeoutErrorMessage
makeErrorMessage OutputTooLongError = outputTooLongErrorMessage
makeErrorMessage EmptyListError = emptyListErrorMessage
makeErrorMessage ListTooLongError = listTooLongErrorMessage
makeErrorMessage ElementTooLongError = elementTooLongErrorMessage
makeErrorMessage _ = compileErrorMessage

noPreviewAvailableMessage :: String
noPreviewAvailableMessage =
    "No preview available.\n\
    \Choose an argument field first, then try again."

compileErrorMessage :: String
compileErrorMessage =
    "Invalid expression.\n\
    \Perhaps your expression contains a syntax error or ambiguous type."

timeoutErrorMessage :: String
timeoutErrorMessage =
    "Timed out.\n\
    \Perhaps you entered an infinite list or an infinitely recursive \
    \expression."

outputTooLongErrorMessage :: String
outputTooLongErrorMessage = printf
    "Output too long to be displayed.\n\
    \The maximum output length that can be displayed is %v characters."
    maxOutputLength

emptyListErrorMessage :: String
emptyListErrorMessage =
    "Empty list.\n\
    \Ensure the list contains at least one element."

listTooLongErrorMessage :: String
listTooLongErrorMessage = printf
    "List contains too many elements.\n\
    \Ensure the list contains no more than %v elements."
    maxListLength

elementTooLongErrorMessage :: String
elementTooLongErrorMessage = printf
    "Found element too long to be animated.\n\
    \Ensure all elements can be displayed in no more than %v characters."
    maxElementLength

{-|
    Maximum time that a limited interpreter (e.g. in 'runLimitedInterpreter')
    can run for.
-}
maxTimeout :: Int
maxTimeout = 2 * 10^(6 :: Int)

{-|
    Maximum output length that a limited interpreter (e.g. in
    'runLimitedInterpreter') can return.
-}
maxOutputLength :: Int
maxOutputLength = 200

pattern TimeoutError :: InterpreterError
pattern TimeoutError = UnknownError "Timeout"

pattern OutputTooLongError :: InterpreterError
pattern OutputTooLongError = UnknownError "OutputTooLong"

pattern EmptyListError :: InterpreterError
pattern EmptyListError = UnknownError "EmptyList"

pattern ListTooLongError :: InterpreterError
pattern ListTooLongError = UnknownError "ListTooLong"

pattern ElementTooLongError :: InterpreterError
pattern ElementTooLongError = UnknownError "ElementTooLong"
