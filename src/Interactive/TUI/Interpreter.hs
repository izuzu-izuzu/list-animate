{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Interactive.TUI.Interpreter where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Language.Haskell.Interpreter
    ( Interpreter
    , InterpreterError (UnknownError)
    , MonadIO (..)
    , eval
    , parens
    , runInterpreter
    , setImports
    , typeOf
    )
import System.Timeout (timeout)
import Text.Printf (printf)

runLimitedInterpreter
    :: MonadIO m
    => Interpreter String
    -> m (Either InterpreterError String)
runLimitedInterpreter task = liftIO $ do
    result <- runInterpreter $ do
        setImports ["Prelude", "Text.Show.Functions"]
        task
    timeoutResult <- timeout maxTimeout $ do
        evaluate $ either (const ()) rnf result
        pure result
    pure $ case fmap (drop maxOutputLength <$>) timeoutResult of
        Nothing -> Left TimeoutError
        Just (Left err) -> Left err
        Just (Right []) -> result
        Just (Right _) -> Left OutputTooLongError

runLimitedEval :: MonadIO m => String -> m (Either InterpreterError String)
runLimitedEval = runLimitedInterpreter . eval

runLimitedEvalWithType
    :: MonadIO m
    => String
    -> m (Either InterpreterError String)
runLimitedEvalWithType arg = runLimitedInterpreter $ do
    expr <- eval arg
    ty <- typeOf arg
    pure $ printf "%v :: %v" expr ty

{-|
    A list must have 6 elements or fewer, with each element having length 6 or
    lower.
-}
splitListStr :: MonadIO m => String -> m (Either InterpreterError [String])
splitListStr =
    fmap (read @[String] <$>)
    . runLimitedInterpreter
    . eval
    . ("show <$> " <>)
    . parens

validateListStr :: MonadIO m => String -> m (Either InterpreterError String)
validateListStr listStr = do
    list <- splitListStr listStr
    pure $ case list of
        Left err -> Left err
        Right ls
            | null ls -> Left EmptyListError
            | isLongList ls -> Left ListTooLongError
            | hasLongElement ls -> Left ElementTooLongError
            | otherwise -> Right listStr
    where
        isLongList = isLongerThan maxListLength
        hasLongElement = any (isLongerThan maxElementLength)

validateSplitListStr
    :: MonadIO m
    => String
    -> m (Either InterpreterError [String])
validateSplitListStr listStr =
    either (pure . Left) splitListStr =<< validateListStr listStr

validateList :: Show a => [a] -> Bool
validateList =
    and
    . ([not . null, isShort, hasOnlyShortElements] <*>)
    . pure
    where
        isShort = isNoLongerThan maxListLength
        hasOnlyShortElements = all (isNoLongerThan maxElementLength . show)

isNoLongerThan :: Int -> [a] -> Bool
isNoLongerThan maxLength = null . drop maxLength

isLongerThan :: Int -> [a] -> Bool
isLongerThan maxLength = not . null . drop maxLength

maxListLength :: Int
maxListLength = 6

maxElementLength :: Int
maxElementLength = 6

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

maxTimeout :: Int
maxTimeout = 2 * 10^(6 :: Int)

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
