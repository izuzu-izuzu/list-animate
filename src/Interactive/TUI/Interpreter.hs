{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Interactive.TUI.Interpreter where

import Control.DeepSeq (force)
import Control.Exception (evaluate, ErrorCall, try)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Data.Char (toLower)
import Data.Foldable (find)
import Data.List (isInfixOf)
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
import Text.RawString.QQ (r)

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
    timeoutErrorCallTest <-
        liftIO
        . timeout maxTimeout
        . try @ErrorCall
        . evaluate
        . force
        $ result
    case timeoutErrorCallTest of
        Nothing -> throwE TimeoutError
        Just (Left _) -> throwE FoundErrorCallError
        Just (Right result') -> case drop maxOutputLength result' of
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
makeErrorMessage FoundErrorCallError = foundErrorCallErrorMessage
makeErrorMessage OutputTooLongError = outputTooLongErrorMessage
makeErrorMessage EmptyListError = emptyListErrorMessage
makeErrorMessage ListTooLongError = listTooLongErrorMessage
makeErrorMessage ElementTooLongError = elementTooLongErrorMessage
makeErrorMessage (fmap toLower . show -> err)
    | "ambiguous type" `isInfixOf` err = ambiguousTypeErrorMessage
    {-
    | "couldn't match expected type" `isInfixOf` err =
        mismatchedTypesErrorMessage
    -}
    | otherwise = genericErrorMessage

noPreviewAvailableMessage :: String
noPreviewAvailableMessage =
    [r|No preview available.
Choose an argument field first, then try again.|]

genericErrorMessage :: String
genericErrorMessage =
    [r|Invalid expression.
Perhaps your input contains invalid syntax or incorrect types.|]

ambiguousTypeErrorMessage :: String
ambiguousTypeErrorMessage =
    [r|Found ambiguous type.
Add type annotations to your input (e.g. "[] :: [Int]" instead of "[]").|]

mismatchedTypesErrorMessage :: String
mismatchedTypesErrorMessage =
    [r|Found mismatched types.
Ensure your input has the correct types (e.g. not using "head" on an Int).|]

timeoutErrorMessage :: String
timeoutErrorMessage =
    [r|Timed out.
Perhaps you entered an infinite list or an infinitely recursive value.|]

foundErrorCallErrorMessage :: String
foundErrorCallErrorMessage =
    [r|Error detected.
Perhaps you used `undefined` or an expression that results in an error.|]

outputTooLongErrorMessage :: String
outputTooLongErrorMessage = printf
    [r|Output too long to be displayed.
The maximum output length that can be displayed is %v characters.|]
    maxOutputLength

emptyListErrorMessage :: String
emptyListErrorMessage =
    [r|Empty list.
Ensure the list contains at least one element.|]

listTooLongErrorMessage :: String
listTooLongErrorMessage = printf
    [r|List contains too many elements.
Ensure the list contains no more than %v elements.|]
    maxListLength

elementTooLongErrorMessage :: String
elementTooLongErrorMessage = printf
    [r|Found element too long to be animated.
Ensure all elements can be displayed in no more than %v characters.|]
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

pattern FoundErrorCallError :: InterpreterError
pattern FoundErrorCallError = UnknownError "FoundErrorCall"

pattern OutputTooLongError :: InterpreterError
pattern OutputTooLongError = UnknownError "OutputTooLong"

pattern EmptyListError :: InterpreterError
pattern EmptyListError = UnknownError "EmptyList"

pattern ListTooLongError :: InterpreterError
pattern ListTooLongError = UnknownError "ListTooLong"

pattern ElementTooLongError :: InterpreterError
pattern ElementTooLongError = UnknownError "ElementTooLong"
