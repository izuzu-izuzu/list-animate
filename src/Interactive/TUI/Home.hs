{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interactive.TUI.Home where

import Brick
import Interactive.TUI.Core
import Interactive.TUI.Interpreter
import Brick.Forms
import Language.Haskell.Interpreter (InterpreterError (UnknownError), eval, runInterpreter, setImports, MonadIO (liftIO), runStmt, parens, MonadInterpreter)
import Data.List (intersperse)
import Control.Lens ((^.), (.~))
import Brick.Focus
import Data.Text (unpack)
import System.Timeout (timeout)
import Data.Maybe (fromMaybe)
import Graphics.Vty hiding (Input)
import Text.Printf (printf)
import Control.Monad ((<=<))
import Animations.Append (appendAnimation)
import Reanimate (reanimate)

makeForm :: Input -> Form Input e Name
makeForm =
    setFormConcat (vBox . intersperse (vLimit 1 $ fill '·'))
    . newForm
        [ radioField
            emptyInputField
            [ ((), SelectFnAppendField, "(++) :: [a] -> [a] -> [a]")
            , ((), SelectFnHeadField, "head :: [a] -> a")
            ]
        ]
    
homeEvent :: State e -> EventM Name (Next (State e))
homeEvent state = 
    continue
    . (output .~ "<prompt>")
    . (form .~ (makeForm . formState . (^. form) $ state))
    . (mode .~ Home)
    $ state
