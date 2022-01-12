{-# OPTIONS_GHC -Wall #-}

{-|
    Functions related to 'Effect's.
-}
module Utilities.Effect (animateE, mkAnimationE, composeE) where

import Reanimate

{-|
    Animate an effect applied to an SVG over a set duration.
-}
mkAnimationE :: Duration -> Effect -> SVG -> Animation
mkAnimationE d effect = applyE effect . staticFrame d

{-|
    Animate an effect applied to an SVG over a duration of 1.
-}
animateE :: Effect -> SVG -> Animation
animateE = mkAnimationE 1

{-|
    Compose multiple effect into a single effect.
-}
composeE :: [Effect] -> Effect
composeE [] _ _ = id
composeE effects d t = foldr1 (.) $ fmap (\e -> e d t) effects
