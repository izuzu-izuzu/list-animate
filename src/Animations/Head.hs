{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Head (headAnimation) where

import Control.Lens ((%~), (.~))
import Data.Foldable (traverse_)

import Reanimate
import Reanimate.Builtin.Documentation (docEnv)
import Reanimate.Scene
    ( oContext
    , oDraw
    , oEasing
    , oFadeOut
    , oHide
    , oHideWith
    , oModify
    , oNew
    , oShow
    , oShowWith
    , oTween
    )

import Utilities.Main
import Utilities.List

main :: IO ()
main = reanimate headAnimation

env :: Animation -> Animation
env =
    docEnv
    . addStatic (mkBackground $ "floralwhite")
    . addStatic mkBackgroundGrid
    . addStatic mkBackgroundAxes

{-|
    Animation for the 'head' function.
-}
headAnimation :: Animation
headAnimation = env . applyE (overEnding 1 fadeOutE) $ scene $ do
    let
        xsColor = "red"
        headColor = "magenta"

        typeSigGlyph =
            withDefaultBoldTextStrokeFill
            . translate 0 2.5
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "head :: [a] -> a"

        funcDefGlyph =
            withDefaultBoldTextStrokeFill
            . translate 0 1.5
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "head xs"

        xsBoxesGlyph =
            translate 0 (-0.5) . mkGroup $ list4Boxes xsColor
        (tailBoxesGlyph, headBoxGlyph) = splitGlyphs [0] xsBoxesGlyph

        xsLabelsGlyph =
            translate 0 (-0.5) . mkGroup $ list4Labels xsColor "x" "m"
        (tailLabelsGlyph, headLabelGlyph) =
            splitGlyphs [0, 1] xsLabelsGlyph

    typeSig <- oNew typeSigGlyph

    funcDef <- oNew funcDefGlyph

    xsBoxes <- oNew xsBoxesGlyph
    xsLabels <- oNew xsLabelsGlyph

    headBox <- oNew headBoxGlyph
    headLabel <- oNew headLabelGlyph

    tailBoxes <- oNew tailBoxesGlyph
    tailLabels <- oNew tailLabelsGlyph

    wait 1

    forkAllWithLag 0.25
        [ oShowWith typeSig $ setDuration 1 . oDraw
        , oShowWith funcDef $ setDuration 1 . oDraw
        ]

    wait 0.5

    forkAll
        [ let f t' =
                  withSubglyphs [4, 5]
                      (withTweenedColor "black" xsColor t')
          in oTween funcDef 1 $ \t -> oContext %~ (f t .)
        , oShowWith xsBoxes $ setDuration 1 . oDraw
        , oShowWith xsLabels $ setDuration 1 . oDraw
        ]

    traverse_ oHide [xsBoxes, xsLabels]
    traverse_ oShow [headBox, headLabel, tailBoxes, tailLabels]

    wait 1

    forkAll
        [ traverse_
              (\x ->
                  oModify x $ oEasing .~ cubicBezierS (0, 0.985, 0.995, 1))
              [headBox, headLabel, tailBoxes, tailLabels]
        , oTween headBox 0.5 $ oMoveTo ((-0.5), 0)
        , oTween headLabel 0.5 $ oMoveTo ((-0.5), 0)
        , oTween tailBoxes 0.5 $ oMoveTo (0.5, 0)
        , oTween tailLabels 0.5 $ oMoveTo (0.5, 0)
        , traverse_
            (\x -> oModify x $ oEasing .~ curveS 2)
            [headBox, headLabel, tailBoxes, tailLabels]
        ]

    wait 1

    forkAll
        [ let f t' =
                  withSubglyphs [4, 5]
                      (withTweenedColor xsColor headColor t')
                  . withSubglyphs [0, 1, 2, 3]
                      (withTweenedColor "black" headColor t')
          in oTween funcDef 1 $ \t -> oContext %~ (f t .)
        , oTween funcDef 1 $ oMoveTo (0, (-0.5))
        , oHideWith tailBoxes $ setDuration 0.5 . oFadeOut
        , oHideWith tailLabels $ setDuration 0.5 . oFadeOut
        , oTween headLabel 1 $ oMoveTo (3, 0)
        , let f t' =
                  mkGroup
                  . map (withTweenedColor xsColor headColor t')
                  . removeGroups
          in oTween headBox 1 $ \t -> oContext %~ (f t .)
        , oTween headBox 1 $ oMoveTo (3, 0)
        , oHideWith headBox $ setDuration 1.5 . reverseA . oDraw
        , let f t' =
                  aroundCenter (scale (1 + 0.5 * t'))
                  . withSubglyphs [0..10]
                      (withTweenedColor xsColor headColor t')
          in oTween headLabel 1 $ \t -> oContext %~ (f t .)
        ]

    wait 3
