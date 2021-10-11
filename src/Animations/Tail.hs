{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Tail (tailAnimation) where

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
main = reanimate tailAnimation

env :: Animation -> Animation
env =
    docEnv
    . addStatic (mkBackground $ "floralwhite")
    . addStatic mkBackgroundGrid
    . addStatic mkBackgroundAxes
{-|
    Animation for the 'tail' function.
-}
tailAnimation :: Animation
tailAnimation = env . applyE (overEnding 1 fadeOutE) $ scene $ do
    let
        xsColor = "red"
        headColor = "magenta"

        typeSigGlyph =
            withDefaultBoldTextStrokeFill
            . translate 0 2.5
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "tail :: [a] -> [a]"

        funcDefGlyph =
            withDefaultBoldTextStrokeFill
            . translate 0 1.5
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "tail xs"

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
        , oHideWith headBox $ setDuration 0.5 . oFadeOut
        , oHideWith headLabel $ setDuration 0.5 . oFadeOut
        , oTween tailBoxes 1 $ oMoveTo ((-0.5), 0)
        , oTween tailLabels 1 $ oMoveTo ((-0.5), 0)
        , let f t' =
                  mkGroup
                  . map (withTweenedColor xsColor headColor t')
                  . removeGroups
          in oTween tailBoxes 1 $ \t -> oContext %~ (f t .)
        , let f t' =
                  withSubglyphs [0..20]
                      ( withTweenedColor xsColor headColor t'
                      -- . withStrokeWidth (0.025 + 0.025 * t')
                      )
          in oTween tailLabels 1 $ \t -> oContext %~ (f t .)
        ]

    wait 3
