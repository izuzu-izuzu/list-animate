{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Concat (main, concatAnimation) where

import Control.Lens ((.~))
import Data.Foldable (traverse_)

import Reanimate
import Reanimate.Builtin.Documentation (docEnv)
import Reanimate.Scene
    ( oContext
    , oDraw
    , oEasing
    , oModify
    , oNew
    , oShowWith
    , oTween
    )

import Utilities.List
import Utilities.Main

main :: IO ()
main = reanimate concatAnimation

env :: Animation -> Animation
env =
    docEnv
    . addStatic (mkBackground "floralwhite")
    -- . addStatic mkBackgroundGrid
    -- . addStatic mkBackgroundAxes

{-|
    Animation for the '(Data.List.++)' function.
-}
concatAnimation :: Animation
concatAnimation = env . applyE (overEnding 1 fadeOutE) $ scene $ do
    let
        xsColor = "red"
        ysColor = "blue"
        combinedColor = "magenta"
        typeSigGlyph =
            withDefaultBoldTextStrokeFill
            . translate 0 2.5
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "(++) :: [a] -> [a] -> [a]"
        funcDefGlyph =
            withDefaultBoldTextStrokeFill
            . translate 0 1.5
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "xs ++ ys"

    typeSig <- oNew typeSigGlyph
    funcDef <- oNew funcDefGlyph

    xsBoxes <- oNew . mkGroup $ list3Boxes xsColor
    xsLabels <- oNew . mkGroup $ list3Labels xsColor "x" "m"

    ysBoxes <- oNew . mkGroup $ list4Boxes ysColor
    ysLabels <- oNew . mkGroup $ list4Labels ysColor "y" "n"

    let
        showTypeSigFuncDef = forkAllWithLag 0.25
            [ oShowWith typeSig $ setDuration 1 . oDraw
            , oShowWith funcDef $ setDuration 1 . oDraw
            ]

        showXs = forkAllWithDifferentLags $ zip
            [0, 0.25, 0]
            [ oTweenContext funcDef 1 $ \t ->
                  withSubglyphs [0, 1] (withTweenedColor "black" xsColor t)
            , oShowWith xsBoxes $ setDuration 1 . oDraw
            , oShowWith xsLabels $ setDuration 1 . oDraw
            ]

        moveXsBottomLeft = forkAll
            [ oTween xsBoxes 1 $ oMoveTo (-4.5, -1.5)
            , oTween xsLabels 1 $ oMoveTo (-4.5, -1.5)
            ]

        showYs = forkAllWithDifferentLags $ zip
            [0, 0.25, 0]
            [ oTweenContext funcDef 1 $ \t ->
                  withSubglyphs [4, 5] (withTweenedColor "black" ysColor t)
            , oShowWith ysBoxes $ setDuration 1 . oDraw
            , oShowWith ysLabels $ setDuration 1 . oDraw
            ]

        moveYsBottomRight = forkAll
            [ oTween ysBoxes 1 $ oMoveTo (3.5, -1.5)
            , oTween ysLabels 1 $ oMoveTo (3.5, -1.5)
            ]

        moveFuncDefDown = oTween funcDef 1 $ oMoveTo (0, -1)

        snapXsYs = do
            traverse_
                (\x ->
                    oModify x $ oEasing .~ powerS 5)
                [xsBoxes, xsLabels, ysBoxes, ysLabels]
            forkAll
                [ oTween xsBoxes 0.5 $ oMoveTo (-3.5, -1.5)
                , oTween xsLabels 0.5 $ oMoveTo (-3.5, -1.5)
                , oTween ysBoxes 0.5 $ oMoveTo (2.5, -1.5)
                , oTween ysLabels 0.5 $ oMoveTo (2.5, -1.5)
                ]
            traverse_
                (\x -> oModify x $ oEasing .~ curveS 2)
                [xsBoxes, xsLabels, ysBoxes, ysLabels]

        highlightCombinedXsYs = traverse_
            (\x -> oModify x $ oContext .~
                withSubglyphs [0 ..] (withColor combinedColor))
            [funcDef, xsBoxes, xsLabels, ysBoxes, ysLabels]

    wait 1

    showTypeSigFuncDef

    wait 0.5

    showXs

    moveXsBottomLeft

    showYs

    fork moveYsBottomRight
    moveFuncDefDown

    wait 0.5

    snapXsYs

    highlightCombinedXsYs

    wait 3
