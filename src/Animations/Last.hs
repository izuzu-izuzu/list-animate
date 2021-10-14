{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Last (main, lastAnimation) where

import Control.Lens ((.~))
import Data.Foldable (traverse_)
import Data.Text (pack)
import Linear (V2 (V2))

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
    , oTranslate
    , oTween
    )

import Utilities.List
import Utilities.Main

main :: IO ()
main = reanimate lastAnimation

env :: Animation -> Animation
env =
    docEnv
    . addStatic (mkBackground "floralwhite")
    -- . addStatic mkBackgroundGrid
    -- . addStatic mkBackgroundAxes

{-|
    Animation for the 'Data.List.last' function.
-}
lastAnimation :: Animation
lastAnimation = env . applyE (overEnding 1 fadeOutE) $ scene $ do
    let
        xsColor = "red"
        resultColor = "magenta"
        xsBoxWidths = [1, 1, 1, 3, 1, 1, 1]
        xsLabelTexts =
            [ "x\\textsubscript{0}"
            , "x\\textsubscript{1}"
            , "x\\textsubscript{2}"
            , "..."
            , "x\\textsubscript{n-3}"
            , "x\\textsubscript{n-2}"
            , "x\\textsubscript{n-1}"
            ]
        typeSigSvg =
            withDefaultBoldTextStrokeFill
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "last :: [a] -> a"
        funcDefSvg =
            withDefaultBoldTextStrokeFill
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "last xs"
        xsBoxesSvgs = customListBoxes xsBoxWidths xsColor
        xsLabelsSvgs = customListLabels xsBoxWidths xsColor xsLabelTexts

    typeSig <- oNew typeSigSvg
    oModify typeSig $ oTranslate .~ V2 0 2.5

    funcDef <- oNew funcDefSvg
    oModify funcDef $ oTranslate .~ V2 0 1.5

    xsBoxes <- oNew $ mkGroup xsBoxesSvgs
    oModify xsBoxes $ oTranslate .~ V2 0 (-0.5)
    
    xsLabels <- oNew $ mkGroup xsLabelsSvgs
    oModify xsLabels $ oTranslate .~ V2 0 (-0.5)

    xsSplitBoxes@(~[x0Box, x1Box, x2Box, dotsBox, xn3Box, xn2Box, xn1Box]) <-
        traverse oNew xsBoxesSvgs
    xsSplitLabels@(~
        [x0Label, x1Label, x2Label, dotsLabel, xn3Label, xn2Label, xn1Label]
        ) <- traverse oNew xsLabelsSvgs
    traverse_
        (\x -> oModify x $ oTranslate .~ V2 0 (-0.5))
        (xsSplitBoxes ++ xsSplitLabels)

    let
        showTypeSigFuncDef = forkAllWithLag 0.25
            [ oShowWith typeSig $ setDuration 1 . oDraw
            , oShowWith funcDef $ setDuration 1 . oDraw
            ]

        showXs = forkAllWithDifferentLags $ zip
            [0, 0.25, 0]
            [ oTweenContext funcDef 1
                $ \t -> withSubglyphs [4, 5] (withTweenedColor "black" xsColor t)
            , oShowWith xsBoxes (setDuration 1 . oDraw)
            , oShowWith xsLabels (setDuration 1 . oDraw)
            ]

        splits = do
            oHide xsBoxes
            oHide xsLabels
            traverse_ oShow xsSplitBoxes
            traverse_ oShow xsSplitLabels

            let
                sep = 1
                moveXsRight = forkAll $ fmap
                    (\obj -> oTween obj 1 $ oMoveBy (sep/2, 0))
                    (xsSplitBoxes ++ xsSplitLabels)
                splitX0 = forkAll $ fmap
                    (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                    [x0Box, x0Label]
                splitX1 = forkAll $ fmap
                    (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                    [x1Box, x1Label]
                splitX2 = forkAll $ fmap
                    (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                    [x2Box, x2Label]
                splitDots = forkAll
                    [ oTween dotsBox 1.5 $ oMoveBy (-sep, 0)
                    , waitOn $ do
                        let
                            xMidLabels = fmap
                                ( withColor xsColor
                                . withDefaultTextStrokeFill
                                . centerX
                                . latexCfgCenteredYWith
                                    firaMonoCfg
                                    withDefaultTextScale
                                . (\n -> pack $ "x\\textsubscript{" <> n <> "}")
                                . show
                                )
                                ([3 .. 15] :: [Int])
                        forkAllWithDifferentLags $ zip
                            (0.1 : [0.15, 0.14 ..])
                            ( [oTween dotsLabel 1.5 $ oMoveBy (-sep, 0)]
                            ++ fmap
                                (\x -> oModify dotsLabel $ oContext .~ const x)
                                xMidLabels
                            ++ [oModify dotsLabel $ oContext .~ id]
                            )
                    ]
                splitXn3 = forkAll $ fmap
                    (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                    [xn3Box, xn3Label]
                splitXn2 = forkAll $ fmap
                    (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                    [xn2Box, xn2Label]

            traverse_
                (\x -> oModify x $ oEasing .~ softSnapOutS)
                (xsSplitBoxes ++ xsSplitLabels)

            fork moveXsRight; splitX0
            splitX1
            splitX2
            splitDots >> wait 0.25
            splitXn3
            splitXn2

            traverse_
                (\x -> oModify x $ oEasing .~ curveS 2)
                (xsSplitBoxes ++ xsSplitLabels)

        emphasizeResult = forkAll
            [ oTween funcDef 1 $ oMoveBy (0, -0.5)
            , oTween funcDef 1 $ \t -> oContext .~
                withSubglyphs [0 .. 3] (withTweenedColor "black" resultColor t)
                . withSubglyphs [4, 5]  (withTweenedColor xsColor resultColor t)
            , forkAll $ fmap
                (`oHideWith` setDuration 0.5 . oFadeOut)
                (init xsSplitBoxes ++ init xsSplitLabels)
            , forkAll $ fmap
                (\x -> oTween x 1 $ oMoveBy (-4.5, 0))
                [xn1Box, xn1Label]
            , oTween xn1Label 1 $ \t -> oContext .~
                aroundCenter (scale $ 1 + 0.5 * t)
                . withTweenedColor xsColor resultColor t
            , oTween xn1Box 1 $ \t -> oContext .~
                withSubglyphs [0 ..] (withTweenedColor xsColor resultColor t)
            , oHideWith xn1Box $ setDuration 1.5 . reverseA . oDraw
            ]

    wait 1

    showTypeSigFuncDef

    wait 0.5

    showXs

    wait 0.5

    splits

    wait 0.5

    emphasizeResult

    wait 3

softSnapOutS :: Signal
softSnapOutS = cssCubicBezierS (0.25, 0, 0, 1)
