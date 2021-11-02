{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Tail (main, tailAnimation) where

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
    , oTween, oTranslate
    )

import Utilities.List
import Utilities.Main
import Linear (V2(V2))

main :: IO ()
main = reanimate tailAnimation

env :: Animation -> Animation
env =
    docEnv
    . addStatic (mkBackground bgColor)
    -- . addStatic mkBackgroundGrid
    -- . addStatic mkBackgroundAxes

transitions :: Animation -> Animation
transitions = applyE (overEnding 1 fadeOutE)

prepareScene :: Animation -> Animation
prepareScene = env . transitions

fgColor :: String
fgColor = "black"

bgColor :: String
bgColor = "floralwhite"

xsColor :: String
xsColor = "red"

resultColor :: String
resultColor = "magenta"

typeSigSvg :: SVG
typeSigSvg = centerX $ latexCfgCenteredYWith
    firaMonoCfg
    (withDefaultBoldTextStrokeFill . withDefaultTextScale)
    "tail :: [a] -> [a]"

funcDefSvgs :: [SVG]
funcDefSvgs = centerGroupX $ latexCfgChunksCenteredYWith
    firaMonoCfg
    (withDefaultBoldTextStrokeFill . withDefaultTextScale)
    ["tail ", "xs"]

xsBoxesSvgs :: [SVG]
xsBoxesSvgs = list4Boxes xsColor

xsLabelsSvgs :: [SVG]
xsLabelsSvgs = list4Labels xsColor "x" "n"

{-|
    Animation for the 'Data.List.tail' function.
-}
tailAnimation :: Animation
tailAnimation = prepareScene $ scene $ do
    typeSig <- oNew typeSigSvg
    oModify typeSig $ oTranslate .~ V2 0 2.5

    funcDef <- oNew $ mkGroup funcDefSvgs
    funcDefSplit@(~[funcDefTail, funcDefXs]) <- traverse oNew funcDefSvgs
    traverse_
        (\obj -> oModify obj $ oTranslate .~ V2 0 1.5)
        (funcDef : funcDefSplit)

    xsBoxes <- oNew $ mkGroup xsBoxesSvgs
    xsLabels <- oNew $ mkGroup xsLabelsSvgs

    headBox <- oNew $ head xsBoxesSvgs
    headLabel <- oNew $ head xsLabelsSvgs

    tailBoxes <- oNew . mkGroup . tail $ xsBoxesSvgs
    tailLabels <- oNew . mkGroup . tail $ xsLabelsSvgs

    traverse_
        (\obj -> oModify obj $ oTranslate .~ V2 0 (-0.5))
        [xsBoxes, xsLabels, headBox, headLabel, tailBoxes, tailLabels]

    let
        showTypeSigFuncDef d = waitOn $ do
            forkAll
                [ oShowWith typeSig $ setDuration d . oDraw
                , wait (d/4) >> oShowWith funcDef (setDuration d . oDraw)
                ]
            oHide funcDef
            traverse_ oShow funcDefSplit

        showXs d = waitOn $ do
            waitOn $ forkAll
                [ oTweenContext funcDefXs d $ withTweenedColor fgColor xsColor
                , oShowWith xsBoxes $ setDuration d . oDraw
                , wait (d/4) >> oShowWith xsLabels (setDuration d . oDraw)
                ]
            traverse_ oHide [xsBoxes, xsLabels]
            traverse_ oShow [headBox, headLabel, tailBoxes, tailLabels]

        splitHead d = waitOn $ forkAll
            [ oTween headBox d $ oMoveBy (-0.5, 0)
            , oTween headLabel d $ oMoveBy (-0.5, 0)
            , oTween tailBoxes d $ oMoveBy (0.5, 0)
            , oTween tailLabels d $ oMoveBy (0.5, 0)
            ]

        moveFuncDefDown d = waitOn . forkAll $ fmap
            (\obj -> oTween obj d $ oMoveBy (0, -0.5))
            funcDefSplit

        focusTail d = waitOn $ forkAll
            [ oHideWith headBox $ setDuration (d/2) . oFadeOut
            , oHideWith headLabel $ setDuration (d/2) . oFadeOut
            , oTween tailLabels d $ oMoveBy (-1, 0)
            , oTween tailBoxes d $ oMoveBy (-1, 0)
            ]

        highlightResult d = waitOn $ forkAll
            $ oTweenContext funcDefTail d
                (withTweenedColor fgColor resultColor)
            : fmap
                (\obj -> oTweenContext obj d
                    $ withSubglyphs [0 ..]
                    . withTweenedColor xsColor resultColor)
                [funcDefXs, headBox, headLabel, tailBoxes, tailLabels]

    wait 1

    showTypeSigFuncDef 1

    wait 0.5

    showXs 1

    wait 1

    traverse_
        (\obj -> oModify obj $ oEasing .~ softSnapOutS)
        [headBox, headLabel, tailBoxes, tailLabels]

    splitHead 0.5

    traverse_
        (\obj -> oModify obj $ oEasing .~ curveS 2)
        [headBox, headLabel, tailBoxes, tailLabels]

    wait 0.5

    waitOn $ forkAll
        [ moveFuncDefDown 1
        , highlightResult 1
        , focusTail 1
        ]

    wait 3

softSnapOutS :: Signal
softSnapOutS = cssCubicBezierS (0.25, 0, 0, 1)
