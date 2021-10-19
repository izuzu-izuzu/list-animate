{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Append (main, appendAnimation) where

import Control.Lens ((.~))
import Data.Foldable (traverse_)
import Linear (V2 (V2))

import Reanimate
import Reanimate.Builtin.Documentation (docEnv)
import Reanimate.Scene
    ( oDraw
    , oEasing
    , oHide
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
main = reanimate appendAnimation

env :: Animation -> Animation
env =
    docEnv
    . addStatic (mkBackground bgColor)
    . addStatic mkBackgroundGrid
    . addStatic mkBackgroundAxes

fgColor :: String
fgColor = "black"

bgColor :: String
bgColor = "floralwhite"

xsColor :: String
xsColor = "red"

ysColor :: String
ysColor = "blue"

resultColor :: String
resultColor = "magenta"

typeSigSvg :: SVG
typeSigSvg = centerX $ latexCfgCenteredYWith
    firaMonoCfg
    (withDefaultBoldTextStrokeFill . withDefaultTextScale)
    "(++) :: [a] -> [a] -> [a]"

funcDefSvgs :: [SVG]
funcDefSvgs = centerGroupX $ latexCfgChunksCenteredYWith
    firaMonoCfg
    (withDefaultBoldTextStrokeFill . withDefaultTextScale)
    ["xs ", "++ ", "ys"]

xsBoxesSvgs :: [SVG]
xsBoxesSvgs = list3Boxes xsColor

xsLabelsSvgs :: [SVG]
xsLabelsSvgs = list3Labels xsColor "x" "m"

ysBoxesSvgs :: [SVG]
ysBoxesSvgs = list4Boxes ysColor

ysLabelsSvgs :: [SVG]
ysLabelsSvgs = list4Labels ysColor "y" "n"

{-|
    Animation for the '(Data.List.++)' function.
-}
appendAnimation :: Animation
appendAnimation = env . applyE (overEnding 1 fadeOutE) $ scene $ do
    typeSig <- oNew typeSigSvg
    oModify typeSig $ oTranslate .~ V2 0 2.5
    
    funcDef <- oNew $ mkGroup funcDefSvgs
    funcDefSplit@(~[funcDefXs, funcDefAppend, funcDefYs]) <-
        traverse oNew funcDefSvgs
    traverse_
        (\obj -> oModify obj $ oTranslate .~ V2 0 1.5)
        (funcDef : funcDefSplit)

    xsBoxes <- oNew $ mkGroup xsBoxesSvgs
    xsLabels <- oNew $ mkGroup xsLabelsSvgs

    ysBoxes <- oNew $ mkGroup ysBoxesSvgs
    ysLabels <- oNew $ mkGroup ysLabelsSvgs

    let
        showTypeSigFuncDef d = waitOn $ do
            fork . oShowWith typeSig $ setDuration d . oDraw
            wait (d/4)
            oShowWith funcDef $ setDuration d . oDraw
            oHide funcDef
            traverse_ oShow funcDefSplit

        showXs d = waitOn $ forkAll
            [ oTweenContext funcDefXs d $ withTweenedColor fgColor xsColor
            , oShowWith xsBoxes $ setDuration d . oDraw
            , wait (d/4) >> oShowWith xsLabels (setDuration d . oDraw)
            ]

        moveXsBottomLeft d = waitOn . forkAll $ fmap
            (\obj -> oTween obj d $ oMoveTo (-4.5, -1.5))
            [xsBoxes, xsLabels]

        showYs d = waitOn $ forkAll
            [ oTweenContext funcDefYs d $ withTweenedColor fgColor ysColor
            , oShowWith ysBoxes $ setDuration d . oDraw
            , wait (d/4) >> oShowWith ysLabels (setDuration d . oDraw)
            ]

        moveYsBottomRight d = waitOn . forkAll $ fmap
            (\obj -> oTween obj d $ oMoveTo (3.5, -1.5))
            [ysBoxes, ysLabels]

        moveFuncDefDown d = waitOn . forkAll $ fmap
            (\obj -> oTween obj d $ oMoveBy (0, -1))
            funcDefSplit

        snapXsYs d = waitOn . forkAll
            $ fmap
                (\obj -> oTween obj d $ oMoveBy (1, 0))
                [xsBoxes, xsLabels]
            ++ fmap
                (\obj -> oTween obj d $ oMoveBy (-1, 0))
                [ysBoxes, ysLabels]

        highlightResult d = waitOn . forkAll
            $ oTweenColor fgColor resultColor funcDefAppend d
            : fmap
                (\obj -> oTweenColor xsColor resultColor obj d)
                [funcDefXs, xsBoxes, xsLabels]
            ++ fmap
                (\obj -> oTweenColor ysColor resultColor obj d)
                [funcDefYs, ysBoxes, ysLabels]
            where
                oTweenColor start end obj d' = oTweenContext obj d'
                    $ \t -> withSubglyphs [0 ..] (withTweenedColor start end t)

    wait 1
    
    showTypeSigFuncDef 1
    
    wait 0.5
    
    showXs 1
    moveXsBottomLeft 1
    showYs 1
    fork $ moveYsBottomRight 1
    moveFuncDefDown 1
    
    wait 0.5

    traverse_
        (`oModify` (oEasing .~ snapInS))
        [xsBoxes, xsLabels, ysBoxes, ysLabels]
    snapXsYs 0.5
    traverse_
        (`oModify` (oEasing .~ curveS 2))
        [xsBoxes, xsLabels, ysBoxes, ysLabels]
    highlightResult 0

    wait 3
