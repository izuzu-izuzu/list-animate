{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Head (main, headAnimation, headDynamicAnimation) where

import Control.Lens ((.~), (+~))
import Data.Foldable (traverse_)
import Data.Text (pack, replace)
import Linear (V2 (V2))

import Reanimate
import Reanimate.Builtin.Documentation (docEnv)
import Reanimate.Scene
    ( oDraw
    , oEasing
    , oFadeOut
    , oHide
    , oHideWith
    , oModify
    , oNew
    , oShow
    , oShowWith
    , oTween, oTranslate, Object
    )

import Utilities.List
import Utilities.Main

main :: IO ()
main = reanimate headAnimation

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
    "head :: [a] -> a"

funcDefSvgs :: [SVG]
funcDefSvgs = centerGroupX $ latexCfgChunksCenteredYWith
    firaMonoCfg
    (withDefaultBoldTextStrokeFill . withDefaultTextScale)
    ["head ", "xs"]

xsBoxesSvgs :: [SVG]
xsBoxesSvgs = list4Boxes xsColor

xsLabelsSvgs :: [SVG]
xsLabelsSvgs = list4Labels xsColor "x" "n"

{-|
    Animation for the 'Data.List.head' function.
-}
headAnimation :: Animation
headAnimation = prepareScene $ scene $ do
    typeSig <- oNew typeSigSvg
    oModify typeSig $ oTranslate .~ V2 0 2.5

    funcDef <- oNew $ mkGroup funcDefSvgs
    funcDefSplit@(~[funcDefHead, funcDefXs]) <- traverse oNew funcDefSvgs
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

        focusHead d = waitOn $ forkAll
            [ oHideWith tailBoxes $ setDuration (d/2) . oFadeOut
            , oHideWith tailLabels $ setDuration (d/2) . oFadeOut
            , oTween headLabel d $ oMoveBy (3.5, 0)
            , oTweenContext headLabel d
                $ \t -> aroundCenterX (scale (1 + 0.5*t))
            , oTween headBox d $ oMoveBy (3.5, 0)
            , oHideWith headBox $ setDuration (d*3/2) . reverseA . oDraw
            ]

        highlightResult d = waitOn . forkAll
            $ oTweenContext funcDefHead d
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
        , focusHead 1
        ]

    wait 3

dynamicBoxWidth :: Double
dynamicBoxWidth = 1.5

withDynamicBoxStrokeFill :: SVG -> SVG
withDynamicBoxStrokeFill = withStrokeWidth 0.05 . withFillOpacity 0

dynamicLabelTextScale :: Double
dynamicLabelTextScale = 0.4

makeDynamicTypeSigSvg :: String -> SVG
makeDynamicTypeSigSvg =
    centerX
    . latexCfgCenteredYWith
        firaMonoCfg
        (withDefaultBoldTextStrokeFill . withDefaultTextScale)
    . replace " " "~"
    . pack
    . ("head :: " <>)

makeDynamicXsBoxesSvgs :: [String] -> [SVG]
makeDynamicXsBoxesSvgs xs = customListBoxesWith
    (dynamicBoxWidth <$ xs)
    (withColor xsColor . withDynamicBoxStrokeFill)

makeDynamicXsLabelsSvgs :: [String] -> [SVG]
makeDynamicXsLabelsSvgs xs = customListLabelsWith
    (dynamicBoxWidth <$ xs)
    ( withColor xsColor
    . withDefaultTextStrokeFill
    . scale dynamicLabelTextScale
    . centerX
    )
    (pack <$> xs)

headDynamicAnimation :: String -> [String] -> Animation
headDynamicAnimation typeSigStr xs = prepareScene $ scene $ do
    let
        dynamicTypeSigSvg = makeDynamicTypeSigSvg typeSigStr
        dynamicXsBoxesSvgs = makeDynamicXsBoxesSvgs xs
        dynamicXsLabelsSvgs = makeDynamicXsLabelsSvgs xs
    
    typeSig <- oNewWithSvgLocation dynamicTypeSigSvg
    oModify typeSig $ oTranslate .~ V2 0 2.5

    funcDef <- oNew $ mkGroup funcDefSvgs
    funcDefSplit@(~[funcDefHead, funcDefXs]) <- traverse oNew funcDefSvgs
    traverse_
        (\obj -> oModify obj $ oTranslate +~ V2 0 1.5)
        (funcDef : funcDefSplit)

    xsBoxes <- oNew $ mkGroup dynamicXsBoxesSvgs
    xsLabels <- oNew $ mkGroup dynamicXsLabelsSvgs

    headBox <- oNewWithSvgLocation $ head dynamicXsBoxesSvgs
    headLabel <- oNewWithSvgLocation $ head dynamicXsLabelsSvgs

    tailBoxes <- oNewWithSvgLocation . mkGroup . tail $ dynamicXsBoxesSvgs
    tailLabels <- oNewWithSvgLocation . mkGroup . tail $ dynamicXsLabelsSvgs

    traverse_
        (\obj -> oModify obj $ oTranslate +~ V2 0 (-0.5))
        [xsBoxes, xsLabels, headBox, headLabel, tailBoxes, tailLabels]

    let
        hSep = dynamicBoxWidth
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
            [ oTween headBox d $ oMoveBy (-hSep/2, 0)
            , oTween headLabel d $ oMoveBy (-hSep/2, 0)
            , oTween tailBoxes d $ oMoveBy (hSep/2, 0)
            , oTween tailLabels d $ oMoveBy (hSep/2, 0)
            ]

        moveFuncDefDown d = waitOn . forkAll $ fmap
            (\obj -> oTween obj d $ oMoveBy (0, -0.5))
            funcDefSplit

        focusHead d = waitOn $ forkAll
            [ oHideWith tailBoxes $ setDuration (d/2) . oFadeOut
            , oHideWith tailLabels $ setDuration (d/2) . oFadeOut
            , oTween headLabel d $ oMoveTo (0, -0.5)
            , oTweenContext headLabel d
                $ \t -> aroundCenterX (scale (1 + 0.5*t))
            , oTween headBox d $ oMoveTo (0, -0.5)
            , oHideWith headBox $ setDuration (d*3/2) . reverseA . oDraw
            ]

        highlightResult d = waitOn $ forkAll
            $ oTweenContext funcDefHead d
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
        , focusHead 1
        ]

    wait 3

softSnapOutS :: Signal
softSnapOutS = cssCubicBezierS (0.25, 0, 0, 1)

centerOf :: SVG -> (Double, Double)
centerOf svg = (minX + w/2, minY + h/2)
    where (minX, minY, w, h) = boundingBox svg

oNewWithSvgLocation :: SVG -> Scene s (Object s SVG)
oNewWithSvgLocation svg = do
    let (locX, locY) = centerOf svg
    obj <- oNew $ center svg
    oModify obj $ oTranslate .~ V2 locX locY
    pure obj
