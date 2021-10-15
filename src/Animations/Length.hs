{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Length (main, lengthAnimation) where

import Control.Lens ((.~), (%~))
import Data.Foldable (traverse_)
import Data.Text (pack, Text)
import Linear (V2 (V2))

import Reanimate
import Reanimate.Builtin.Documentation (docEnv)
import Reanimate.Scene
    ( oContext
    , oDraw
    , oEasing
    , oFadeIn
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
main = reanimate lengthAnimation

env :: Animation -> Animation
env =
    docEnv
    . addStatic (mkBackground "floralwhite")
    -- . addStatic mkBackgroundGrid
    -- . addStatic mkBackgroundAxes

{-|
    Animation for the 'Data.List.length' function.
-}
lengthAnimation :: Animation
lengthAnimation = env . applyE (overEnding 1 fadeOutE) $ scene $ do
    let
        xsColor = "red"
        lengthColor = "dodgerblue"
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
            $ "length :: [a] -> Int"
        funcDefSvg =
            withDefaultBoldTextStrokeFill
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "length xs"
        xsBoxesSvgs = customListBoxes xsBoxWidths xsColor
        xsLabelsSvgs = customListLabels xsBoxWidths xsColor xsLabelTexts
        lengthBraceSvg = makeLengthBrace lengthColor 1
        lengthLabelSvg = makeLengthLabel lengthColor "1"

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

    lengthBrace <- oNew lengthBraceSvg
    oModify lengthBrace $ oTranslate .~ V2 (-5) (-1.5)

    lengthLabel <- oNew lengthLabelSvg
    oModify lengthLabel $ oTranslate .~ V2 (-5) (-2.5)

    let
        showTypeSigFuncDef = forkAllWithLag 0.25
            [ oShowWith typeSig $ setDuration 1 . oDraw
            , oShowWith funcDef $ setDuration 1 . oDraw
            ]

        showXs = forkAllWithDifferentLags $ zip
            [0, 0.25, 0]
            [ oTweenContext funcDef 1
                $ \t -> withSubglyphs [6, 7] (withTweenedColor "black" xsColor t)
            , oShowWith xsBoxes (setDuration 1 . oDraw)
            , oShowWith xsLabels (setDuration 1 . oDraw)
            ]

        splits = do
            oHide xsBoxes
            oHide xsLabels
            traverse_ oShow xsSplitBoxes
            traverse_ oShow xsSplitLabels

            traverse_
                (\x -> oModify x $ oEasing .~ softSnapOutS)
                (lengthLabel : lengthBrace : xsSplitBoxes ++ xsSplitLabels)

            let
                sep = 1
                moveXsRight = forkAll $ fmap
                    (\obj -> oTween obj 1 $ oMoveBy (sep/2, 0))
                    (xsSplitBoxes ++ xsSplitLabels)
                splitX0 = forkAll
                    $ oModify lengthBrace (oContext .~ scaleXY 0 1)
                    : oShowWith lengthBrace oFadeIn
                    : oTween lengthBrace 1 (\t -> oContext .~ scaleXY t 1)
                    : oShowWith lengthLabel oFadeIn
                    : oTween lengthLabel 1 (oMoveBy (sep/2, 0))
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        [x0Box, x0Label]
                splitX1 = forkAll
                    $ oTween lengthBrace 1 (\t -> oContext .~
                        const (makeLengthBrace lengthColor $ fromToS 1 2 t))
                    : oTween lengthLabel 1 (oMoveBy (sep/2, 0))
                    : (wait 0.375 >> oModify lengthLabel
                        (oContext .~ const (makeLengthLabel lengthColor "2")))
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        [x1Box, x1Label]
                splitX2 = forkAll
                    $ oTween lengthBrace 1 (\t -> oContext .~
                        const (makeLengthBrace lengthColor $ fromToS 2 3 t))
                    : oTween lengthLabel 1 (oMoveBy (sep/2, 0))
                    : (wait 0.375 >> oModify lengthLabel
                        (oContext .~ const (makeLengthLabel lengthColor "3")))
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        [x2Box, x2Label]
                splitDots = forkAll
                    [ oTween dotsBox 1.5 $ oMoveBy (-sep, 0)
                    , oTween lengthBrace 1.5 (\t -> oContext .~
                        const (makeLengthBrace lengthColor $ fromToS 3 6 t))
                    , oModify lengthBrace $ oEasing .~ softSnapOutS
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
                                ([3 .. 13] :: [Int])
                        forkAllWithDifferentLags . zip
                            (0.1 : [0.1, 0.09 ..])
                            $ oTween dotsLabel 1.5 (oMoveBy (-sep, 0))
                            : fmap
                                (\x -> oModify dotsLabel $ oContext .~ const x)
                                xMidLabels
                            ++ [oModify dotsLabel $ oContext .~ id]
                    , waitOn $ do
                        let
                            lengthLabels =
                                map (pack . show) ([3 .. 13] :: [Int])
                                ++ ["..."]
                                ++ map (pack . ("n-" ++) . show)
                                    ([6, 5, 4, 3] :: [Int])
                        forkAllWithDifferentLags . zip
                            (0.1 : [0.1, 0.09 .. 0] ++ [0.15] ++ [0.1, 0.15 ..])
                            $ oTween lengthLabel 1.5 (oMoveBy (sep*3/2, 0))
                            : fmap
                                (\x -> oModify lengthLabel $ oContext .~
                                    const (makeLengthLabel lengthColor x))
                                lengthLabels
                    ]
                splitXn3 = forkAll
                    $ oTween lengthBrace 1 (\t -> oContext .~
                        const (makeLengthBrace lengthColor $ fromToS 6 7 t))
                    : oTween lengthLabel 1 (oMoveBy (sep/2, 0))
                    : (wait 0.375 >> oModify lengthLabel
                        (oContext .~ const (makeLengthLabel lengthColor "n-2")))
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        [xn3Box, xn3Label]
                splitXn2 = forkAll
                    $ oTween lengthBrace 1 (\t -> oContext .~
                        const (makeLengthBrace lengthColor $ fromToS 7 8 t))
                    : oTween lengthLabel 1 (oMoveBy (sep/2, 0))
                    : (wait 0.375 >> oModify lengthLabel
                        (oContext .~ const (makeLengthLabel lengthColor "n-1")))
                    : fmap
                    (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                    [xn2Box, xn2Label]
                joinXn1 = forkAll
                    $ oTween lengthBrace 1 (\t -> oContext .~
                        const (makeLengthBrace lengthColor $ fromToS 8 9 t))
                    : oTween lengthLabel 1 (oMoveBy (sep/2, 0))
                    : (wait 0.375 >> oModify lengthLabel
                        (oContext .~ const (makeLengthLabel lengthColor "n")))
                    : fmap
                    (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                    [xn1Box, xn1Label]
                centerAll = forkAll $ fmap
                    (\obj -> oTween obj 1 $ oMoveBy (sep/2, 0))
                    (lengthBrace : lengthLabel : xsSplitBoxes ++ xsSplitLabels)

            fork moveXsRight
            splitX0
            splitX1
            splitX2
            splitDots
            wait 0.25
            splitXn3
            splitXn2
            fork joinXn1
            fork $ traverse_
                (\x -> oModify x $ oEasing .~ curveS 2)
                (lengthBrace : lengthLabel : xsSplitBoxes ++ xsSplitLabels)
            centerAll

        emphasizeResult = do
            lengthBrace' <- oNew $ makeLengthBrace lengthColor 9
            oModify lengthBrace' $ oTranslate .~ V2 (-4.5) (-1.5)

            oHide lengthBrace
            oShow lengthBrace'
            
            forkAll
                [ oTween funcDef 1 $ oMoveBy (0, -0.5)
                , oTween funcDef 1 $ \t -> oContext .~
                    withSubglyphs [0 .. 5]
                        (withTweenedColor "black" resultColor t)
                    . withSubglyphs [6, 7]
                        (withTweenedColor xsColor resultColor t)
                , forkAll $ fmap
                    (\x -> oTween x 1 $ \t -> oContext %~
                        (withTweenedColor lengthColor resultColor t .))
                    [lengthLabel]
                , forkAll $ fmap
                    (\x -> oTween x 1 $ oMoveBy (0, 2))
                    [lengthBrace', lengthLabel]
                , forkAll $ fmap
                    (`oHideWith` setDuration 0.5 . oFadeOut)
                    (lengthBrace' : xsSplitBoxes ++ xsSplitLabels)
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

makeLengthBrace :: String -> Double -> SVG
makeLengthBrace color =
    withDefaultLineStrokeFill
    . withColor color
    . \x -> mkPathString $ unwords
        [ "M 0 0"
        , "c 0.0625 -0.25 0.125 -0.25 0.25 -0.25"
        , "h " <> show ((x - 1) / 2)
        , "c 0.125 0 0.1875 0 0.25 -0.25"
        , "c 0.0625 0.25 0.125 0.25 0.25 0.25"
        , "h " <> show ((x - 1) / 2)
        , "c 0.125 0 0.1875 0 0.25 0.25"
        ]

makeLengthLabel :: String -> Text -> SVG
makeLengthLabel color =
    withColor color
    . withDefaultTextStrokeFill
    . centerX
    . latexCfgCenteredYWith firaMonoCfg (scale 0.75)
