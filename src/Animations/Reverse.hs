{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Reverse (main, reverseAnimation) where

import Control.Lens ((.~), (%~))
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
    , oTween, oZIndex, oRead
    )

import Utilities.List
import Utilities.Main

main :: IO ()
main = reanimate reverseAnimation

env :: Animation -> Animation
env =
    docEnv
    . addStatic (mkBackground "floralwhite")
    -- . addStatic mkBackgroundGrid
    -- . addStatic mkBackgroundAxes

{-|
    Animation for the 'Data.List.reverse' function.
-}
reverseAnimation :: Animation
reverseAnimation = env . applyE (overEnding 1 fadeOutE) $ scene $ do
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
            $ "reverse :: [a] -> [a]"
        funcDefSvg =
            withDefaultBoldTextStrokeFill
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "reverse xs"
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
                $ \t -> withSubglyphs [7, 8] (withTweenedColor "black" xsColor t)
            , oShowWith xsBoxes (setDuration 1 . oDraw)
            , oShowWith xsLabels (setDuration 1 . oDraw)
            ]

        splits = do
            oHide xsBoxes
            oHide xsLabels
            traverse_ oShow xsSplitBoxes
            traverse_ oShow xsSplitLabels

            let
                hSep = 2
                vSep = 2
                movePath relPath t obj = do
                    start <- oRead obj oTranslate
                    oTween obj t $ (oTranslate .~) . (start +) . relPath
                splitMove dx1 dx2 dy = movePath
                    $ cubicBezierV2 (V2 0 0) (V2 dx1 0) (V2 dx1 dy) (V2 dx2 dy)
                moveXsRight = forkAll $ fmap
                    (\obj -> oTween obj 1 $ oMoveBy (hSep/2, 0))
                    (xsSplitBoxes ++ xsSplitLabels)
                splitX0 = do
                    fork . forkAll $ fmap
                        (splitMove (-hSep/2) 8 (-vSep) 1.25)
                        [x0Box, x0Label]
                    wait 0.25
                    forkAll $ fmap
                        (\obj -> oTween obj 1.25 $ oMoveBy (-0.5, 0))
                        (drop 1 xsSplitBoxes ++ drop 1 xsSplitLabels)
                splitX1 = do
                    fork . forkAll $ fmap
                        (splitMove (-hSep/2) 6.5 (-vSep) 1.25)
                        [x1Box, x1Label]
                    wait 0.25
                    forkAll $ fmap
                        (\obj -> oTween obj 1.25 $ oMoveBy (-0.5, 0))
                        (drop 2 xsSplitBoxes ++ drop 2 xsSplitLabels)
                splitX2 = do
                    fork . forkAll $ fmap
                        (splitMove (-hSep/2) 5 (-vSep) 1.25)
                        [x2Box, x2Label]
                    wait 0.25
                    forkAll $ fmap
                        (\obj -> oTween obj 1.25 $ oMoveBy (-0.5, 0))
                        (drop 3 xsSplitBoxes ++ drop 3 xsSplitLabels)
                splitDots = waitOn $ do
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
                    fork . forkAll $ fmap
                        (splitMove (-hSep/2) 1.5 (-vSep) 1.25)
                        [dotsBox, dotsLabel]
                    fork $ do
                        wait 0.25
                        forkAll $ fmap
                            (\obj -> oTween obj 1.25 $ oMoveBy (-1.5, 0))
                            (drop 4 xsSplitBoxes ++ drop 4 xsSplitLabels)
                    do
                        wait 0.1
                        forkAllWithDifferentLags $ zip
                            [0.15, 0.14 ..]
                            ( fmap
                                (\x -> oModify dotsLabel $ oContext .~ const x)
                                xMidLabels
                            ++ [oModify dotsLabel $ oContext .~ id]
                            )
                splitXn3 = do
                    fork . forkAll $ fmap
                        (splitMove (-1 - hSep/2) (-1) (-vSep) 1.25)
                        [xn3Box, xn3Label]
                    wait 0.25
                    forkAll $ fmap
                        (\obj -> oTween obj 1.25 $ oMoveBy (-0.5, 0))
                        (drop 5 xsSplitBoxes ++ drop 5 xsSplitLabels)
                splitXn2 = do
                    fork . forkAll $ fmap
                        (splitMove (-2.5 - hSep/2) (-2.5) (-vSep) 1.25)
                        [xn2Box, xn2Label]
                    wait 0.25
                    forkAll $ fmap
                        (\obj -> oTween obj 1.25 $ oMoveBy (-0.5, 0))
                        (drop 6 xsSplitBoxes ++ drop 6 xsSplitLabels)
                splitXn1 = do
                    fork . forkAll $ fmap
                        (splitMove (-4 - hSep/2) (-4) (-vSep) 1.25)
                        [xn1Box, xn1Label]
                    wait 0.25
                    forkAll $ fmap
                        (\obj -> oTween obj 1.25 $ oMoveBy (-0.5, 0))
                        (drop 7 xsSplitBoxes ++ drop 7 xsSplitLabels)

            traverse_
                (\x -> oModify x $ oEasing .~ easeInOutS)
                (xsSplitBoxes ++ xsSplitLabels)

            splitX0
            splitX1
            splitX2
            splitDots >> wait 0.25
            splitXn3
            splitXn2
            splitXn1

            traverse_
                (\x -> oModify x $ oEasing .~ curveS 2)
                (xsSplitBoxes ++ xsSplitLabels)

        emphasizeResult = forkAll
            [ oTween funcDef 1 $ oMoveBy (0, -0.5)
            , oTween funcDef 1 $ \t -> oContext .~
                withSubglyphs [0 .. 6] (withTweenedColor "black" resultColor t)
                . withSubglyphs [7, 8]  (withTweenedColor xsColor resultColor t)
            , forkAll $ fmap
                (\x -> oTween x 1 $ \t -> oContext .~
                    withSubglyphs [0 ..] (withTweenedColor xsColor resultColor t)
                )
                (xsSplitBoxes ++ xsSplitLabels)
            , forkAll $ fmap
                (\x -> oTween x 1 $ oMoveBy (0, 2))
                (xsSplitBoxes ++ xsSplitLabels)
            ]

    wait 1

    showTypeSigFuncDef

    wait 0.5

    showXs

    wait 0.5

    waitOn splits

    wait 0.5

    emphasizeResult

    wait 3

softSnapOutS :: Signal
softSnapOutS = cssCubicBezierS (0.25, 0, 0, 1)

easeInOutS :: Signal
easeInOutS = cssCubicBezierS (0.5, 0, 1/8, 1)

cubicBezierV2
    :: V2 Double
    -> V2 Double
    -> V2 Double
    -> V2 Double
    -> Double
    -> V2 Double
cubicBezierV2 p0 p1 p2 p3 t = sum $ zipWith3
    (\coef p pow -> coef * t'^pow * (1 - t')^(3 - pow) * p)
    [1, 3, 3, 1]
    [p0, p1, p2, p3]
    [0, 1, 2, 3 :: Int]
    where t' = pure t
