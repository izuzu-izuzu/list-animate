{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Map (main, mapAnimation) where

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
main = reanimate mapAnimation

env :: Animation -> Animation
env =
    docEnv
    . addStatic (mkBackground "floralwhite")
    -- . addStatic mkBackgroundGrid
    -- . addStatic mkBackgroundAxes

{-|
    Animation for the 'Data.List.map' function.
-}
mapAnimation :: Animation
mapAnimation = env . applyE (overEnding 1 fadeOutE) $ scene $ do
    let
        xsColor = "red"
        fColor = "blue"
        fxsColor = "dodgerblue"
        resultColor = "magenta"
        xsBoxWidths = [1.5, 1.5, 1.5, 3, 1.5, 1.5, 1.5]
        xsLabelTexts =
            [ "x\\textsubscript{0}"
            , "x\\textsubscript{1}"
            , "x\\textsubscript{2}"
            , "..."
            , "x\\textsubscript{n-3}"
            , "x\\textsubscript{n-2}"
            , "x\\textsubscript{n-1}"
            ]
        fxsLabelTexts = fmap
            (\x -> if x /= "..." then "f\\;" <> x else x)
            xsLabelTexts
        typeSigSvg =
            withDefaultBoldTextStrokeFill
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "map :: (a -> b) -> [a] -> [b]"
        funcDefSvg =
            withDefaultBoldTextStrokeFill
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "map f xs"
        funcDefFSvg =
            withSubglyphs [0 ..] withDefaultTextStrokeFill
            . snd
            . splitGlyphs [3]
            $ funcDefSvg
        xsBoxesSvgs = customListBoxes xsBoxWidths xsColor
        xsLabelsSvgs = customListLabels xsBoxWidths xsColor xsLabelTexts
        fxsBoxesSvgs = customListBoxes xsBoxWidths fxsColor
        fxsLabelsSvgs = customListLabelsWith
            xsBoxWidths
            ( withColor fxsColor
            . withDefaultTextStrokeFill
            . centerX
            . scale 0.425
            )
            fxsLabelTexts

    typeSig <- oNew typeSigSvg
    oModify typeSig $ oTranslate .~ V2 0 2.5

    funcDef <- oNew funcDefSvg
    oModify funcDef $ oTranslate .~ V2 0 1.5

    funcDefF <- oNew $ centerX funcDefFSvg
    oModify funcDefF $ oTranslate .~ V2 (midX funcDefFSvg) 1.5

    xsBoxes <- oNew $ mkGroup xsBoxesSvgs
    xsLabels <- oNew $ mkGroup xsLabelsSvgs
    xsSplitBoxes@(~
        [ x0Box
        , x1Box
        , x2Box
        , xsDotsBox
        , xn3Box
        , xn2Box
        , xn1Box
        ]) <- traverse oNew xsBoxesSvgs
    xsSplitLabels@(~
        [ x0Label
        , x1Label
        , x2Label
        , xsDotsLabel
        , xn3Label
        , xn2Label
        , xn1Label
        ]) <- traverse oNew xsLabelsSvgs
    traverse_
        (\x -> oModify x $ oTranslate .~ V2 0 (-0.5))
        (xsBoxes : xsLabels : xsSplitBoxes ++ xsSplitLabels)

    fxsBoxes <- oNew $ mkGroup fxsBoxesSvgs
    fxsLabels <- oNew $ mkGroup fxsLabelsSvgs
    fxsSplitBoxes@(~
        [ fx0Box
        , fx1Box
        , fx2Box
        , fxsDotsBox
        , fxn3Box
        , fxn2Box
        , fxn1Box
        ]) <- traverse oNew fxsBoxesSvgs
    fxsSplitLabels@(~
        [ fx0Label
        , fx1Label
        , fx2Label
        , fxsDotsLabel
        , fxn3Label
        , fxn2Label
        , fxn1Label
        ]) <- traverse oNew fxsLabelsSvgs
    traverse_
        (\x -> oModify x $ oTranslate .~ V2 0 (-0.5))
        (fxsBoxes : fxsLabels : fxsSplitBoxes ++ fxsSplitLabels)

    let
        showTypeSigFuncDef = forkAllWithLag 0.25
            [ oShowWith typeSig $ setDuration 1 . oDraw
            , oShowWith funcDef $ setDuration 1 . oDraw
            ]

        showSpriteF dur loc = do
            fSprite <- fork
                . newSpriteSVG
                . uncurry translate loc
                . scale 1.25
                . centerX
                . withColor fColor
                $ funcDefFSvg
            fork $ spriteTween fSprite dur $ translate 0 . ((-3) *) . easeInOutS
            spriteTween fSprite dur $ fadeOutEarlyE easeInOutS 1

        showF = forkAll
            [ oTweenContext funcDef 0.5 $ \t ->
                withSubglyphs [3] (withTweenedColor "black" fColor t)
            , oShow funcDefF
            , oTweenContext funcDefF 0.5 $ \t ->
                withTweenedColor "black" fColor t
            , oTweenContext funcDefF 1 $ \t -> scale (1 + 0.25*t)
            , oTween funcDefF 1 $ oMoveBy (0, -1)
            , wait 0.25 >> oTween funcDefF 1 (oMoveTo (-5.25, 0.5))
            ]

        showXs = forkAll
            [ oTweenContext funcDef 1 $ \t ->
                withSubglyphs [4, 5] (withTweenedColor "black" xsColor t)
            , oShowWith xsBoxes (setDuration 1 . oDraw)
            , wait 0.25 >> oShowWith xsLabels (setDuration 1 . oDraw)
            ]

        showSplits = do
            oHide xsBoxes
            oHide xsLabels
            traverse_ oShow xsSplitBoxes
            traverse_ oShow xsSplitLabels

            let
                hSep = 1.5
                vSep = 2
                moveDownFadeOut obj t = waitOn $ forkAll
                    [ oTween obj t $ oMoveBy (0, -vSep)
                    , oHideWith obj $ setDuration t . oFadeOutEarly
                    ]
                moveDownFadeIn obj t = waitOn $ forkAll
                    [ oShowWith obj $ setDuration t . oFadeInLate
                    , oTween obj t $ oMoveBy (0, -vSep)
                    ]
                splitX0 = waitOn . forkAll
                    $ showSpriteF 1 (-5.25, 0.5)
                    : (wait 0.25 >> oTween funcDefF 1 (oMoveBy (hSep, 0)))
                    : fmap (`moveDownFadeOut` 1) [x0Box, x0Label]
                    ++ fmap (`moveDownFadeIn` 1) [fx0Box, fx0Label]
                splitX1 = waitOn . forkAll
                    $ showSpriteF 1 (-5.25 + hSep, 0.5)
                    : (wait 0.25 >> oTween funcDefF 1 (oMoveBy (hSep, 0)))
                    : fmap (`moveDownFadeOut` 1) [x1Box, x1Label]
                    ++ fmap (`moveDownFadeIn` 1) [fx1Box, fx1Label]
                splitX2 = waitOn . forkAll
                    $ showSpriteF 1 (-5.25 + 2*hSep, 0.5)
                    : (wait 0.25 >> oTween funcDefF 1 (oMoveBy (1.5*hSep, 0)))
                    : fmap (`moveDownFadeOut` 1) [x2Box, x2Label]
                    ++ fmap (`moveDownFadeIn` 1) [fx2Box, fx2Label]
                splitDots = let t = 1.5 in waitOn $ forkAll
                    [ showSpriteF t (-5.25 + 3.5*hSep, 0.5)
                    , wait 0.25 >> oTween funcDefF t (oMoveBy (1.5*hSep, 0))
                    , oTween xsDotsBox t $ oMoveBy (0, -vSep)
                    , oHideWith xsDotsBox $ setDuration t . oFadeOutEarly
                    , oTween fxsDotsBox t $ oMoveBy (0, -vSep)
                    , oShowWith fxsDotsBox $ setDuration t . oFadeInLate
                    , waitOn $ do
                        let
                            xsMidLabelsSvgs = fmap
                                ( withColor xsColor
                                . withDefaultTextStrokeFill
                                . centerX
                                . latexCfgCenteredYWith
                                    firaMonoCfg
                                    (scale 0.425)
                                . pack
                                . (\n -> "x\\textsubscript{" <> n <> "}")
                                . show
                                )
                                ([3 .. 18] :: [Int])
                            showXsMidLabelsSprite loc = do
                                v <- newVar $ head xsMidLabelsSvgs
                                s <- fork . newSprite
                                    $ uncurry translate loc <$> unVar v
                                spriteE s . overBeginning (t/5)
                                    $ fadeInLateE easeInOutS
                                spriteE s . overEnding (t*4/5)
                                    $ fadeOutEarlyE easeInOutS
                                fork . spriteTween s t
                                    $ translate 0 . ((-vSep) *) . easeInOutS
                                wait (t/10)
                                traverse_
                                    (\(svg, d) -> writeVar v svg >> wait d)
                                    $ zip
                                        xsMidLabelsSvgs
                                        ([0.15, 0.14 .. 0.05] ++ repeat 0.05)
                                destroySprite s
                        forkAll
                            [ oTween xsDotsLabel t (oMoveBy (0, -vSep))
                            , oHideWith xsDotsLabel
                                $ setDuration (t/5) . oFadeOutEarly
                            , showXsMidLabelsSprite (0, -0.5)
                            ]
                    , waitOn $ do
                        let
                            fxsMidLabelsSvgs = fmap
                                ( withColor fxsColor
                                . withDefaultTextStrokeFill
                                . centerX
                                . latexCfgCenteredYWith
                                    firaMonoCfg
                                    (scale 0.425)
                                . pack
                                . (\n -> "f\\;x\\textsubscript{" <> n <> "}")
                                . show
                                )
                                ([3 .. 18] :: [Int])
                            showFxsMidLabelsSprite loc = do
                                v <- newVar $ head fxsMidLabelsSvgs
                                s <- fork . newSprite
                                    $ uncurry translate loc <$> unVar v
                                spriteE s . overBeginning (t*4/5)
                                    $ fadeInLateE easeInOutS
                                spriteE s $ overEnding (t/5)
                                    $ fadeOutEarlyE easeInOutS
                                fork . spriteTween s t
                                    $ translate 0 . ((-vSep) *) . easeInOutS
                                wait (t/10)
                                traverse_
                                    (\(svg, d) -> writeVar v svg >> wait d)
                                    $ zip
                                        fxsMidLabelsSvgs
                                        ([0.15, 0.14 .. 0.05] ++ repeat 0.05)
                                destroySprite s

                        forkAll
                            [ oTween fxsDotsLabel t (oMoveBy (0, -vSep))
                            , showFxsMidLabelsSprite (0, -0.5)
                            , wait (t*4/5) >> oShowWith fxsDotsLabel
                                (setDuration (t/5) . oFadeInLate)
                            ]
                    ]
                splitXn3 = waitOn . forkAll
                    $ showSpriteF 1 (-5.25 + 5*hSep, 0.5)
                    : (wait 0.25 >> oTween funcDefF 1 (oMoveBy (hSep, 0)))
                    : fmap (`moveDownFadeOut` 1) [xn3Box, xn3Label]
                    ++ fmap (`moveDownFadeIn` 1) [fxn3Box, fxn3Label]
                splitXn2 = waitOn . forkAll
                    $ showSpriteF 1 (-5.25 + 6*hSep, 0.5)
                    : (wait 0.25 >> oTween funcDefF 1 (oMoveBy (hSep, 0)))
                    : fmap (`moveDownFadeOut` 1) [xn2Box, xn2Label]
                    ++ fmap (`moveDownFadeIn` 1) [fxn2Box, fxn2Label]
                splitXn1 = waitOn . forkAll
                    $ showSpriteF 1 (-5.25 + 7*hSep, 0.5)
                    : oHide funcDefF
                    : fmap (`moveDownFadeOut` 1) [xn1Box, xn1Label]
                    ++ fmap (`moveDownFadeIn` 1) [fxn1Box, fxn1Label]

            splitX0
            splitX1
            splitX2
            splitDots
            splitXn3
            splitXn2
            splitXn1

        emphasizeResult = forkAll
            [ oTween funcDef 1 $ oMoveBy (0, -0.5)
            , oTween funcDef 1 $ \t -> oContext .~
                withSubglyphs [0, 1, 2] (withTweenedColor "black" resultColor t)
                . withSubglyphs [3] (withTweenedColor fColor resultColor t)
                . withSubglyphs [4, 5]  (withTweenedColor xsColor resultColor t)
            , forkAll $ fmap
                (\x -> oTween x 1 $ \t -> oContext .~
                    withSubglyphs [0 ..]
                        (withTweenedColor fxsColor resultColor t)
                )
                (fxsSplitBoxes ++ fxsSplitLabels)
            , forkAll $ fmap
                (\x -> oTween x 1 $ oMoveBy (0, 2))
                (fxsSplitBoxes ++ fxsSplitLabels)
            ]

    wait 1

    showTypeSigFuncDef

    wait 0.5

    showXs

    wait 0.5

    traverse_
        (\x -> oModify x $ oEasing .~ easeInOutS)
        (funcDefF :
             xsSplitBoxes ++ xsSplitLabels ++ fxsSplitBoxes ++ fxsSplitLabels)

    showF

    wait 0.5

    showSplits

    traverse_
        (\x -> oModify x $ oEasing .~ curveS 2)
        (funcDefF :
             xsSplitBoxes ++ xsSplitLabels ++ fxsSplitBoxes ++ fxsSplitLabels)

    wait 0.5

    emphasizeResult

    wait 3

easeInOutS :: Signal
easeInOutS = cssCubicBezierS (0.5, 0, 0, 1)

fadeInLateE :: Signal -> Effect
fadeInLateE s d t = fadeInE d . (d *) . s $ clamp ((t/d - 1/4) / (3/4)) 0 1

fadeOutEarlyE :: Signal -> Effect
fadeOutEarlyE s d t = fadeOutE d . (d *) . s $ clamp ((t/d) / (3/4)) 0 1

oFadeInLate :: SVG -> Animation
oFadeInLate svg = animate
    $ \t -> let t' = clamp ((t - 1/4) / (3/4)) 0 1 in withGroupOpacity t' svg

oFadeOutEarly :: SVG -> Animation
oFadeOutEarly = reverseA . oFadeInLate

clamp :: (Num a, Ord a) => a -> a -> a -> a
clamp x l h
    | x < l = l
    | x > h = h
    | otherwise = x

midX :: SVG -> Double
midX svg = minX + w/2
    where (minX, _, w, _) = boundingBox svg
