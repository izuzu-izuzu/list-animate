{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Intersperse (main, intersperseAnimation) where

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
    , oTween, oFadeIn
    )

import Utilities.List
import Utilities.Main
import Data.List (intersperse)

main :: IO ()
main = reanimate intersperseAnimation

env :: Animation -> Animation
env = docEnv
    . addStatic (mkBackground "floralwhite")
    -- . addStatic mkBackgroundGrid
    -- . addStatic mkBackgroundAxes

{-|
    Animation for the 'Data.List.intersperse' function.
-}
intersperseAnimation :: Animation
intersperseAnimation = env . applyE (overEnding 1 fadeOutE) $ scene $ do
    let
        xsColor = "red"
        yColor = "blue"
        yBoxesColor = xsColor
        resultColor = "magenta"
        xsBoxWidths = [1, 1, 1, 3, 1, 1, 1]
        yBoxWidths = [1, 1, 1, 2, 1, 1, 1]
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
            $ "intersperse :: a -> [a] -> [a]"
        funcDefSvg =
            withDefaultBoldTextStrokeFill
            . centerX
            . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
            $ "intersperse y xs"
        xsBoxesSvgs = customListBoxes xsBoxWidths xsColor
        xsLabelsSvgs = customListLabels xsBoxWidths xsColor xsLabelTexts
        yBoxesSvgs = (++) <$> take 3 <*> drop 4
            $ customListBoxes yBoxWidths yBoxesColor
        yLabelsSvgs = (++) <$> take 3 <*> drop 4
            $ customListLabels yBoxWidths yColor (replicate 7 "y")

    typeSig <- oNew typeSigSvg
    oModify typeSig $ oTranslate .~ V2 0 2.5

    funcDef <- oNew funcDefSvg
    oModify funcDef $ oTranslate .~ V2 0 1.5

    xsBoxes <- oNew $ mkGroup xsBoxesSvgs
    oModify xsBoxes $ oTranslate .~ V2 0 (-0.5)

    xsLabels <- oNew $ mkGroup xsLabelsSvgs
    oModify xsLabels $ oTranslate .~ V2 0 (-0.5)

    xsSplitBoxes@(~[x0B, x1B, x2B, dotsB, xn3B, xn2B, xn1B]) <-
        traverse oNew xsBoxesSvgs
    xsSplitLabels@(~[x0L, x1L, x2L, dotsL, xn3L, xn2L, xn1L]) <-
        traverse oNew xsLabelsSvgs
    traverse_
        (\x -> oModify x $ oTranslate .~ V2 0 (-0.5))
        (xsSplitBoxes ++ xsSplitLabels)

    ySplitBoxes@(~[y0B, y1B, y2B, yn4B, yn3B, yn2B]) <-
        traverse oNew yBoxesSvgs
    ySplitLabels@(~[y0L, y1L, y2L, yn4L, yn3L, yn2L]) <-
        traverse oNew yLabelsSvgs
    traverse_
        (\x -> oModify x $ oTranslate .~ V2 0 (-0.5))
        (ySplitBoxes ++ ySplitLabels)

    let
        showTypeSigFuncDef = forkAllWithLag 0.25
            [ oShowWith typeSig $ setDuration 1 . oDraw
            , oShowWith funcDef $ setDuration 1 . oDraw
            ]

        showXs = forkAllWithDifferentLags $ zip
            [0, 0.25, 0, 0]
            [ oTweenContext funcDef 1 $ \t ->
                withSubglyphs [12, 13] (withTweenedColor "black" xsColor t)
            , oShowWith xsBoxes (setDuration 1 . oDraw)
            , oShowWith xsLabels (setDuration 1 . oDraw)
            ]

        showY = do
            oModify funcDef $ oEasing .~ softSnapOutS
            oTweenContext funcDef 1 $ \t ->
                withSubglyphs [11] (withTweenedColor "black" yColor t)
            oModify funcDef $ oEasing .~ curveS 2

        showSplits = do
            let
                sep = 1
                signal = softSnapOutS
                spawnYBox sig dur loc = do
                    let
                        makeBox w =
                            uncurry translate loc
                            . head
                            $ customListBoxes [w] yBoxesColor
                    v <- newVar 0
                    s <- fork . newSprite $ makeBox <$> unVar v
                    tweenVar v dur $ \_ t -> sep * sig t
                    destroySprite s

                moveAllRight dur = waitOn . forkAll $ fmap
                    (\obj -> oTween obj dur $ oMoveBy (sep/2, 0))
                    ( xsSplitBoxes
                    ++ xsSplitLabels
                    ++ ySplitBoxes
                    ++ ySplitLabels
                    )
                splitX0 = waitOn . forkAll
                    $ moveAllRight 1
                    : (spawnYBox signal 1 (-3.5, -0.5)
                        >> oShow y0B)
                    : oShowWith y0L (setDuration 1 . oFadeIn)
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        ( take 1 xsSplitBoxes
                        ++ take 1 xsSplitLabels
                        ++ take 0 ySplitBoxes
                        ++ take 0 ySplitLabels
                        )
                splitX1 = waitOn . forkAll
                    $ moveAllRight 1
                    : (spawnYBox signal 1 (-2, -0.5)
                        >> oShow y1B)
                    : oShowWith y1L (setDuration 1 . oFadeIn)
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        ( take 2 xsSplitBoxes
                        ++ take 2 xsSplitLabels
                        ++ take 1 ySplitBoxes
                        ++ take 1 ySplitLabels
                        )
                splitX2 = waitOn . forkAll
                    $ moveAllRight 1
                    : (spawnYBox signal 1 (-0.5, -0.5)
                        >> oShow y2B)
                    : oShowWith y2L (setDuration 1 . oFadeIn)
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        ( take 3 xsSplitBoxes
                        ++ take 3 xsSplitLabels
                        ++ take 2 ySplitBoxes
                        ++ take 2 ySplitLabels
                        )
                splitDots = let t = 1.5 in waitOn $ do
                    let
                        xsMidLabelsSvgs = take 19 $ intersperse
                            ( withColor yColor
                            . withDefaultTextStrokeFill
                            . centerX
                            . latexCfgCenteredYWith
                                firaMonoCfg
                                withDefaultTextScale
                            $ "y"
                            )
                            $ fmap
                                ( withColor xsColor
                                . withDefaultTextStrokeFill
                                . centerX
                                . latexCfgCenteredYWith
                                    firaMonoCfg
                                    withDefaultTextScale
                                . (\n -> pack $ "x\\textsubscript{" <> n <> "}")
                                . show
                                )
                                ([3 .. 14] :: [Int])
                        showXsMidLabelsSprite sig loc = do
                                v <- newVar $ head xsMidLabelsSvgs
                                s <- fork . newSprite
                                    $ uncurry translate loc <$> unVar v
                                spriteE s . overBeginning (t/5)
                                    $ \d' t' -> fadeInE d' $ d' * sig (t'/d')
                                spriteE s $ overEnding (t/5)
                                    $ \d' t' -> fadeOutE d' $ d' * sig (t'/d')
                                traverse_
                                    (\(svg, d) -> writeVar v svg >> wait d)
                                    $ zip
                                        xsMidLabelsSvgs
                                        ([0.15, 0.14 .. 0.05] ++ repeat 0.05)
                                destroySprite s
                    forkAll
                        [ oHideWith dotsL (setDuration (t/5) . oFadeOut)
                            >> oModify dotsL (oContext .~
                                withSubglyphs [1] (withColor yColor))
                            >> wait (t*3/5)
                            >> oShowWith dotsL (setDuration (t/5) . oFadeIn)
                        , showXsMidLabelsSprite signal (1.5, -0.5)
                        ]
                splitXn4 = waitOn . forkAll
                    $ moveAllRight 1
                    : (spawnYBox signal 1 (3, -0.5)
                        >> oShow yn4B)
                    : oShowWith yn4L (setDuration 1 . oFadeIn)
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        ( take 4 xsSplitBoxes
                        ++ take 4 xsSplitLabels
                        ++ take 3 ySplitBoxes
                        ++ take 3 ySplitLabels
                        )
                splitXn3 = waitOn . forkAll
                    $ moveAllRight 1
                    : (spawnYBox signal 1 (4.5, -0.5)
                        >> oShow yn3B)
                    : oShowWith yn3L (setDuration 1 . oFadeIn)
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        ( take 5 xsSplitBoxes
                        ++ take 5 xsSplitLabels
                        ++ take 4 ySplitBoxes
                        ++ take 4 ySplitLabels
                        )
                splitXn2 = waitOn . forkAll
                    $ moveAllRight 1
                    : (spawnYBox signal 1 (6, -0.5)
                        >> oShow yn2B)
                    : oShowWith yn2L (setDuration 1 . oFadeIn)
                    : fmap
                        (\obj -> oTween obj 1 $ oMoveBy (-sep, 0))
                        ( take 6 xsSplitBoxes
                        ++ take 6 xsSplitLabels
                        ++ take 5 ySplitBoxes
                        ++ take 5 ySplitLabels
                        )
                        
            oHide xsBoxes
            oHide xsLabels
            traverse_ oShow xsSplitBoxes
            traverse_ oShow xsSplitLabels
            
            traverse_
                (\x -> oModify x $ oTranslate .~ V2 (-sep/2) (-0.5))
                (ySplitBoxes ++ ySplitLabels)

            traverse_
                (\x -> oModify x $ oEasing .~ signal)
                (xsSplitBoxes ++ xsSplitLabels ++ ySplitBoxes ++ ySplitLabels)

            splitX0
            splitX1
            splitX2
            splitDots >> wait 0.25
            splitXn4
            splitXn3
            splitXn2

            traverse_
                (\x -> oModify x $ oEasing .~ curveS 2)
                (xsSplitBoxes ++ xsSplitLabels ++ ySplitBoxes ++ ySplitLabels)

        emphasizeResult = forkAll
            [ oTween funcDef 1 $ oMoveBy (0, -0.5)
            , oTween funcDef 1 $ \t -> oContext .~
                withSubglyphs [0 .. 10] (withTweenedColor "black" resultColor t)
                . withSubglyphs [11]  (withTweenedColor yColor resultColor t)
                . withSubglyphs [12, 13]  (withTweenedColor xsColor resultColor t)
            , forkAll $ fmap
                (\x -> oTween x 1 $ \t -> oContext .~
                    withSubglyphs [0 ..] (withTweenedColor xsColor resultColor t)
                )
                (xsSplitBoxes ++ xsSplitLabels ++ ySplitBoxes)
            , oTween dotsL 1 $ \t -> oContext .~
                withSubglyphs [1] (withTweenedColor yColor resultColor t)
                . withSubglyphs [0, 2] (withTweenedColor xsColor resultColor t)
            , forkAll $ fmap
                (\x -> oTween x 1 $ \t -> oContext .~
                    withSubglyphs [0 ..] (withTweenedColor yColor resultColor t)
                )
                ySplitLabels
            ]

    wait 1

    showTypeSigFuncDef

    wait 0.5

    showXs

    wait 0.5

    fork showY
    showSplits

    wait 0.5

    emphasizeResult

    wait 3

softSnapOutS :: Signal
softSnapOutS = cssCubicBezierS (0.25, 0, 0, 1)
