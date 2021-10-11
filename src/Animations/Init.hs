{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Animations.Init (initAnimation) where

import Control.Lens ((.~))
import Data.Foldable (traverse_)
import Data.Text (pack)
import Linear (V2(V2))

import Reanimate
import Reanimate.Builtin.Documentation (docEnv)
import Reanimate.Scene

import Utilities.Main
import Utilities.List

main :: IO ()
main = reanimate initAnimation

env :: Animation -> Animation
env = docEnv . addStatic (mkBackground "floralwhite")
    -- . addStatic mkBackgroundGrid
    -- . addStatic mkBackgroundAxes

initAnimation :: Animation
initAnimation = env . applyE (overEnding 1 fadeOutE)
    $ scene
    $ do
        let sep = 1
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
            typeSigSvg = withDefaultBoldTextStrokeFill
                . centerX
                . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
                $ "init :: [a] -> [a]"
            funcDefSvg = withDefaultBoldTextStrokeFill
                . centerX
                . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
                $ "init xs"
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
        xsSplitLabels@(~[ x0Label
                        , x1Label
                        , x2Label
                        , dotsLabel
                        , xn3Label
                        , xn2Label
                        , xn1Label
                        ]) <- traverse oNew xsLabelsSvgs
        traverse_
            (\x -> oModify x $ oTranslate .~ V2 0 (-0.5))
            (xsSplitBoxes ++ xsSplitLabels)

        let showTypeSigFuncDef = forkAllWithLag
                0.25
                [ oShowWith typeSig $ setDuration 1 . oDraw
                , oShowWith funcDef $ setDuration 1 . oDraw
                ]

            showXs = forkAllWithDifferentLags
                $ zip
                    [0, 0.25, 0]
                    [ oTweenContext funcDef 1
                      $ \t ->
                      withSubglyphs [4, 5] (withTweenedColor "black" xsColor t)
                    , oShowWith xsBoxes (setDuration 1 . oDraw)
                    , oShowWith xsLabels (setDuration 1 . oDraw)
                    ]

            splits = do
                oHide xsBoxes
                oHide xsLabels
                traverse_ oShow xsSplitBoxes
                traverse_ oShow xsSplitLabels

                traverse_
                    (\x -> oModify x $ oEasing .~ snapOutS)
                    (xsSplitBoxes ++ xsSplitLabels)

                forkAll
                    [ forkAll
                      $ fmap
                          (\obj -> oTween obj 1 $ oMoveBy (-sep / 2, 0))
                          [x0Box, x0Label]
                    , forkAll
                      $ fmap
                          (\obj -> oTween obj 1 $ oMoveBy (sep / 2, 0))
                          (tail xsSplitBoxes ++ tail xsSplitLabels)
                    ]

                forkAll
                    [ oTween x1Box 1 $ oMoveBy (-sep, 0)
                    , oTween x1Label 1 $ oMoveBy (-sep, 0)
                    ]

                forkAll
                    [ oTween x2Box 1 $ oMoveBy (-sep, 0)
                    , oTween x2Label 1 $ oMoveBy (-sep, 0)
                    ]

                forkAll
                    [ oModify dotsBox $ oEasing .~ (** 1.5) . snapOutS
                    , oModify dotsLabel $ oEasing .~ (** 1.5) . snapOutS
                    , oTween dotsBox 1.5 $ oMoveBy (-sep, 0)
                    , waitOn
                      $ do
                          let xMidLabels = fmap
                                  (withColor xsColor
                                   . withDefaultTextStrokeFill
                                   . centerX
                                   . latexCfgCenteredYWith
                                       firaMonoCfg
                                       withDefaultTextScale
                                   . \n ->
                                   "x\\textsubscript{" <> pack (show n) <> "}")
                                  ([3 .. 9] :: [Int])
                          forkAllWithDifferentLags
                              $ zip
                                  (0.1 : [0.2, 0.175 ..])
                                  ([oTween dotsLabel 1.5 $ oMoveBy (-sep, 0)]
                                   ++ fmap
                                       (\x ->
                                        oModify dotsLabel $ oContext .~ const x)
                                       xMidLabels
                                   ++ [oModify dotsLabel $ oContext .~ id])
                    ]
                wait 0.25

                forkAll
                    [ oTween xn3Box 1 $ oMoveBy (-sep, 0)
                    , oTween xn3Label 1 $ oMoveBy (-sep, 0)
                    ]

                forkAll
                    [ oTween xn2Box 1 $ oMoveBy (-sep, 0)
                    , oTween xn2Label 1 $ oMoveBy (-sep, 0)
                    ]

                traverse_
                    (\x -> oModify x $ oEasing .~ curveS 2)
                    (xsSplitBoxes ++ xsSplitLabels)

            emphasizeResult = forkAll
                [ oTween funcDef 1 $ oMoveBy (0, -0.5)
                , oTween funcDef 1
                  $ \t -> oContext
                  .~ withSubglyphs
                      [0 .. 3]
                      (withTweenedColor "black" resultColor t)
                  . withSubglyphs
                      [4, 5]
                      (withTweenedColor xsColor resultColor t)
                , forkAll
                  $ fmap
                      (\x -> oTween x 1
                       $ \t ->
                       oContext .~ withTweenedColor xsColor resultColor t)
                      (init xsSplitBoxes ++ init xsSplitLabels)
                , forkAll
                  $ fmap
                      (\x -> oTween x 1 $ oMoveBy (1, 0))
                      (init xsSplitBoxes ++ init xsSplitLabels)
                , forkAll
                      [ oHideWith xn1Box $ setDuration 0.5 . oFadeOut
                      , oHideWith xn1Label $ setDuration 0.5 . oFadeOut
                      ]
                ]

        wait 1

        showTypeSigFuncDef

        wait 0.5

        showXs

        wait 0.5

        splits

        -- wait 0.5

        emphasizeResult

        wait 3
