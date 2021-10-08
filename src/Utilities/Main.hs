{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
    Basic utility functions.
-}
module Utilities.Main
    (
    -- * Convenience functions
      (-<)
    , splitInitLast
    -- * LaTeX
    -- ** Configurations
    , firaMonoCfg
    -- ** Invoking LaTeX
    , latexCfgCenteredYWith
    -- * SVG
    -- ** Attributes
    , withDefaultTextScale
    , withDefaultTextStrokeFill
    , withDefaultBoldTextStrokeFill
    , withDefaultLineStrokeFill
    , withColor
    , withColorPixel
    , withTweenedColor
    , withTweenedStrokeColor
    , withTweenedFillColor
    -- ** Transformations
    , distribute1D
    , distributeX
    , distributeY
    , distributeWithSpacingX
    , distributeWithSpacingY
    -- * Scenes
    , forkLag
    , forkAll
    , forkAllWithLag
    , forkAllWithDifferentLags
    -- * Objects
    , oMoveTo
    , oTweenContext
    -- * Other
    , mkColorPixel
    , mkBackgroundAxes
    , mkBackgroundGrid
    )
where

import Codec.Picture (PixelRGBA8)
import Control.Lens ((%~))
import Data.List (intersperse)
import Data.Text (Text)
import Graphics.SvgTree (Texture (ColorRef))
import Linear.V2 (V2 (V2))
import Linear.Vector (lerp)

import Reanimate
import Reanimate.ColorComponents (interpolateRGBA8, labComponents)
import Reanimate.LaTeX (TexConfig (TexConfig), TexEngine (LaTeX), latexCfg)
import Reanimate.Scene (Object, ObjectData, oContext, oTranslate, oTween)

infixl 1 -<

{-|
    A left-associative version of '$', with precedence 1. Occasionally useful.
-}
(-<) :: (a -> b) -> a -> b
(-<) = ($)

{-|
    Split a list into a 2-tuple containing its @init@ and @last@.

    > splitInitLast [] == Nothing
    > splitInitLast xs == Just (init xs, last xs)
-}
splitInitLast :: [a] -> Maybe ([a], a)
splitInitLast [] = Nothing
splitInitLast [x] = Just ([], x)
splitInitLast (x : xs) = Just (x : a, b)
    where Just (a, b) = splitInitLast xs

{-|
    Enable the Fira Mono font in \(\LaTeX\) glyphs. Use with functions
    starting with @latexCfg@.
-}
firaMonoCfg :: TexConfig
firaMonoCfg = TexConfig LaTeX
    [ "\\usepackage{FiraMono}"
    , "\\renewcommand*\\familydefault{\\ttdefault}"
    , "\\usepackage[T1]{fontenc}"
    ]
    ["\\normalfont"]

{-|
    Scale an SVG uniformly by a default factor suitable for text. Best used
    with 'mkText', 'latex', etc.
-}
withDefaultTextScale :: SVG -> SVG
withDefaultTextScale = scale 0.5

{-|
    Modify an SVG using a default stroke width and fill opacity suitable for
    regular text. Best used with 'mkText', 'latex', etc.
-}
withDefaultTextStrokeFill :: SVG -> SVG
withDefaultTextStrokeFill = withStrokeWidth 0.025 . withFillOpacity 1

{-|
    Modify an SVG using a default stroke width and fill opacity suitable for
    bold text. Best used with 'mkText', 'latex', etc.
-}
withDefaultBoldTextStrokeFill :: SVG -> SVG
withDefaultBoldTextStrokeFill = withStrokeWidth 0.05 . withFillOpacity 1

{-|
    Modify an SVG using a default stroke width and fill opacity suitable for
    outline shapes. Best used with 'mkRect', 'mkCircle', etc.
-}
withDefaultLineStrokeFill :: SVG -> SVG
withDefaultLineStrokeFill = withStrokeWidth 0.05 . withFillOpacity 0

{-|
    Create a pair of axes spanning the screen.
-}
mkBackgroundAxes :: SVG
mkBackgroundAxes =
    gridLayout
    . replicate 2
    . replicate 2
    . withStrokeWidth 0.03
    . withStrokeColor "gray"
    $ mkRect (screenWidth / 2) (screenHeight / 2)

{-|
    Create a unit square grid spanning the screen.
-}
mkBackgroundGrid :: SVG
mkBackgroundGrid =
    gridLayout
    . replicate (round (screenHeight :: Double))
    . replicate (round (screenWidth :: Double))
    . withStrokeWidth 0.01
    . withStrokeColor "gray"
    $ mkRect 1 1

{-|
    Given a list of element sizes in one dimension, return how many units from
    the origin each element must be translated so that they line up
    head-to-tail, centered at the origin as a whole.

    For example,
    > distribute1D [2, 4, 3, 1] == [-4, -1, 2.5, 4.5]
-}
distribute1D :: [Double] -> [Double]
distribute1D sizes = scanl1 (+) $ zipWith avg sizes ((-s) : sizes)
  where
    s = sum sizes
    avg a b = (a + b) / 2

{-|
    Horizontally distribute a list of SVGs relative to the bounding box that
    contains all of them, with a specific amount of spacing in between.
-}
distributeWithSpacingX :: Double -> [SVG] -> [SVG]
distributeWithSpacingX spacing svgs = svgs'
  where
    groupCenterX =
        let (minX, _, w, _) = boundingBox $ mkGroup svgs
        in minX + w / 2
    widths = svgWidth <$> svgs
    translations = unintersperse . distribute1D . intersperse spacing $ widths
    svgs' = zipWith
        (\dx svg -> translate (groupCenterX + dx) 0 . centerX $ svg)
        translations
        svgs

    unintersperse xs = case xs of
        [] -> []
        [x] -> [x]
        x : _ : rest -> x : unintersperse rest

{-|
    Horizontally distribute a list of SVGs relative to the bounding box that
    contains all of them, with no spacing in between.
-}
distributeX :: [SVG] -> [SVG]
distributeX = distributeWithSpacingX 0

{-|
    Vertically distribute a list of SVGs relative to the bounding box that
    contains all of them, with a specific amount of spacing in between.
-}
distributeWithSpacingY :: Double -> [SVG] -> [SVG]
distributeWithSpacingY spacing svgs = svgs'
  where
    groupCenterY =
        let (_, minY, _, h) = boundingBox $ mkGroup svgs
        in minY + h / 2
    heights = svgHeight <$> svgs
    translations = unintersperse . distribute1D . intersperse spacing $ heights
    svgs' = zipWith
        (\dy svg -> translate 0 (groupCenterY + dy) . centerY $ svg)
        translations
        svgs

    unintersperse xs = case xs of
        [] -> []
        [x] -> [x]
        x : _ : rest -> x : unintersperse rest

{-|
    Vertically distribute a list of SVGs relative to the bounding box that
    contains all of them, with no spacing in between.
-}
distributeY :: [SVG] -> [SVG]
distributeY = distributeWithSpacingY 0

{-|
    Similar to 'latexCfg', but the resulting text glyph is also vertically
    centered using its baseline, so that different glyphs containing
    characters like \"y\" and \"^\" are properly aligned.

    For now, baseline centering requires that any SVG transformation (such as
    @scale 0.5@) be given as the second argument.
-}
latexCfgCenteredYWith :: TexConfig -> (SVG -> SVG) -> Text -> SVG
latexCfgCenteredYWith config transformation =
    mkGroup
    . drop 4
    . removeGroups
    . centerY
    . transformation
    . latexCfg config
    . ("$\\biggl \\lvert$" <>)

{-|
    'fork' a scene and add a time delay.

    > do {forkLag 1 scene1; scene2} == do {fork scene1; wait 1; scene2}
-}
forkLag :: Double -> Scene s a -> Scene s ()
forkLag time action = fork action >> wait time

{-|
    'forkLag' each given scene with its corresponding time delay, except for
    the last scene, which is neither 'fork'ed nor given a time delay. An empty
    scene is played at the end.

    > forkAllWithDifferentLags [(1, scene1), (2, scene2), (3, scene3)] == do
    >    fork scene1; wait 1
    >    fork scene2; wait 2
    >    scene 3
    >    pure ()
-}
forkAllWithDifferentLags :: [(Double, Scene s a)] -> Scene s ()
forkAllWithDifferentLags scenes = case splitInitLast scenes of
    Nothing -> pure ()
    Just (initScenes, lastScene) -> do
        sequence_ $ uncurry forkLag <$> initScenes
        _ <- snd lastScene
        pure ()

{-|
    'forkLag' each given scene with a uniform time delay, except for the last
    scene, which is neither 'fork'ed nor given a time delay. An empty scene is
    played at the end.

    > forkAllWithLag 2 [scene1, scene2, scene3] == do
    >     fork scene1; wait 2
    >     fork scene2; wait 2
    >     scene3
    >     pure ()
-}
forkAllWithLag :: Double -> [Scene s a] -> Scene s ()
forkAllWithLag _ [] = pure ()
forkAllWithLag time scenes = forkAllWithDifferentLags $ (time,) <$> scenes

{-|
    'fork' each given scene except the last. An empty scene is played at the
    end.

    > forkAll [scene1, scene2, scene3] == do
    >     fork scene1
    >     fork scene2
    >     scene3
    >     pure ()
-}
forkAll :: [Scene s a] -> Scene s ()
forkAll = forkAllWithLag 0

{-|
    Simultaneously set the stroke and fill color.
-}
withColor :: String -> SVG -> SVG
withColor color =
    withStrokeColor color
    . withFillColor color

{-|
    Simultaneously set the stroke and fill color.
-}
withColorPixel :: PixelRGBA8 -> SVG -> SVG
withColorPixel colorPixel =
    withStrokeColorPixel colorPixel
    . withFillColorPixel colorPixel

{-|
    Gradually translate an 'Reanimate.Scene.Object' toward a new position by
    modifying its internal 'ObjectData'. Best used with 'oTween'.

    For example, @oTween obj 1 $ oMoveTo (3, 4)@ gradually moves @obj@ to the
    point (3, 4) over 1 second.
-}
oMoveTo :: (Double, Double) -> Double -> ObjectData a -> ObjectData a
oMoveTo (x, y) t = oTranslate %~ lerp t (V2 x y)

{-|
    Modify the context of an 'Reanimate.Scene.Object' over a set duration by
    applying a tweening function to it.

    For example,
    @oTweenContext obj 1 (withTweenedStrokeColor "purple" "blue")@
    changes the SVG stroke color of @obj@ from purple to blue over 1 second.
-}
oTweenContext :: Object s a -> Duration -> (Double -> SVG -> SVG) -> Scene s ()
oTweenContext obj dur f = oTween obj dur $ \t -> oContext %~ (f t .)

{-|
    Convert an SVG color name to an RGBA value. If the given name is not a
    valid SVG color name, then the value (240, 248, 255, 255) (off-white) is
    returned.
-}
mkColorPixel :: String -> PixelRGBA8
mkColorPixel color = pixel
    where ColorRef pixel = mkColor color

{-|
    Interpolate between two SVG color names using the CIELAB color space.
-}
interpolateAsPixel :: String -> String -> Double -> PixelRGBA8
interpolateAsPixel fromColor toColor =
    interpolateRGBA8 labComponents rgba1 rgba2
    where
        (ColorRef rgba1) = mkColor fromColor
        (ColorRef rgba2) = mkColor toColor

{-|
    Tween (\"fade\") the color of an SVG between two RGBA values.
-}
withTweenedColorPixel :: PixelRGBA8 -> PixelRGBA8 -> Double -> SVG -> SVG
withTweenedColorPixel fromPixel toPixel t =
    withColorPixel $ interpolateRGBA8 labComponents fromPixel toPixel t

{-|
    Tween (\"fade\") the color of an SVG between two color names.

    For example, @withTweenedColor "magenta" "green" 0.25 svg@ sets the color
    of @svg@ to be a quarter of the way between magenta and green.
-}
withTweenedColor :: String -> String -> Double -> SVG -> SVG
withTweenedColor fromColor toColor =
    withTweenedColorPixel (mkColorPixel fromColor) (mkColorPixel toColor)

{-|
    Tween (\"fade\") the stroke color of an SVG between two RGBA values.
-}
withTweenedStrokeColorPixel :: PixelRGBA8 -> PixelRGBA8 -> Double -> SVG -> SVG
withTweenedStrokeColorPixel fromPixel toPixel t =
    withStrokeColorPixel $ interpolateRGBA8 labComponents fromPixel toPixel t

{-|
    Tween (\"fade\") the stroke color of an SVG between two color names.

    For example, @withTweenedStrokeColor "magenta" "green" 0.25 svg@ sets the
    stroke color of @svg@ to be a quarter of the way between magenta and green.
-}
withTweenedStrokeColor :: String -> String -> Double -> SVG -> SVG
withTweenedStrokeColor fromColor toColor =
    withTweenedStrokeColorPixel (mkColorPixel fromColor) (mkColorPixel toColor)

{-|
    Tween (\"fade\") the fill color of an SVG between two RGBA values.
-}
withTweenedFillColorPixel :: PixelRGBA8 -> PixelRGBA8 -> Double -> SVG -> SVG
withTweenedFillColorPixel fromPixel toPixel t =
    withFillColorPixel $ interpolateRGBA8 labComponents fromPixel toPixel t

{-|
    Tween (\"fade\") the fill color of an SVG between two color names.

    For example, @withTweenedFillColor "magenta" "green" 0.25 svg@ sets the
    fill color of @svg@ to be a quarter of the way between magenta and green.
-}
withTweenedFillColor :: String -> String -> Double -> SVG -> SVG
withTweenedFillColor fromColor toColor =
    withTweenedFillColorPixel (mkColorPixel fromColor) (mkColorPixel toColor)
