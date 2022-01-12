{-# OPTIONS_GHC -Wall #-}

{-|
    SVG-related functions.
-}
module Utilities.Svg
    ( mkColorPixel
    , rgb
    , mkBackgroundAxes
    , mkBackgroundGrid
    , svgCenter
    , svgCenterX
    , svgCenterY
    , withDefaultTextScale
    , withDefaultTextStrokeFill
    , withDefaultLineStrokeFill
    , withColor
    , withColorPixel
    , withTweenedColor
    , withTweenedColorPixel
    , withTweenedStrokeColor
    , withTweenedStrokeColorPixel
    , withTweenedFillColor
    , withTweenedFillColorPixel
    , centerGroup
    , centerGroupX
    , centerGroupY
    )
    where

import Codec.Picture (Pixel8, PixelRGBA8 (PixelRGBA8))
import Data.Function (on)
import Graphics.SvgTree (Texture (ColorRef))

import Reanimate
import Reanimate.ColorComponents (interpolateRGBA8, labComponents)

{-|
    Convert a web color name to an RGBA value. If the given name is not a valid
    web color name, then the color (240, 248, 255, 255) (off-white) is
    returned.
-}
mkColorPixel :: String -> PixelRGBA8
mkColorPixel color = pixel
    where ColorRef pixel = mkColor color

{-|
    Convert an RGB triplet into an RGBA value.
-}
rgb :: (Pixel8, Pixel8, Pixel8) -> PixelRGBA8
rgb (r, g, b) = PixelRGBA8 r g b 255

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
    Create a grid spanning the screen with set /x/- and /y/-steps.
-}
mkBackgroundGrid :: Double -> Double -> SVG
mkBackgroundGrid x y =
    center
    . gridLayout
    . replicate (2 * ceiling (screenHeight/y))
    . replicate (2 * ceiling (screenWidth/x))
    . withStrokeWidth 0.01
    . withStrokeColor "gray"
    $ mkRect (adjust x) (adjust y)
    where
        adjust 0 = 1
        adjust s = abs s


{-|
    Find the coordinates of the center of an SVG.

    This function internally uses 'boundingBox' and has the same limitations.
-}
svgCenter :: SVG -> (Double, Double)
svgCenter svg = (minX + w/2, minY + h/2)
    where (minX, minY, w, h) = boundingBox svg

{-|
    Find the /x/-coordinate of the center of an SVG.

    This function internally uses 'boundingBox' and has the same limitations.
-}
svgCenterX :: SVG -> Double
svgCenterX = fst . svgCenter

{-|
    Find the /y/-coordinate of the center of an SVG.

    This function internally uses 'boundingBox' and has the same limitations.
-}
svgCenterY :: SVG -> Double
svgCenterY = snd . svgCenter

{-|
    Scale an SVG uniformly by a default factor suitable for text.

    This function is best used with text-rendering functions like 'mkText' and
    'latex'.
-}
withDefaultTextScale :: SVG -> SVG
withDefaultTextScale = scale 0.5

{-|
    Modify an SVG using a default stroke width and fill opacity suitable for
    text.

    This function is best used with text-rendering functions like 'mkText' and
    'latex'.
-}
withDefaultTextStrokeFill :: SVG -> SVG
withDefaultTextStrokeFill =
    withStrokeWidth (defaultStrokeWidth / 2) . withFillOpacity 1

{-|
    Modify an SVG using a default stroke width and fill opacity suitable for
    outline shapes.

    This function is best used with shape-rendering functions like 'mkRect' and
    'mkCircle'.
-}
withDefaultLineStrokeFill :: SVG -> SVG
withDefaultLineStrokeFill =
    withStrokeWidth defaultStrokeWidth . withFillOpacity 0

{-|
    Simultaneously set the stroke and fill color of an SVG.
-}
withColor :: String -> SVG -> SVG
withColor color = withStrokeColor color . withFillColor color

{-|
    Simultaneously set the stroke and fill color of an SVG, using an RGBA
    value.
-}
withColorPixel :: PixelRGBA8 -> SVG -> SVG
withColorPixel colorPixel =
    withStrokeColorPixel colorPixel . withFillColorPixel colorPixel

{-|
    Tween (\"fade\") the color of an SVG between two RGBA values.

    This function uses the CIELAB color space for interpolation.
-}
withTweenedColorPixel :: PixelRGBA8 -> PixelRGBA8 -> Double -> SVG -> SVG
withTweenedColorPixel fromPixel toPixel t =
    withColorPixel $ interpolateRGBA8 labComponents fromPixel toPixel t

{-|
    Tween (\"fade\") the color of an SVG between two color names.

    For example, @withTweenedColor "magenta" "green" 0.25 svg@ sets the color
    of @svg@ to be a quarter of the way between magenta and green.

    This function uses the CIELAB color space for interpolation.
-}
withTweenedColor :: String -> String -> Double -> SVG -> SVG
withTweenedColor = withTweenedColorPixel `on` mkColorPixel

{-|
    Tween (\"fade\") the stroke color of an SVG between two RGBA values.

    This function uses the CIELAB color space for interpolation.
-}
withTweenedStrokeColorPixel :: PixelRGBA8 -> PixelRGBA8 -> Double -> SVG -> SVG
withTweenedStrokeColorPixel fromPixel toPixel t =
    withStrokeColorPixel $ interpolateRGBA8 labComponents fromPixel toPixel t

{-|
    Tween (\"fade\") the stroke color of an SVG between two color names.

    For example, @withTweenedStrokeColor "magenta" "green" 0.25 svg@ sets the
    stroke color of @svg@ to be a quarter of the way between magenta and green.

    This function uses the CIELAB color space for interpolation.
-}
withTweenedStrokeColor :: String -> String -> Double -> SVG -> SVG
withTweenedStrokeColor = withTweenedStrokeColorPixel `on` mkColorPixel

{-|
    Tween (\"fade\") the fill color of an SVG between two RGBA values.

    This function uses the CIELAB color space for interpolation.
-}
withTweenedFillColorPixel :: PixelRGBA8 -> PixelRGBA8 -> Double -> SVG -> SVG
withTweenedFillColorPixel fromPixel toPixel t =
    withFillColorPixel $ interpolateRGBA8 labComponents fromPixel toPixel t

{-|
    Tween (\"fade\") the fill color of an SVG between two color names.

    For example, @withTweenedFillColor "magenta" "green" 0.25 svg@ sets the
    fill color of @svg@ to be a quarter of the way between magenta and green.

    This function uses the CIELAB color space for interpolation.
-}
withTweenedFillColor :: String -> String -> Double -> SVG -> SVG
withTweenedFillColor = withTweenedFillColorPixel `on` mkColorPixel

{-|
    Translate the given list of SVGs such that they are horizontally centered
    as a whole.

    This function internally uses 'boundingBox' and has the same limitations.
-}
centerGroupX :: [SVG] -> [SVG]
centerGroupX svgs = translate (-midX) 0 <$> svgs
    where midX = svgCenterX $ mkGroup svgs

{-|
    Translate the given list of SVGs such that they are vertically centered
    as a whole.

    This function internally uses 'boundingBox' and has the same limitations.
-}
centerGroupY :: [SVG] -> [SVG]
centerGroupY svgs = translate 0 (-midY) <$> svgs
    where midY = svgCenterY $ mkGroup svgs

{-|
    Translate the given list of 'SVG's such that they are both horizontally and
    vertically centered as a whole.
-}
centerGroup :: [SVG] -> [SVG]
centerGroup svgs = translate (-midX) (-midY) <$> svgs
    where (midX, midY) = svgCenter $ mkGroup svgs
