{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
    Basic utility functions.
-}
module Utilities.Main
    ( -- * Convenience functions
      (-<)
    , splitInitLast
      -- * Reanimate
      -- ** LaTeX
      -- *** Configurations
    , firaMonoCfg
      -- *** Invoking LaTeX
    , latexCfgCenteredYWith
    , latexCfgChunksCenteredYWith
      -- ** SVG
      -- *** Attributes
    , svgCenter
    , svgCenterX
    , svgCenterY
    , withDefaultTextScale
    , withDefaultTextStrokeFill
    , withDefaultBoldTextStrokeFill
    , withDefaultLineStrokeFill
    , withColor
    , withColorPixel
    , withTweenedColor
    , withTweenedStrokeColor
    , withTweenedFillColor
      -- *** Transformations
    , centerGroup
    , centerGroupX
    , centerGroupY
    , distribute1D
    , distributeX
    , distributeY
    , distributeWithSpacingX
    , distributeWithSpacingY
      -- ** Animations
      -- *** Easing functions
    , cssCubicBezierS
    , snapInS
    , snapOutS
    , softSnapInS
    , softSnapOutS
      -- *** Scenes
    , forkLag
    , forkAll
    , forkAllWithLag
    , forkAllWithDifferentLags
      -- **** Objects
    , oNewWithSvgPosition
    , oMoveTo
    , oMoveBy
    , oTweenContext
      -- *** Effects
    , mkAnimationE
    , animateE
    , composeE
      -- *** Scaling
    , fitAnimationToSize
      -- ** Other
    , mkColorPixel
    , mkBackgroundAxes
    , mkBackgroundGrid
      -- * Brick
    , strWrapBreak
    ,plexMonoLightCfg,plexMonoMediumCfg)
where

import Codec.Picture (PixelRGBA8)
import Control.Lens ((%~), (.~))
import Data.List (find, intersperse)
import Data.Text (Text, replace)
import Graphics.SvgTree (Texture (ColorRef))
import Linear (V2 (V2), lerp)
import Text.Wrap (WrapSettings (breakLongWords), defaultWrapSettings)

import Reanimate
import Reanimate.ColorComponents (interpolateRGBA8, labComponents)
import Reanimate.LaTeX
    ( TexConfig (TexConfig)
    , TexEngine (LaTeX, XeLaTeX)
    , latexCfg
    , latexCfgChunks
    )
import Reanimate.Scene
    ( Object
    , ObjectData
    , oContext
    , oModify
    , oNew
    , oTranslate
    , oTween
    )

import Brick
import Text.RawString.QQ (r)

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
    where
        Just (a, b) = splitInitLast xs

{-|
    Enable the Fira Mono font in \(\LaTeX\) glyphs. Use with functions
    starting with @latexCfg@.
-}
firaMonoCfg :: TexConfig
firaMonoCfg = TexConfig
    LaTeX
    [ "\\usepackage{FiraMono}"
    , "\\renewcommand*\\familydefault{\\ttdefault}"
    , "\\usepackage[T1]{fontenc}"
    ]
    ["\\normalfont"]

plexMonoLightCfg :: TexConfig
plexMonoLightCfg = TexConfig
    XeLaTeX
    [ [r|\usepackage[T1]{fontenc}|]
    , [r|\usepackage{setspace}|]
    , [r|\usepackage[usefilenames,RMstyle=Light,SSstyle=Light,TTstyle=Light,DefaultFeatures={Ligatures=Common}]{plex-otf} %|]
    , [r|\renewcommand*\familydefault{\ttdefault} %% Only if the base font of the document is to be monospaced|]
    ]
    [[r|\mdseries|]]

plexMonoMediumCfg :: TexConfig
plexMonoMediumCfg = TexConfig
    XeLaTeX
    [ [r|\usepackage[T1]{fontenc}|]
    , [r|\usepackage{setspace}|]
    , [r|\usepackage[usefilenames,RMstyle=Medium,SSstyle=Medium,TTstyle=Medium,DefaultFeatures={Ligatures=Common}]{plex-otf} %|]
    , [r|\renewcommand*\familydefault{\ttdefault} %% Only if the base font of the document is to be monospaced|]
    , [r|\newcommand{\textlf}{\PlexLightTT}|]
    ]
    [[r|\mdseries|]]

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
withDefaultBoldTextStrokeFill = withStrokeWidth 0.025 . withFillOpacity 1

{-|
    Modify an SVG using a default stroke width and fill opacity suitable for
    outline shapes. Best used with 'mkRect', 'mkCircle', etc.
-}
withDefaultLineStrokeFill :: SVG -> SVG
withDefaultLineStrokeFill = withStrokeWidth 0.04 . withFillOpacity 0

{-|
    Create a pair of axes spanning the screen.
-}
mkBackgroundAxes :: SVG
mkBackgroundAxes = gridLayout
    . replicate 2
    . replicate 2
    . withStrokeWidth 0.03
    . withStrokeColor "gray"
    $ mkRect (screenWidth / 2) (screenHeight / 2)

{-|
    Create a unit square grid spanning the screen.
-}
mkBackgroundGrid :: SVG
mkBackgroundGrid = gridLayout
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
            let (minX, _, w, _) = boundingBox $ mkGroup svgs in minX + w / 2
        widths = svgWidth <$> svgs
        translations =
            unintersperse . distribute1D . intersperse spacing $ widths
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
            let (_, minY, _, h) = boundingBox $ mkGroup svgs in minY + h / 2
        heights = svgHeight <$> svgs
        translations =
            unintersperse . distribute1D . intersperse spacing $ heights
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
latexCfgCenteredYWith config transformation = mkGroup
    . (\x -> take (length x - 4) x)
    . drop 4
    . removeGroups
    . centerY
    . transformation
    . latexCfg config
    . (<> "\\makebox[0em]{$\\biggr\\rvert$}")
    . ("\\makebox[0em]{$\\biggl\\lvert$}" <>)
    . replace "'" "{\\textquotesingle}"
    . replace " " "{\\ }"

{-|
    Similar to 'latexCfgChunks', but the resulting text glyphs are also
    vertically centered using their common baseline, so that different glyphs
    containing characters like \"y\" and \"^\" are properly aligned.

    For now, baseline centering requires that any SVG transformation (such as
    @scale 0.5@) be given as the second argument.
-}
latexCfgChunksCenteredYWith :: TexConfig -> (SVG -> SVG) -> [Text] -> [SVG]
latexCfgChunksCenteredYWith config transformation =
    init
    . tail
    . centerGroupY
    . fmap transformation
    . latexCfgChunks config
    . (<> ["\\makebox[0em]{$\\biggr\\rvert$}"])
    . (["\\makebox[0em]{$\\biggl\\lvert$}"] <>)
    . fmap
        ( replace "'" "{\\textquotesingle}"
        . replace " " "{\\ }"
        )

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
forkAllWithLag time scenes = forkAllWithDifferentLags $ (time, ) <$> scenes

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
withColor color = withStrokeColor color . withFillColor color

{-|
    Simultaneously set the stroke and fill color.
-}
withColorPixel :: PixelRGBA8 -> SVG -> SVG
withColorPixel colorPixel =
    withStrokeColorPixel colorPixel . withFillColorPixel colorPixel

{-|
    Gradually translate an 'Reanimate.Scene.Object' toward a new position by
    modifying its internal 'ObjectData'. Best used with 'oTween'.

    For example, @oTween obj 1 $ oMoveTo (3, 4)@ gradually moves @obj@ to the
    point (3, 4) over 1 second.
-}
oMoveTo :: (Double, Double) -> Double -> ObjectData a -> ObjectData a
oMoveTo (x, y) t = oTranslate %~ lerp t (V2 x y)

{-|
    Gradually translate an 'Reanimate.Scene.Object' a certain amount by
    modifying its internal 'ObjectData'. Best used with 'oTween'.

    For example, @oTween obj 1 $ oMoveBy (3, 4)@ gradually moves @obj@ by 3
    units rightward and 4 units upward over 1 second.
-}
oMoveBy :: (Double, Double) -> Double -> ObjectData a -> ObjectData a
oMoveBy (x, y) t = oTranslate %~ \curr -> lerp t (V2 x y + curr) curr

{-|
    Modify the context of an 'Reanimate.Scene.Object' over a set duration by
    applying a tweening function to it.

    For example,
    @oTweenContext obj 1 (withTweenedStrokeColor "purple" "blue")@
    changes the SVG stroke color of @obj@ from purple to blue over 1 second.
-}
oTweenContext
    :: Object s a -> Duration -> (Double -> SVG -> SVG) -> Scene s ()
oTweenContext obj dur f = oTween obj dur $ \t -> oContext %~ (f t .)

{-|
    Convert an SVG color name to an RGBA value. If the given name is not a
    valid SVG color name, then the value (240, 248, 255, 255) (off-white) is
    returned.
-}
mkColorPixel :: String -> PixelRGBA8
mkColorPixel color = pixel
    where
        ColorRef pixel = mkColor color

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
withTweenedStrokeColorPixel
    :: PixelRGBA8 -> PixelRGBA8 -> Double -> SVG -> SVG
withTweenedStrokeColorPixel fromPixel toPixel t =
    withStrokeColorPixel $ interpolateRGBA8 labComponents fromPixel toPixel t

{-|
    Tween (\"fade\") the stroke color of an SVG between two color names.

    For example, @withTweenedStrokeColor "magenta" "green" 0.25 svg@ sets the
    stroke color of @svg@ to be a quarter of the way between magenta and green.
-}
withTweenedStrokeColor :: String -> String -> Double -> SVG -> SVG
withTweenedStrokeColor fromColor toColor = withTweenedStrokeColorPixel
    (mkColorPixel fromColor)
    (mkColorPixel toColor)

{-|
    Tween (\"fade\") the fill color of an SVG between two RGBA values.
-}
withTweenedFillColorPixel :: PixelRGBA8 -> PixelRGBA8 -> Double -> SVG -> SVG
withTweenedFillColorPixel fromPixel toPixel t =
    withFillColorPixel $ interpolateRGBA8 labComponents fromPixel toPixel t

{-|
    Tween (\"fade\") the fill color of an SVG between two color names.

    For example, @withTweenedFillColor "magenta" "green" 0.25 svg@ sets the fill
    color of @svg@ to be a quarter of the way between magenta and green.
-}
withTweenedFillColor :: String -> String -> Double -> SVG -> SVG
withTweenedFillColor fromColor toColor =
    withTweenedFillColorPixel (mkColorPixel fromColor) (mkColorPixel toColor)

{-|
    Similar to @cubic-bezier@ implemented in CSS.
-}
cssCubicBezierS :: (Double, Double, Double, Double) -> Signal
cssCubicBezierS (x1, y1, x2, y2) s = maybe s snd closestXY
    where
        ts = takeWhile (<= 1) $ iterate (+ 1 / 1200) 0
        xs = fxt <$> ts
        ys = fyt <$> ts
        fxt t =
            3 * x1' * t * (1 - t) ^ (2 :: Int)
            + 3 * x2' * t ^ (2 :: Int) * (1 - t)
            + t ^ (3 :: Int)
        fyt t =
            3 * y1 * t * (1 - t) ^ (2 :: Int)
            + 3 * y2 * t ^ (2 :: Int) * (1 - t)
            + t ^ (3 :: Int)
        closestXY = find ((>= s) . fst) $ zip xs ys
        x1'
            | x1 < 0 = 0
            | x1 > 1 = 1
            | otherwise = x1
        x2'
            | x2 < 0 = 0
            | x2 > 1 = 1
            | otherwise = x2

{-|
    Snap-out signal.
-}
snapOutS :: Signal
snapOutS = cssCubicBezierS (0.060, 0.975, 0.195, 0.985)

{-|
    Snap-in signal.
-}
snapInS :: Signal
snapInS = reverseS . snapOutS . reverseS

{-|
    Soft snap-out signal.
-}
softSnapOutS :: Signal
softSnapOutS = cssCubicBezierS (0.25, 0, 0, 1)

{-|
    Soft snap-in signal.
-}
softSnapInS :: Signal
softSnapInS = reverseS . softSnapOutS . reverseS

{-|
    Compose multiple 'Effect's into a single 'Effect'.
-}
composeE :: [Effect] -> Effect
composeE [] _ _ = id
composeE effects d t = foldr1 (.) $ fmap (\e -> e d t) effects

{-|
    Animate an 'Effect' applied to an 'SVG' over a set duration.
-}
mkAnimationE :: Duration -> Effect -> SVG -> Animation
mkAnimationE d effect svg = mkAnimation d $ \t -> effect 1 t svg

{-|
    Animate an 'Effect' applied to an 'SVG' over a duration of 1.
-}
animateE :: Effect -> SVG -> Animation
animateE = mkAnimationE 1

{-|
    Translate the given list of 'SVG's such that they are horizontally centered
    as a whole.
-}
centerGroupX :: [SVG] -> [SVG]
centerGroupX svgs = fmap (translate (-midX) 0) svgs
    where
        (minX, _, w, _) = boundingBox $ mkGroup svgs
        midX = minX + w/2

{-|
    Translate the given list of 'SVG's such that they are vertically centered
    as a whole.
-}
centerGroupY :: [SVG] -> [SVG]
centerGroupY svgs = fmap (translate 0 (-midY)) svgs
    where
        (_, minY, _, h) = boundingBox $ mkGroup svgs
        midY = minY + h/2

{-|
    Translate the given list of 'SVG's such that they are both horizontally and
    vertically centered as a whole.
-}
centerGroup :: [SVG] -> [SVG]
centerGroup svgs = fmap (translate (-midX) (-midY)) svgs
    where
        (minX, minY, w, h) = boundingBox $ mkGroup svgs
        midX = minX + w/2
        midY = minY + h/2

{-|
    Fit an animation within a set canvas size. If the animation already fits,
    it is not scaled up.
-}
fitAnimationToSize :: (Double, Double) -> Animation -> Animation
fitAnimationToSize (width, height) animation = mapA (scale factor) animation
    where
        dur = duration animation
        sampleFrequency = 20
        sampleFrames =
            (`frameAt` animation) . (dur *) <$> [0, 1/sampleFrequency .. 1]
        frameBoundingBoxes = boundingBox <$> sampleFrames
        maxFrameWidth = maximum $ (\(_, _, w, _) -> w) <$> frameBoundingBoxes
        maxFrameHeight = maximum $ (\(_, _, _, h) -> h) <$> frameBoundingBoxes
        factor = minimum [1, width/maxFrameWidth, height/maxFrameHeight]

{-|
    Create a new 'Reanimate.Scene.Object' from an SVG, such that the position
    of the SVG is transferred to the new object.

    For example, if @svg@ is centered at (3, 4), then the new object created by
    @oNewWithSvgPosition svg@ has position (3, 4), and the contained SVG is
    centered at (0, 0). This way, all translations can be handled using the
    object's position, without having to manipulate the inner SVG.
-}
oNewWithSvgPosition :: SVG -> Scene s (Object s SVG)
oNewWithSvgPosition svg = do
    let (locX, locY) = svgCenter svg
    obj <- oNew $ center svg
    oModify obj $ oTranslate .~ V2 locX locY
    pure obj

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

strWrapBreak :: String -> Widget n
strWrapBreak = strWrapWith (defaultWrapSettings {breakLongWords = True})
