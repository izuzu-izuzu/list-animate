{-# OPTIONS_GHC -Wall #-}

{-|
    Miscellaneous functions.
-}
module Utilities.Misc
    ( (-<)
    , distribute1D
    , fitAnimationToSize
    )
    where

import Reanimate

infixl 1 -<

{-|
    A left-associative version of '$', with precedence 1. Occasionally useful.
-}
(-<) :: (a -> b) -> a -> b
(-<) = ($)

{-|
    Given a list of element sizes in one dimension, return the distance by
    which each element must be translated from the origin, such that they line
    up head-to-tail, centered at the origin as a whole.

    For example, suppose @svgs = [mkRect 2 1, mkRect 4 2, mkRect 3 2, mkRect 1
    1]@ is a list of SVGs, all centered at the origin. To line up these SVGs
    head-to-tail horizontally, first take their /x/-sizes (i.e. widths), and
    then use @distribute1D@.

    >>> distribute1D [2, 4, 3, 1]  -- or distribute1D $ svgWidth <$> svgs
    [-4, -1, 2.5, 4.5]

    The first SVG needs to be translated by -4 /x/-units, the second by -1
    /x/-unit, and so on.
-}
distribute1D :: [Double] -> [Double]
distribute1D sizes = scanl1 (+) $ zipWith avg sizes ((-s) : sizes)
    where
        s = sum sizes
        avg a b = (a + b) / 2

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
