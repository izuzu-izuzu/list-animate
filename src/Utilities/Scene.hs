{-# OPTIONS_GHC -Wall #-}

{-|
    Functions related to 'Scene's and 'Object's.
-}
module Utilities.Scene
    ( oNewWithSvgPosition
    , oMoveTo
    , oMoveToX
    , oMoveToY
    , oMoveBy
    )
    where

import Control.Lens ((%~), (+~), (.~))
import Linear (Additive (lerp), V2 (V2))

import Reanimate
import Reanimate.Scene (Object, oNew, oTranslate, oModify, ObjectData)

import Utilities.Svg (svgCenter)

{-|
    Create a new 'Reanimate.Scene.Object' from an SVG, such that the position
    of the SVG is transferred to the object. This way, all translations can be
    handled using the object's position, without having to manipulate the inner
    SVG.

    For example, if @svg@ is centered at (3, 4), then the new object created by
    @oNewWithSvgPosition svg@ has position (3, 4), and the contained SVG is
    centered at (0, 0).
-}
oNewWithSvgPosition :: SVG -> Scene s (Object s SVG)
oNewWithSvgPosition svg = do
    let (locX, locY) = svgCenter svg
    obj <- oNew $ center svg
    oModify obj $ oTranslate .~ V2 locX locY
    pure obj

{-|
    Gradually translate an 'Reanimate.Scene.Object' to a new position.

    This function is best used with 'oTween'.

    For example, @oTween obj 1 $ oMoveTo (3, 4)@ gradually moves @obj@ to the
    point (3, 4) over 1 second.
-}
oMoveTo :: (Double, Double) -> Double -> ObjectData a -> ObjectData a
oMoveTo (x, y) t = oTranslate %~ lerp t (V2 x y)

{-|
    Gradually translate an 'Reanimate.Scene.Object' horizontally to a new
    /x/-coordinate.

    This function is best used with 'oTween'.

    For example, @oTween obj 1 $ oMoveToX 3@ gradually moves @obj@ over 1
    second, such that the final /x/-coordinate of @obj@ is 3.
-}
oMoveToX :: Double -> Double -> ObjectData a -> ObjectData a
oMoveToX x t = oTranslate %~ \curr@(V2 _ currY) -> lerp t (V2 x currY) curr

{-|
    Gradually translate an 'Reanimate.Scene.Object' vertically to a new
    /y/-coordinate.

    This function is best used with 'oTween'.

    For example, @oTween obj 1 $ oMoveToY 2@ gradually moves @obj@ over 1
    second, such that the final /y/-coordinate of @obj@ is 2.
-}
oMoveToY :: Double -> Double -> ObjectData a -> ObjectData a
oMoveToY y t = oTranslate %~ \curr@(V2 currX _) -> lerp t (V2 currX y) curr

{-|
    Gradually translate an 'Reanimate.Scene.Object' by the given amount.

    This function is best used with 'oTween'.

    For example, @oTween obj 1 $ oMoveBy (3, 4)@ gradually and simultaneously
    moves @obj@ by 3 units rightward and 4 units upward over 1 second.
-}
oMoveBy :: (Double, Double) -> Double -> ObjectData a -> ObjectData a
oMoveBy (x, y) t = oTranslate +~ lerp t (V2 x y) 0
