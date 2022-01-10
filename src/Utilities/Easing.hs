{-# OPTIONS_GHC -Wall #-}

{-|
    Easing functions.
-}
module Utilities.Easing
    ( easeS
    , easeInS
    , easeInOutS
    , easeOutS
    , snapOutS
    , snapInS
    , softSnapOutS
    , softSnapInS
    , cubicBezierS
    , cubicBezierFullS
    , quarticBezierS
    , quarticBezierFullS
    )
    where

import Control.Lens ((^.))
import Data.List (find)
import Linear (V2 (V2), _x, _y)

import Reanimate hiding (cubicBezierS)

{-|
    Create a cubic Bézier easing function defined by the given 4 points.
-}
cubicBezierFullS :: (V2 Double, V2 Double, V2 Double, V2 Double) -> Signal
cubicBezierFullS (p0, p1, p2, p3) s = maybe s (^. _y) closestP
  where
    ts = (\t -> V2 t t) <$> [0, 1/240 .. 1]
    ps = tToP <$> ts
    tToP t = sum $ zipWith3
        (\p pow coef -> clampX p * coef * t^pow * (1-t)^(3-pow))
        [p0, p1, p2, p3]
        [0 .. 3 :: Int]
        [1, 3, 3, 1]
    closestP = find ((>= s) . (^. _x)) ps
    clamp = min 1 . max 0
    clampX (V2 x y) = V2 (clamp x) y

{-|
    Given 4 numbers @x1@, @y1@, @x2@ and @y2@, create a cubic Bézier easing
    function defined by the 4 points (0, 0), (@x1@, @y1@), (@x2@, @y2@) and
    (1, 1).

    This function is similar to the @cubic-bezier()@ function in CSS.
-}
cubicBezierS :: (Double, Double, Double, Double) -> Signal
cubicBezierS (x1, y1, x2, y2) = cubicBezierFullS
    (V2 0 0, V2 x1 y1, V2 x2 y2, V2 1 1)

{-|
    Create a quartic Bézier easing function defined by the given 5 points.
-}
quarticBezierFullS ::
    (V2 Double, V2 Double, V2 Double, V2 Double, V2 Double)
    -> Signal
quarticBezierFullS (p0, p1, p2, p3, p4) s = maybe s (^. _y) closestP
  where
    ts = (\t -> V2 t t) <$> [0, 1/240 .. 1]
    ps = tToP <$> ts
    tToP t = sum $ zipWith3
        (\p pow coef -> clampX p * coef * t^pow * (1-t)^(4-pow))
        [p0, p1, p2, p3, p4]
        [0 .. 4 :: Int]
        [1, 4, 6, 4, 1]
    closestP = find ((>= s) . (^. _x)) ps
    clamp = min 1 . max 0
    clampX (V2 x y) = V2 (clamp x) y

{-|
    Given 6 numbers @x1@, @y1@, @x2@, @y2@, @x3@ and @y3@, create a quartic
    Bézier easing function defined by the 5 points (0, 0), (@x1@, @y1@), (@x2@,
    @y2@), (@x3@, @y3@) and (1, 1).

    This function is similar to 'cubicBezierS'.
-}
quarticBezierS :: (Double, Double, Double, Double, Double, Double) -> Signal
quarticBezierS (x1, y1, x2, y2, x3, y3) = quarticBezierFullS
    (V2 0 0, V2 x1 y1, V2 x2 y2, V2 x3 y3, V2 1 1)

{-|
    A signal that starts slowly, accelerates sharply, and then slows gradually
    towards the end.

    This signal is the same as @ease@ in CSS.
-}
easeS :: Signal
easeS = cubicBezierS (0.25, 0.1, 0.25, 1.0)


{-|
    A signal that starts slowly, and then progressively speeds up until the
    end, at which point it stops abruptly.

    This signal is the same as @ease-in@ in CSS.
-}
easeInS :: Signal
easeInS = cubicBezierS (0.42, 0.0, 1.0, 1.0)

{-|
    A signal that starts slowly, speeds up, and then slows down towards the
    end.

    This signal is the same as @ease-in-out@ in CSS.
-}
easeInOutS :: Signal
easeInOutS = cubicBezierS (0.42, 0.0, 0.58, 1.0)

{-|
    A signal that starts abruptly, and then progressively slows down towards
    the end.

    This signal is the same as @ease-out@ in CSS.
-}
easeOutS :: Signal
easeOutS = cubicBezierS (0.0, 0.0, 0.58, 1.0)

{-|
    A signal that starts very abruptly, and then progressively slows down
    towards the end.

    See https://gist.github.com/arbalest/2760d2e16b1ce65f0299.
-}
snapOutS :: Signal
snapOutS = cubicBezierS (0.060, 0.975, 0.195, 1)

{-|
    A signal that starts very slowly, and then progressively speeds up until
    the end, at which point it stops abruptly.

    This signal is 'snapOutS' but reversed.
-}
snapInS :: Signal
snapInS = reverseS . snapOutS . reverseS


{-|
    A signal that starts slowly, sharply speeds up, and then slows down towards
    the end.

    This signal is similar to @ease-in-out@ in CSS, but more abrupt.
-}
softSnapOutS :: Signal
softSnapOutS = cubicBezierS (0.25, 0, 0, 1)

{-|
    A signal that starts slowly, gradually speeds up, and then sharply slows
    down towards the end.

    This signal is 'softSnapOutS' but reversed.
-}
softSnapInS :: Signal
softSnapInS = reverseS . softSnapOutS . reverseS
