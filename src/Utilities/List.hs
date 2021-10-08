{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

{-|
    Templates for visualizing lists.

    This module contains 6 templates, named from @list0@ to @list5@, to
    represent lists of different lengths. For each template, list outlines
    (boxes) and element labels are drawn using separate functions. Labels are
    drawn with proper subscripts.

    * @list0@ represents an empty list of the form \(\mathtt{[]}\).
    * @list1@ represents a singleton list of the form \(\mathtt{[x_{0}]}\).
    * @list2@ represents a short list of the form
    \(\mathtt{[x_{0}, ..., x_{n - 1}]}\).
    * @list3@ represents a medium-length list of the form
    \(\mathtt{[x_{0}, x_{1}, ..., x_{n - 1}]}\).
    * @list4@ represents a long list of the form
    \(\mathtt{[x_{0}, x_{1}, ..., x_{n - 2}, x_{n - 1}]}\).
    * @list5@ represents an infinite list of the form
    \(\mathtt{[x_{0}, x_{1}, x_{2}, ...]}\).

    Each drawing function returns a list of SVGs. To combine them into a
    single SVG, use 'mkGroup', e.g. @mkGroup list2Boxes@. Note that the
    combined SVG cannot be reliably ungrouped to recover the original list.
-}
module Utilities.List
    (
    -- * List templates
    -- ** @list0@
      list0Box
    , emptyListBox
    -- ** @list1@
    , list1Box
    , list1Label
    , singletonListBox
    , singletonListLabel
    -- ** @list2@
    , list2Boxes
    , list2Labels
    -- ** @list3@
    , list3Boxes
    , list3Labels
    -- ** @list4@
    , list4Boxes
    , list4Labels
    -- ** @list5@
    , list5Boxes
    , list5Labels
    )
where

import Data.Text (Text)

import Reanimate

import Utilities.Main

{-|
    Create a dashed box with the given color to represent an empty list.
-}
list0Box :: String -> SVG
list0Box color =
    withColor color
    . withDefaultLineStrokeFill
    . withStrokeDashArray [2, 2]
    $ mkRect 0.5 1

{-|
    Alias for 'list0Box'.
-}
emptyListBox :: String -> SVG
emptyListBox = list0Box

{-|
    Create a solid box with the given color to represent a singleton list
    (i.e. a list of length 1).
-}
list1Box :: String -> SVG
list1Box color =
    withColor color
    . withDefaultLineStrokeFill
    $ mkRect 1 1

{-|
    Create a text label from the given color and name to represent a singleton
    list (i.e. a list of length 1).

    For example, @list1Label "green" "y"@ makes a green \(\mathtt{y_{0}}\)
    label.
-}
list1Label :: String -> Text -> SVG
list1Label color name =
    withColor color . withDefaultTextStrokeFill
    . centerX . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale
    $ name <> "\\textsubscript{0}"

{-|
    Alias for 'list1Box'.
-}
singletonListBox :: String -> SVG
singletonListBox = list1Box

{-|
    Alias for 'list1Label'.
-}
singletonListLabel :: String -> Text -> SVG
singletonListLabel = list1Label

list2BoxWidths :: [Double]
list2BoxWidths = [1, 1, 1]

list2Offsets :: [SVG -> SVG]
list2Offsets = (\dx -> translate dx 0) <$> distribute1D list2BoxWidths

{-|
    Create boxes with the given color to represent a list of the form
    \(\mathtt{[x_{0}, ..., x_{n - 1}]}\).
-}
list2Boxes :: String -> [SVG]
list2Boxes color =
    fmap (withColor color . withDefaultLineStrokeFill)
    . zipWith ($) list2Offsets
    . fmap (\w -> mkRect w 1)
    $ list2BoxWidths

{-|
    Create text labels from the given color, name and size to represent a list
    of the form \(\mathtt{[x_{0}, ..., x_{n - 1}]}\).

    For example, @list2Label "green" "y" "m"@ makes 3 green labels:
    \(\mathtt{y_{0}}\), \(\mathtt{...}\) (ellipsis) and \(\mathtt{y_{m - 1}}\).
-}
list2Labels :: String -> Text -> Text -> [SVG]
list2Labels color name size =
    fmap (withColor color . withDefaultTextStrokeFill)
    . zipWith ($) list2Offsets
    . fmap (centerX . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale)
    $
        [ name <> "\\textsubscript{0}"
        , "..."
        , name <> "\\textsubscript{" <> size <> "-1}"
        ]

list3BoxWidths :: [Double]
list3BoxWidths = [1, 1, 2, 1]

list3Offsets :: [SVG -> SVG]
list3Offsets = (\dx -> translate dx 0) <$> distribute1D list3BoxWidths

{-|
    Create boxes with the given color to represent a list of the form
    \(\mathtt{[x_{0}, x_{1}, ..., x_{n - 1}]}\).
-}
list3Boxes :: String -> [SVG]
list3Boxes color =
    fmap (withColor color . withDefaultLineStrokeFill)
    . zipWith ($) list3Offsets
    . fmap (\w -> mkRect w 1)
    $ list3BoxWidths

{-|
    Create text labels from the given color, name and size to represent a list
    of the form \(\mathtt{[x_{0}, x_{1}, ..., x_{n - 1}]}\).

    For example, @list3Label "green" "y" "m"@ makes 4 green labels:
    \(\mathtt{y_{0}}\), \(\mathtt{y_{1}}\), \(\mathtt{...}\) (ellipsis) and
    \(\mathtt{y_{m - 1}}\).
-}
list3Labels :: String -> Text -> Text -> [SVG]
list3Labels color name size =
    fmap (withColor color . withDefaultTextStrokeFill)
    . zipWith ($) list3Offsets
    . fmap (centerX . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale)
    $
        [ name <> "\\textsubscript{0}"
        , name <> "\\textsubscript{1}"
        , "..."
        , name <> "\\textsubscript{" <> size <> "-1}"
        ]

list4BoxWidths :: [Double]
list4BoxWidths = [1, 1, 3, 1, 1]

list4Offsets :: [SVG -> SVG]
list4Offsets = (\dx -> translate dx 0) <$> distribute1D list4BoxWidths

{-|
    Create boxes with the given color to represent a list of the form
    \(\mathtt{[x_{0}, x_{1}, ..., x_{n - 2}, x_{n - 1}]}\).
-}
list4Boxes :: String -> [SVG]
list4Boxes color =
    fmap (withColor color . withDefaultLineStrokeFill)
    . zipWith ($) list4Offsets
    . fmap (\w -> mkRect w 1)
    $ list4BoxWidths

{-|
    Create text labels from the given color, name and size to represent a list
    of the form \(\mathtt{[x_{0}, x_{1}, ..., x_{n - 2}, x_{n - 1}]}\).

    For example, @list4Label "green" "y" "m"@ makes 5 green labels:
    \(\mathtt{y_{0}}\), \(\mathtt{y_{1}}\), \(\mathtt{...}\) (ellipsis),
    \(\mathtt{y_{m - 2}}\) and \(\mathtt{y_{m - 1}}\)
-}
list4Labels :: String -> Text -> Text -> [SVG]
list4Labels color name size =
    fmap (withColor color . withDefaultTextStrokeFill)
    . zipWith ($) list4Offsets
    . fmap (centerX . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale)
    $
        [ name <> "\\textsubscript{0}"
        , name <> "\\textsubscript{1}"
        , "..."
        , name <> "\\textsubscript{" <> size <> "-2}"
        , name <> "\\textsubscript{" <> size <> "-1}"
        ]

list5BoxWidths :: [Double]
list5BoxWidths = [1, 1, 1, 2]

list5Offsets :: [SVG -> SVG]
list5Offsets = (\dx -> translate dx 0) <$> distribute1D list5BoxWidths

{-|
    Create boxes with the given color to represent a list of the form
    \(\mathtt{[x_{0}, x_{1}, x_{2}, ...]}\).
-}
list5Boxes :: String -> [SVG]
list5Boxes color =
    fmap (withColor color . withDefaultLineStrokeFill)
    . zipWith ($) list5Offsets
    . fmap (\w -> mkRect w 1)
    $ list5BoxWidths

{-|
    Create text labels from the given color, name and size to represent a list
    of the form \(\mathtt{[x_{0}, x_{1}, x_{2}, ...]}\).

    For example, @list5Label "green" "y"@ makes 4 green labels:
    \(\mathtt{y_{0}}\), \(\mathtt{y_{1}}\), \(\mathtt{y_{2}}\) and
    \(\mathtt{...}\) (ellipsis).
-}
list5Labels :: String -> Text -> [SVG]
list5Labels color name =
    fmap (withColor color . withDefaultTextStrokeFill)
    . zipWith ($) list5Offsets
    . fmap (centerX . latexCfgCenteredYWith firaMonoCfg withDefaultTextScale)
    $
        [ name <> "\\textsubscript{0}"
        , name <> "\\textsubscript{1}"
        , name <> "\\textsubscript{2}"
        , "..."
        ]
