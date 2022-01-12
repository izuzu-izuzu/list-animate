{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
    \(\LaTeX\) configurations and rendering functions.
-}
module Utilities.LaTeX
    ( defaultRegularTextCfg
    , defaultBoldTextCfg
    , firaMonoCfg
    , plexMonoLightCfg
    , plexMonoMediumCfg
    , latexCfgAlignedYWith
    , latexCfgChunksAlignedYWith
    )
    where

import Data.Text (Text, replace)
import Text.RawString.QQ (r)

import Reanimate
import Reanimate.LaTeX
    ( TexConfig (TexConfig)
    , TexEngine (LaTeX, XeLaTeX)
    , latexCfg
    , latexCfgChunks
    )

import Utilities.Svg (centerGroupY)

{-|
    Default configuration for regular text.
-}
defaultRegularTextCfg :: TexConfig
defaultRegularTextCfg = plexMonoLightCfg

{-|
    Default configuration for bold text.
-}
defaultBoldTextCfg :: TexConfig
defaultBoldTextCfg = plexMonoMediumCfg

{-|
    Enable the Fira Mono font in \(\LaTeX\) glyphs.

    This value should be used with functions starting with @latexCfg@.
-}
firaMonoCfg :: TexConfig
firaMonoCfg = TexConfig
    LaTeX
    [ [r|\usepackage{FiraMono}|]
    , [r|\renewcommand*\familydefault{\ttdefault}|]
    , [r|\usepackage[T1]{fontenc}|]
    ]
    [[r|\normalfont|]]

{-|
    Enable the IBM Plex Mono Light font in \(\LaTeX\) glyphs.

    This value should be used with functions starting with @latexCfg@.
-}
plexMonoLightCfg :: TexConfig
plexMonoLightCfg = TexConfig
    XeLaTeX
    [ [r|\usepackage[T1]{fontenc}|]
    , [r|\usepackage{setspace}|]
    , [r|\usepackage[usefilenames,RMstyle=Light,SSstyle=Light,TTstyle=Light,DefaultFeatures={Ligatures=Common}]{plex-otf} %|]
    , [r|\renewcommand*\familydefault{\ttdefault} %% Only if the base font of the document is to be monospaced|]
    ]
    [[r|\mdseries|]]

{-|
    Enable the IBM Plex Mono Medium font in \(\LaTeX\) glyphs.

    This value should be used with functions starting with @latexCfg@.
-}
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
    Similar to 'latexCfg', but the resulting text glyph is also vertically
    aligned using its baseline, so that different glyphs containing characters
    like \"y\" and \"^\" are properly aligned.

    For now, baseline alignment requires that any SVG transformation (such as
    @scale 0.5@) be given as the second argument.
-}
latexCfgAlignedYWith :: TexConfig -> (SVG -> SVG) -> Text -> SVG
latexCfgAlignedYWith config transformation =
    mkGroup
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
    vertically aligned using their common baseline, so that different glyphs
    containing characters like \"y\" and \"^\" are properly aligned.

    For now, baseline alignment requires that any SVG transformation (such as
    @scale 0.5@) be given as the second argument.
-}
latexCfgChunksAlignedYWith :: TexConfig -> (SVG -> SVG) -> [Text] -> [SVG]
latexCfgChunksAlignedYWith config transformation =
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
