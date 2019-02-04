{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module TwoBig where

import XMonad
import XMonad.Layout.Groups
import XMonad.Layout.Groups.Examples
import XMonad.Layout.StackTile

twoBig = group tiled zoomRowG
  where
    st      = StackTile nmaster delta (8/10)
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = (2/(1 + (toRational (sqrt 5 :: Double))))
    delta   = 2/100


