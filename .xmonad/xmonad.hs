-- XMonad config, based on Loki Davison's: https://github.com/loki42
-- Author: Noon Silk
-- Location: http://github.com/silky/dotfiles

import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Mosaic
import XMonad.Layout.LayoutCombinators
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Layout Management
--  We are interested in changing layouts in two ways. One is the
--  typical iteration with mod-space, the other is specific layout
--  selection through another-mod-<letter>, where the letter the
--  specific layout

myLayout = toggleLayouts Full (Tall 1 (3/100) (1/2))

-- This does changing between layouts. The mappings are obvious, the
-- potential layouts to consider are:
--  > Full
--  > Tall
--  > ...
--
myKeys = [
    -- mod4Mask is the windows key.
     ((mod4Mask, xK_f), sendMessage (Toggle "Tall"))
   , ((mod4Mask, xK_g), sendMessage (Toggle "Full"))
  ]  


main = xmonad $ defaultConfig {
    borderWidth            = 1
    , terminal             = "/usr/bin/konsole"
    , normalBorderColor    = "#000000"
    , focusedBorderColor   = "#e01b4c"
    , layoutHook           = myLayout
} `additionalKeys` myKeys                                            
