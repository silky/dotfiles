-- XMonad config, based on Loki Davison's: https://github.com/loki42
-- This is a simple config with a focus on being able to switch between
-- layouts easily.
--
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
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Layout Management
--
--  We are interested in changing layouts in two ways. One is the
--  typical iteration with mod-space, the other is specific layout
--  selection through windows-<letter>, where the letter indicates
--  specific layout

layouts :: [Layout Window]
layouts = [ Layout tiled
          , Layout $ Mirror tiled
          , Layout $ spiral (3/4)
          ]
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = (2/(1 + (toRational(sqrt(5)::Double))))
 
     -- Percent of screen to increment by when resizing panes
     delta   = 2/100


-- myLayout = toggleLayouts Full (Tall 1 (3/100) (1/2))

myKeys   = [
    -- mod4Mask is the windows key.
     ((mod4Mask, xK_f), sendMessage (Toggle "Tall"))
   , ((mod4Mask, xK_g), sendMessage (Toggle "Full"))
   , ((mod4Mask, xK_w), sendMessage (Toggle "mirror Tall"))
  ]  


-- Setup
--
main = xmonad $ defaultConfig {
    borderWidth            = 1
    , terminal             = "/usr/bin/konsole"
    , normalBorderColor    = "#000000"
    , focusedBorderColor   = "#e01b4c"
    , layoutHook           = myLayout
} `additionalKeys` myKeys                                            
