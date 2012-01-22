-- XMonad config, based on Loki Davison's: https://github.com/loki42
-- This is a simple config with a focus on being able to switch between
-- layouts easily.
--
-- Author: Noon Silk
-- Location: http://github.com/silky/dotfiles
--
-- Inspiration:
--  http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey%27s_Config.hs
--  http://www.haskell.org/wikiupload/9/9c/NNoeLLs_Desktop_2011-08-31.png
--  http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Config.html
--
import System.IO
import XMonad hiding ( (|||) )
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
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Layout.Named(named)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


-- Layout Management
--
--  We are interested in changing layouts in two ways. One is the
--  typical iteration with mod-space, the other is specific layout
--  selection through windows-<letter>, where the letter indicates
--  specific layout

myLayout = named "C:Tiled" tiled ||| named "C:MTiled" (Mirror tiled)
    ||| Full ||| named "C:Spiral" (spiral (3/4))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = (2/(1 + (toRational(sqrt(5)::Double))))
 
     -- Percent of screen to increment by when resizing panes
     delta   = 2/100


-- toggleLayouts is some function that takes two layouts,
-- it's sadly not a setting. Oh well. This config doesn't
-- toggle, but at least it's possible to change to specific
-- layouts.

myKeys   = [
    -- mod4Mask is the windows key.
     ((mod4Mask, xK_f), sendMessage $ JumpToLayout "Full")
   , ((mod4Mask, xK_g), sendMessage $ JumpToLayout "C:Tiled")
   , ((mod4Mask, xK_w), sendMessage $ JumpToLayout "C:MTiled")
   , ((mod4Mask, xK_s), sendMessage $ JumpToLayout "C:Spiral")
  ]


-- Setup
--
main = xmonad $ defaultConfig {
    borderWidth            = 1
    , terminal             = "/usr/bin/konsole"
    , normalBorderColor    = "#000000"
    , focusedBorderColor   = "#e01b4c"
    , layoutHook           = myLayout
} `additionalKeys` myKeys `additionalKeysP` [
      ("M-g", gotoMenu)
    , ("M-b", bringMenu)
      -- Some default "goto" operations.
    , ("M-f", runOrRaise "firefox" (className =? "Firefox"))
    , ("M-o", runOrRaise "gvim"    (className =? "Gvim"))
    ]
