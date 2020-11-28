-- XMonad config.
-- This is a simple config with a focus on being able to switch between
-- layouts easily.
--
-- Author: Noon van der Silk
-- Location: http://github.com/silky/dotfiles
--
-- Inspiration:
--  http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey%27s_Config.hs
--  http://www.haskell.org/wikiupload/9/9c/NNoeLLs_Desktop_2011-08-31.png
--  http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Config.html
--  https://bitbucket.org/tobyodavies/shared/src
--  https://betweentwocommits.com/blog/xmonad-layouts-guide

import System.IO
import XMonad hiding ( (|||) )
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.Place
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Circle
import XMonad.Layout.OneBig
import XMonad.Layout.Grid
import XMonad.Layout.ZoomRow
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named (named)
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.CopyWindow
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWS
import XMonad.Layout.Groups.Examples
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiColumns
import TwoBig

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


-- Layout Management
--
--  We are interested in changing layouts in two ways. One is the
--  typical iteration with mod-space, the other is specific layout
--  selection through windows-<letter>, where the letter indicates
--  specific layout


myLayout = layoutHints $ smartBorders $ 
        named "Tiled"          tiled 
    ||| named "MTiled"         (Mirror tiled)
    ||| named "CenteredMaster" (zoomRow)
    ||| named "TallCols"       (Mirror $ multiCol [1] 1 0.01 (0.5))
    ||| named "Circle"         Circle
    ||| named "Big"            (OneBig (3/4) (3/4))
    -- ||| named "C:Circle" rowOfColumns
  where
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = (2/(1 + (toRational (sqrt 5 :: Double))))

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100


-- toggleLayouts is some function that takes two layouts,
-- it's sadly not a setting. Oh well. This config doesn't
-- toggle, but at least it's possible to change to specific
-- layouts.


layoutChangeModMask = mod1Mask .|. shiftMask


myKeys   = 
   [ ((layoutChangeModMask, xK_f), sendMessage $ JumpToLayout "Full")
   , ((layoutChangeModMask, xK_t), sendMessage $ JumpToLayout "Tiled")
   , ((layoutChangeModMask, xK_w), sendMessage $ JumpToLayout "MTiled")
   , ((layoutChangeModMask, xK_b), sendMessage $ JumpToLayout "Big")
   , ((layoutChangeModMask, xK_i), sendMessage $ JumpToLayout "Circle")
   --
   , ((mod1Mask, xK_o), spawn "nautilus --no-desktop")
   , ((mod1Mask, xK_m), spawn "konsole -e alsamixer")
   , ((mod1Mask, xK_e), spawn "konsole -e nvim")
   , ((mod1Mask, xK_r), screenWorkspace 1 >>= flip whenJust (windows . W.view))
   --
   -- Flameshot: <https://github.com/lupoDharkael/flameshot>
   -- 
   , ((mod1Mask, xK_s), spawn "flameshot gui")
   , ((mod1Mask .|. shiftMask, xK_s), spawn "flameshot gui -p ~/Pictures/Screenshots/")
   --
   -- Used to copy say VLC to other screens to watch movies
   , ((layoutChangeModMask, xK_v), windows copyToAll)
   , ((layoutChangeModMask, xK_d), killAllOtherCopies)
   -- 
   -- Increase the size occupied by the focused window
   --
   , ((layoutChangeModMask, xK_plus),  sendMessage zoomIn)
   , ((layoutChangeModMask, xK_minus), sendMessage zoomOut)
  ]


-- Toggle the active workspace with the 'Forward/Back' mouse buttons.
myMouseMod = 0
myMouseBindings x = M.fromList $
    [ ((myMouseMod, 8), const $ moveTo Prev NonEmptyWS)
    , ((myMouseMod, 9), const $ moveTo Next NonEmptyWS)
    ]


-- Setup
main = xmonad $ ewmh def {
      borderWidth        = 1
    , terminal           = "/usr/bin/konsole"
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#b141f2"
    , layoutHook         = myLayout
    , mouseBindings      = myMouseBindings
    , modMask            = mod1Mask

    -- Update pointer to be in the center on focus; I tried
    -- it being the 'Nearest' option, but this was not good
    -- because it still contains the bug wherein you shift
    -- to a new window and focus doesn't change.
    --
    -- Note: This breaks in GIMP for various reasons that I
    -- don't care to investigate right now. If you have troubles
    -- there, just comment it out (or fix it and tell me!).
    --
    , logHook            = updatePointer (0.5, 0.5) (0, 0)

} `additionalKeys` myKeys `additionalKeysP` [
      ("M-g", gotoMenu)
    , ("M-b", bringMenu)
    , ("M-0", spawn "notify-send \"`echo \\`date '+%I:%M %p %A, %b %d %Y'\\``\"")
    ]
