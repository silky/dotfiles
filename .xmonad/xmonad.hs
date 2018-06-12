-- XMonad config.
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
--  https://bitbucket.org/tobyodavies/shared/src

import System.IO
import XMonad hiding ( (|||) )
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.Place
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Grid
import XMonad.Layout.ZoomRow
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named(named)
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.CopyWindow
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleWS
import XMonad.Layout.LayoutHints

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


-- Layout Management
--
--  We are interested in changing layouts in two ways. One is the
--  typical iteration with mod-space, the other is specific layout
--  selection through windows-<letter>, where the letter indicates
--  specific layout

myLayout = layoutHints $ smartBorders $ named "C:Tiled" tiled ||| named "C:MTiled" (Mirror tiled)
    ||| noBorders Full
    ||| named "CenteredMaster" (zoomRow)
    --
    -- I don't care about Spiral at the momemnt, but maybe at some point ...
    -- ||| named "C:Spiral" (spiral (3/4))
    ||| named "C:Big" (OneBig (3/4) (3/4))
  where
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

layoutChangeModMask = mod1Mask .|. shiftMask

myKeys   = [
     ((layoutChangeModMask, xK_f), sendMessage $ JumpToLayout "Full")
   , ((layoutChangeModMask, xK_t), sendMessage $ JumpToLayout "C:Tiled")
   , ((layoutChangeModMask, xK_w), sendMessage $ JumpToLayout "C:MTiled")
   , ((layoutChangeModMask, xK_b), sendMessage $ JumpToLayout "C:Big")
   , ((layoutChangeModMask, xK_i), sendMessage $ JumpToLayout "CenteredMaster")
   -- The "Menu" key next to the Windows key
   -- EasyXMotion is courtesy of Loki: https://github.com/loki42/easyxmotion
   -- , ((0, xK_Menu), spawn "/home/noon/bin/easyxmotion.py --colour=#e01b4c --font='-misc-fixed-bold-r-normal--30-0-100-100-c-0-iso8859-15'")
   , ((mod1Mask, xK_i), do
       sendMessage $ JumpToLayout "Full"
       spawn "hide")

   -- Shutting down
   , ((layoutChangeModMask, xK_q), spawn "gksu 'shutdown -h now'")
   , ((layoutChangeModMask, xK_r), spawn "gksu 'shutdown -r now'")
   -- , ((layoutChangeModMask, xK_s), spawn "gksu 'pm-suspend'")
   , ((mod1Mask, xK_o), spawn "nautilus --no-desktop")
   , ((mod1Mask, xK_m), spawn "konsole -e alsamixer")
   , ((mod1Mask, xK_e), spawn "konsole -e nvim")
   , ((mod1Mask, xK_s), spawn "maim -s | xclip -selection clipboard -t image/png")
   , ((mod1Mask .|. shiftMask, xK_s), spawn "maim -s ~/Pictures/Screenshots/$(date +%s).png")
   -- Okay, so this only works on floating windows.
   -- , ((mod1Mask, xK_r), placeFocused (fixed (0,0)))
   -- , ((mod1Mask, xK_f), withFocused float)
   --
   -- Used to copy say VLC to other screens to watch movies
   , ((layoutChangeModMask, xK_v), windows copyToAll)
   , ((layoutChangeModMask, xK_d), killAllOtherCopies)
  ]


-- Toggle the active workspace with the 'Forward/Back' mouse buttons.
myMouseMod = 0
myMouseBindings x = M.fromList $
    [ ((myMouseMod, 8), (\w -> moveTo Prev NonEmptyWS))
    , ((myMouseMod, 9), (\w -> moveTo Next NonEmptyWS))
    -- , ((myMouseMod, 9), (\w -> toggleWS))
    ]


-- Setup
--
main = xmonad $ ewmh defaultConfig {
      borderWidth          = 1
    , terminal             = "/usr/bin/konsole"
    , normalBorderColor    = "#000000"
    , focusedBorderColor   = "#e01b4c"
    , layoutHook           = myLayout
    , mouseBindings        = myMouseBindings
    , modMask              = mod1Mask

    -- Update pointer to be in the center on focus; I tried
    -- it being the 'Nearest' option, but this was not good
    -- because it still contains the bug wherein you shift
    -- to a new window and focus doesn't change.
    --
    -- Note: This breaks in GIMP for various reasons that I
    -- don't care to investigate right now. If you have troubles
    -- there, just comment it out (or fix it and tell me!).
    --
    , logHook              = updatePointer (0.5, 0.5) (0, 0)

} `additionalKeys` myKeys `additionalKeysP` [
      ("M-g", gotoMenu)
    , ("M-b", bringMenu)
    , ("M-0", spawn "notify-send \"`echo \\`date '+%I:%M %p %A, %b %d %Y'\\`\", \"\\`tl\\``\"")
      -- Consider changing these to "Tab+", but it must be that it
      -- doesn't interrupt anything else.
    ]
