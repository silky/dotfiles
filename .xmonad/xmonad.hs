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

-- TODO:
--  - Add i3lock
--
import System.IO
import XMonad hiding ( (|||) )
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Layout.Circle
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.Groups.Examples
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiColumns
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.Spiral
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ZoomRow
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
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
    ||| noBorders Full
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


myStatusBar = "xmobar -x0 /home/noon/dev/dotfiles/xmobar.conf"

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_q)


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
   , ((mod1Mask, xK_q), screenWorkspace 0 >>= flip whenJust (windows . W.view))
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

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

-- sizes
gap    = 10
topbar = 10
border = 0
prompt = 20
status = 20


-- Setup
main = do
  
  -- TODO: https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/src/XMonad.Hooks.DynamicLog.html#xmobar
  -- https://xmobar.org/
  -- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-DynamicLog.html
  -- https://github.com/altercation/dotfiles-tilingwm/tree/31e23a75eebdedbc4336e7826800586617d7d27d
  -- https://github.com/altercation/dotfiles-tilingwm/blob/31e23a75eebdedbc4336e7826800586617d7d27d/.xmonad/xmonad.hs
  -- https://www.reddit.com/r/xmonad/comments/wu2ev/toggle_xmobar_visibility_with_a_hotkey/
  --   (Maybe this is good.)
  -- http://hackage.haskell.org/package/xmobar-0.8/src/README.html
  -- https://www.reddit.com/r/xmonad/comments/fgyzc/xmonadactionsdynamicworkspaces_where_have_you/
  -- https://github.com/polybar/polybarhttps://github.com/polybar/polybar
  -- https://www.reddit.com/r/xmonad/comments/jyoaat/is_it_possible_to_put_a_repl_in_a_prompt/
  -- https://github.com/nwf/xconfig
  -- h <- spawnPipe myStatusBar

  let myConfig = ewmh def {
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
        , logHook            = do
          --dynamicLogWithPP $ def
          --  { ppCurrent             = xmobarColor active "" . wrap "[" "]"
          --  , ppTitle               = xmobarColor active "" . shorten 50
          --  , ppVisible             = xmobarColor base0  "" . wrap "(" ")"
          --  , ppUrgent              = xmobarColor red    "" . wrap " " " "
          --  , ppHiddenNoWindows     = const ""
          --  , ppSep                 = xmobarColor red blue "  :  "
          --  , ppWsSep               = " "
          --  , ppLayout              = xmobarColor yellow ""
          --  , ppOrder               = id
          --  , ppOutput              = hPutStrLn h  
          --  -- , ppSort                = fmap 
          --  --                           (namedScratchpadFilterOutWorkspace.)
          --  --                           (ppSort def)
          --                            --(ppSort defaultPP)
          --  , ppExtras              = [] }
          updatePointer (0.5, 0.5) (0, 0)
      } `additionalKeys` myKeys `additionalKeysP` [
            ("M-g", gotoMenu)
          , ("M-b", bringMenu)
          , ("M-0", spawn "notify-send \"`echo \\`date '+%I:%M %p %A, %b %d %Y'\\``\"")
          ]

  xmonad myConfig
