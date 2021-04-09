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

-- https://github.com/kmarzic/dotfiles/blob/master/.xmonad/xmonad.ansi.hs

import Data.List.Split (splitOn)
import Data.Maybe (isJust)
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
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


myLayout = layoutHints $ smartBorders $ 
        named "Tiled"          tiled 
    ||| named "MTiled"         (Mirror tiled)
    ||| named "CenteredMaster" (zoomRow)
    ||| noBorders Full
    ||| named "TallCols"       (Mirror $ multiCol [1] 1 0.01 (0.5))
    ||| named "Circle"         Circle
    ||| named "Big"            (OneBig (3/4) (3/4))
  where
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = (2/(1 + (toRational (sqrt 5 :: Double))))

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100


layoutChangeModMask = mod1Mask .|. shiftMask


myKeys conf = 
   [ ((layoutChangeModMask, xK_f), sendMessage $ JumpToLayout "Full")
   , ((layoutChangeModMask, xK_t), sendMessage $ JumpToLayout "Tiled")
   , ((layoutChangeModMask, xK_w), sendMessage $ JumpToLayout "MTiled")
   , ((layoutChangeModMask, xK_b), sendMessage $ JumpToLayout "Big")
   --
   , ((mod1Mask, xK_o), spawn "nautilus --no-desktop")
   , ((mod1Mask, xK_m), spawn "konsole -e alsamixer")
   , ((mod1Mask, xK_e), spawn "konsole -e nvim")
   , ((mod1Mask, xK_p), spawn "dmenu_run -nb '#ffefd1' -sf '#b141e5' -nf '#333333' -sb '#ffefd1'")
   --
   -- Show a random image full-screen
   , ((mod1Mask, xK_i), spawn "feh -Z -. --randomize --image-bg black /home/noon/images"
                        >> sendMessage (JumpToLayout "Full")
     )
   --
   , ((layoutChangeModMask, xK_i), spawn "feh -. -x -q -D 300 -B black -F -Z -z -r /home/noon/slideshow-images"
                        >> sendMessage (JumpToLayout "Full")
     )
   -- 
   -- Move mouse focus to the other screen; useful for more a setup with more
   -- than one screen
   , ((mod1Mask, xK_q), screenWorkspace 0 >>= flip whenJust (windows . W.view))
   , ((mod1Mask, xK_r), screenWorkspace 1 >>= flip whenJust (windows . W.view))
   --
   -- Flameshot: <https://github.com/lupoDharkael/flameshot>
   , ((mod1Mask, xK_s), spawn "flameshot gui")
   --
   , ((mod1Mask .|. shiftMask, xK_s), swapScreen) 
  ]
  ++ 
  [ ((mod1Mask .|. e, k), windows $ onCurrentScreen f i)
      | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
      , (f, e) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]


-- https://stackoverflow.com/questions/33547168/xmonad-combine-dwm-style-workspaces-per-physical-screen-with-cycling-function
swapScreen =  do
  x <- currentScreen
  y <- gets (W.tag . W.workspace . W.current . windowset)

  -- HACK: Relies on details of `XMonad.Layout.IndependentScreens` ...
  let [left,right] = splitOn "_" y
      toggle "0"   = "1"
      toggle "1"   = "0"

  windows $ W.shift (toggle left ++ "_" ++ right)


-- Toggle the active workspace with the 'Forward/Back' mouse buttons.
myMouseMod = 0
myMouseBindings x = M.fromList $
    [ ((myMouseMod, 8), const $ moveTo Prev nonEmptySpacesOnCurrentScreen)
    , ((myMouseMod, 9), const $ moveTo Next nonEmptySpacesOnCurrentScreen)
    ]

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

nonEmptySpacesOnCurrentScreen :: WSType
nonEmptySpacesOnCurrentScreen = WSIs $ do
  s <- currentScreen
  return $ \x -> isJust (W.stack x) && isOnScreen s x

main :: IO ()
main = do
  nScreens <- countScreens
  let myConfig = ewmh def {
          borderWidth        = 1
        , workspaces         = withScreens nScreens (workspaces def)
        , terminal           = "/usr/bin/konsole"
        , normalBorderColor  = "#000000"
        , focusedBorderColor = "#b141f2"
        , layoutHook         = avoidStruts myLayout
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
        , logHook            = updatePointer (0.5, 0.5) (0, 0)
      } `additionalKeys'` myKeys 

  xmonad $ docks myConfig


additionalKeys' :: XConfig a -> (XConfig a -> [((KeyMask, KeySym), X ())]) -> XConfig a
additionalKeys' conf keyList =
    conf { keys = \cnf -> M.union (M.fromList (keyList conf)) (keys conf cnf) }

