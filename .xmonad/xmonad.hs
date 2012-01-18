-- xmonad config based on the one by Vic Fryzel
-- Author: Noon Silk
-- Location: ...
-- Original: http://github.com/vicfryzel/xmonad-config
 
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
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspacec
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- myManageHook = composeAll
--     [
--     -- Commented out because I don't care about these for now.
--     --
--     -- className =? "Chromium"       --> doShift "2:web"
--     -- , resource  =? "desktop_window" --> doIgnore
--     -- , className =? "Galculator"     --> doFloat
--     -- , className =? "Gimp"           --> doFloat
--     -- , className =? "Google-chrome"  --> doShift "2:web"
--     -- , resource  =? "gpicview"       --> doFloat
--     -- , resource  =? "kdesktop"       --> doIgnore
--     -- , className =? "MPlayer"        --> doFloat
--     -- , resource  =? "skype"          --> doFloat
--     -- , className =? "VirtualBox"     --> doShift "4:vm"
--     -- , className =? "Xchat"          --> doShift "5:media"
--     isFullscreen --> (doF W.focusDown <+> doFullFloat)]


------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- myLayout = avoidStruts (
--     Tall 1 (3/100) (1/2) |||
--     Mirror (Tall 1 (3/100) (1/2)) |||
--     -- tabbed shrinkText tabConfig |||
--     OneBig (3/4) (3/4) |||
--     mosaic 2 [3,2] |||
--     Full |||
--     spiral (6/7)) |||
--     noBorders (fullscreenFull Full)

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
-- myNormalBorderColor  = "#000000"
-- myFocusedBorderColor = "#e01b4c"


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
 

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()
 

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = xmonad $ defaultConfig {
    borderWidth            = 1
    , terminal             = "/usr/bin/konsole"
    , normalBorderColor    = "#000000"
    , focusedBorderColor   = "#e01b4c"
}
  -- xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
  -- xmonad $ defaults {
  --     -- logHook = dynamicLogWithPP $ xmobarPP {
  --     --       ppOutput = hPutStrLn xmproc
  --     --     , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
  --     --     , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
  --     --     , ppSep = "   "}
  --     -- , manageHook = manageDocks <+> myManageHook
  --     startupHook = setWMName "LG3D"
  -- }
 

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
-- defaults = defaultConfig {
--     -- simple stuff
--     terminal           = myTerminal,
--     focusFollowsMouse  = myFocusFollowsMouse,
--     borderWidth        = myBorderWidth,
--     modMask            = myModMask,
--     workspaces         = myWorkspaces,
--     normalBorderColor  = myNormalBorderColor,
--     focusedBorderColor = myFocusedBorderColor,
 
--     -- key bindings
--     -- keys               = myKeys,
--     mouseBindings      = myMouseBindings,
 
--     -- hooks, layouts
--     layoutHook         = smartBorders $ myLayout,
--     manageHook         = myManageHook,
--     startupHook        = myStartupHook
-- }
