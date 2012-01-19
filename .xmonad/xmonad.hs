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
-- Run xmonad with all the defaults we set up.
--
main = xmonad $ defaultConfig {
    borderWidth            = 1
    , terminal             = "/usr/bin/konsole"
    , normalBorderColor    = "#000000"
    , focusedBorderColor   = "#e01b4c"
}
