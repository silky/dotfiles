{-# language OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Bool                                  (bool)
import           Data.Text                                  (Text)
import           Paths_taffy
import           System.Directory                           (doesFileExist)
import           System.Environment.XDG.BaseDir             (getUserConfigFile)
import           System.IO                                  (FilePath)
import           System.Taffybar
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.PollingGraph
import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Layout
import           System.Taffybar.Widget.Util
import           System.Taffybar.Widget.Workspaces
import qualified Data.Maybe as DM
import qualified Data.Text as T
import qualified GI.Gtk


transparent = (0.0, 0.0, 0.0, 0.0)
green1      = (0, 1, 0, 1)
green2      = (1, 0, 1, 0.5)


myGraphConfig :: GraphConfig
myGraphConfig = defaultGraphConfig
  { graphPadding         = 0
  , graphBorderWidth     = 0
  , graphWidth           = 75
  , graphBackgroundColor = transparent
  , graphLabel           = Nothing
  , graphDataColors      = [green1, green2]
  }


cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]


customLayoutTitle :: Text -> Text
customLayoutTitle = text
  where text title | "Tabbed" `T.isPrefixOf` title = highlight "00ff00" (T.drop 7 title)
        text title  = highlight "ff0000" title
        highlight color text = "<span fgcolor='#" <> color <> "'>" <> text <> "</span>"


getResource :: String -> IO (Maybe FilePath)
getResource name = firstJust <$> sequence [xdgResource name, staticResource name]
  where onlyIfExists filePath = bool Nothing (Just filePath) <$> doesFileExist filePath
        xdgResource name    = getUserConfigFile "taffybar" name >>= onlyIfExists
        staticResource name = getDataFileName name >>= onlyIfExists
        firstJust = DM.listToMaybe . DM.catMaybes . filter DM.isJust


-- currentWindow = windowsNew defaultWindowsConfig
layout = layoutNew $ LayoutConfig $ return . customLayoutTitle

workspaces = workspacesNew $ defaultWorkspacesConfig
  { minIcons             = 0
  , widgetGap            = 5
  , showWorkspaceFn      = hideEmpty
  , urgentWorkspaceState = True
  , getWindowIconPixbuf  = scaledWindowIconPixbufGetter getWindowIconPixbufFromEWMH
  }


myConfig :: (Maybe FilePath) -> SimpleTaffyConfig
myConfig cssPath = SimpleTaffyConfig
  { monitorsAction = usePrimaryMonitor
  , barHeight      = 30
  , barPadding     = 0
  , barPosition    = Top
  , widgetSpacing  = 2
  , startWidgets   = map (>>= buildContentsBox) [layout] ++ [workspaces]
  , centerWidgets  = [] -- map (>>= buildContentsBox) [currentWindow]
  , endWidgets     = map (>>= buildContentsBox)
    [ textClockNew Nothing "%a %b %d %I:%M %p" 1
    , pollingGraphNew myGraphConfig 0.5 cpuCallback
    ]
  , cssPath     = cssPath
  , startupHook = return ()
  }


main :: IO ()
main = do
  cssPath <- getResource "style.css"
  startTaffybar $ withLogServer $ withToggleServer $
    toTaffyConfig (myConfig cssPath)
