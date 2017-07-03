import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS

import System.Taffybar.Widgets.PollingLabel
import qualified Graphics.UI.Gtk as Gtk
import System.Process

import System.Taffybar.Pager
import Text.Printf

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Taffybar.WorkspaceSwitcher

redForeground = colorize "#ff0000" "#000000"

renderLabel pollingLabel = do
        label <- pollingLabel
        Gtk.widgetShowAll label
        return $ Gtk.toWidget label

main = do
  pager <- pagerNew defaultPagerConfig {  emptyWorkspace = wrap " " " " . colorize "#404040" "#000000"
                                        , activeWorkspace = wrap (redForeground "[") (redForeground "]")
                                        , hiddenWorkspace = wrap " " " "}
  let clock = textClockNew Nothing "<span fgcolor='grey'>%a %b %_d %H:%M</span>" 1
      mpris = mprisNew MPRISConfig {trackLabel = display}
        where artist track  = maybe "[unknown]" id (trackArtist track)
              title  track  = maybe "[unknown]" id (trackTitle  track)
              display :: TrackInfo -> String
              display track = printf "%s - %s" (artist track) (title track)
      tray = systrayNew
      wss = wspaceSwitcherNew pager
      layoutIndicator = renderLabel $ pollingLabelNew "err" 0.1 (readProcess "xkblayout-state" ["print", "%s"] [])
  defaultTaffybar defaultTaffybarConfig { startWidgets = [wss, layoutIndicator]
                                        , endWidgets = [clock, tray, mpris ]
                                        , barPosition = Bottom
                                        , barHeight = 30
                                        }