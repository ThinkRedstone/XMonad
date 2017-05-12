import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.Pager
import Text.Printf
import System.Taffybar.Battery
import System.Taffybar.WorkspaceSwitcher

redForeground = colorize "#ff0000" "#000000"

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
      battery = batteryBarNew defaultBatteryConfig 10
      batteryTime = textBatteryNew "| $time$" 10
  defaultTaffybar defaultTaffybarConfig { startWidgets = [wss, battery, batteryTime]
                                        , endWidgets = [clock, tray, mpris ]
                                        , barPosition = Bottom
                                        , barHeight = 30
                                        }