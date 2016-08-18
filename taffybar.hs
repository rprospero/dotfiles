import Data.List (isSuffixOf)
import Network.HostName

import System.Taffybar

import System.Taffybar.FSMonitor
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.MPRIS

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU
import System.Information.Network

import Graphics.UI.Gtk.Display.Image (Image, imageNewFromFile)
import Graphics.UI.Gtk.Abstract.Widget (Widget, toWidget)
import Graphics.UI.Gtk


import System.Process ( readProcess )
import System.Taffybar.Widgets.PollingLabel ( pollingLabelNew )

myFSInfo :: String -> IO Double
myFSInfo fs = do
  fsOut <- readProcess "df" (["-kP", fs]) ""
  let x = (/100) . fromIntegral . read . init . head . drop 1 . reverse . words . last . lines $ fsOut
  return x

myFSMonitor :: String -> IO Widget
myFSMonitor fs = pollingBarNew (defaultBarConfig barColour) 600 $ myFSInfo fs

memCallback :: IO Double
memCallback = do
  mi <- parseMeminfo
  return $ memoryUsedRatio mi

cpuCallback :: IO Double
cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return totalLoad

netCallback :: IO Double
netCallback = do
  minfo <- getNetInfo "eno1"
  return $ case minfo of
             Nothing -> 0
             Just [] -> 0
             Just xs -> 0 -- (fromIntegral $ head xs) / 1000000000

barColour x
  | x < 1.0/3.0 = (0,3.0*x,0)
  | x < 2.0/3.0 = (3*x-1,1,0)
  | otherwise = (1,abs (3-3*x),0)

icon :: String -> IO Widget
icon f = do
  box <- hBoxNew False 0
  img <- imageNewFromFile $ "/home/adam/dotfiles/" ++ f
  boxPackStart box img PackNatural 0
  widgetShowAll box
  return $ toWidget box

main = do
  let memCfg = defaultBarConfig barColour
      cpuCfg = defaultBarConfig barColour
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew (defaultWeatherConfig "EGCN"){ weatherTemplate = "$tempC$ C @ $humidity$" } 10
      mem = pollingBarNew memCfg 5 memCallback
      cpu = pollingBarNew cpuCfg 5 cpuCallback
      net = pollingBarNew cpuCfg 1 netCallback
      tray = systrayNew
  fsList <- myFSList
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , barHeight = 20
                                        , barPosition = Bottom
                                        , endWidgets = [ tray, wea,
                                                         clock, icon"calendar.xbm",
                                                         mem, icon "mem.xbm",
                                                         cpu, icon "cpu.xbm",
                                                         net, icon "net_wired.xbm"] ++ fsList ++ [note]
                                        }
myFSList :: IO [IO Widget]
myFSList = do
  host <- getHostName
  if (".sheffield.ac.uk" `isSuffixOf` host)
    then return [myFSMonitor "/",
                 myFSMonitor "/data",
                 myFSMonitor "/home",
                 icon "diskette.xbm"]
    else return [myFSMonitor "/",
                 icon "diskette.xbm"]
