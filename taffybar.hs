import Data.List (isSuffixOf)
import Network.HostName

import System.Taffybar

import System.Taffybar.CommandRunner
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

import Data.IORef
import System.Process ( readProcess )
import System.Taffybar.Widgets.PollingLabel ( pollingLabelNew )

myPollingBar = pollingBarNew ((defaultBarConfig barColour){barBackgroundColor = const (0,0.169,0.212)})

myFSInfo :: String -> IO Double
myFSInfo fs = do
  fsOut <- readProcess "df" (["-kP", fs]) ""
  let x = (/100) . fromIntegral . read . init . head . drop 1 . reverse . words . last . lines $ fsOut
  return x

myFSMonitor :: String -> IO Widget
myFSMonitor fs = myPollingBar 600 $ myFSInfo fs

memCallback :: IO Double
memCallback = do
  mi <- parseMeminfo
  return $ memoryUsedRatio mi

cpuCallback :: IO Double
cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return totalLoad

netCallback :: IORef ([Integer]) -> Int -> IO Double
netCallback ref idx = do
  old <- readIORef ref
  minfo <- getNetInfo "eno1"
  case minfo of
    Nothing -> return 0
    Just [] -> return 0
    Just xs -> do
      writeIORef ref xs
      return $ (fromIntegral (xs !! idx - old !! idx)) / 30000000

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
  netref <- newIORef [0, 0]
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew (defaultWeatherConfig "EGCN"){ weatherTemplate = "$tempC$ C @ $humidity$" } 10
      mem = myPollingBar 5 memCallback
      cpu = myPollingBar 5 cpuCallback
      net = myPollingBar 1 $ netCallback netref 0
      netup = myPollingBar 1 $ netCallback netref 1
      mail = commandRunnerNew 10 "/usr/local/bin/notmuch" ["count","tag:unread"] "Unread Mail" "white"
      tray = systrayNew
  fsList <- myFSList
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , barHeight = 20
                                        , barPosition = Bottom
                                        , endWidgets = [ tray, wea,
                                                         clock, icon"calendar.xbm",
                                                         mem, icon "mem.xbm",
                                                         cpu, icon "cpu.xbm",
                                                         netup, net,
                                                         icon "net_wired.xbm"] ++
                                                       fsList ++
                                                       [mail,icon "mail.xbm",
                                                        note]
                                        }
myFSList :: IO [IO Widget]
myFSList = do
  host <- getHostName
  if (".shef.ac.uk" `isSuffixOf` host)
    then return [myFSMonitor "/",
                 myFSMonitor "/data",
                 myFSMonitor "/home",
                 icon "diskette.xbm"]
    else return [myFSMonitor "/",
                 icon "diskette.xbm"]
