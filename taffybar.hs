import Data.List (isSuffixOf)
import Data.Monoid ((<>))
import Network.HostName

import System.Taffybar

import System.Taffybar.CommandRunner
import System.Taffybar.FSMonitor
import System.Taffybar.Pager (colorize, escape)
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

-- Insert image icons
icon :: String -> IO Widget
icon f = do
  box <- hBoxNew False 0
  img <- imageNewFromFile $ "/home/adam/dotfiles/" ++ f
  boxPackStart box img PackNatural 0
  widgetShowAll box
  return $ toWidget box

-- Icon Font Handling
alltheicon code = "<span font_family='all-the-icons'> &#x" <> code <> ";</span>"
faicon code = "<span font_family='FontAwesome'> &#x" <> code <> ";</span>"
fileicon code = "<span font_family='file-icons'> &#x" <> code <> ";</span>"
octicon code = "<span font_family='github-octicons'> &#x" <> code <> ";</span>"
wicon code = "<span font_family='Weather Icons'> &#x" <> code <> ";</span>"

haskell = alltheicon "e921"
hddIcon = faicon "f0a0"
mailIcon = octicon "f03b"
calendarIcon = octicon "f068"
commentsIcon = faicon "f086"
emacsIcon = fileicon "e926"
firefoxIcon = faicon "f269"
globeIcon = faicon "f0ac"
rssIcon = faicon "f09e"
pictureIcon = faicon "f03e"
tableIcon = faicon "f0ce"
terminalIcon = faicon "f120"
textFileIcon = faicon "f15c"
verilogIcon = fileicon "e949"
vhdlIcon = fileicon "e9aa"
wifiIcon = faicon "f1eb"

staticLabel :: String -> IO Widget
staticLabel label = do
  l <- labelNew $ Just label -- (Nothing :: Maybe String)
  labelSetMarkup l label
  let w = (toWidget l)
  widgetShowAll w
  return w

workspaceMangler :: String -> String
workspaceMangler "main" = terminalIcon
workspaceMangler "web" = firefoxIcon
workspaceMangler "emacs" = emacsIcon
workspaceMangler "documents" = textFileIcon
workspaceMangler "chat" = commentsIcon
workspaceMangler "media" = pictureIcon
workspaceMangler x = escape x

main = do
  netref <- newIORef [0, 0]
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew defaultPagerConfig {
        activeWorkspace = colorize "yellow" "" . workspaceMangler,
        hiddenWorkspace = workspaceMangler,
        urgentWorkspace = colorize "red" "yellow" . workspaceMangler,
        visibleWorkspace = colorize "orange" "" . workspaceMangler }
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew (defaultWeatherConfig "EGCN"){ weatherTemplate = "$tempC$ C @ $humidity$" } 10
      mem = myPollingBar 5 memCallback
      cpu = myPollingBar 5 cpuCallback
      net = myPollingBar 1 $ netCallback netref 0
      netup = myPollingBar 1 $ netCallback netref 1
      mail = commandRunnerNew 10 "/usr/local/bin/notmuch" ["count","tag:unread"] "Unread Mail" "white"
      tray = systrayNew
  host <- getHostName
  let fsList = myFSList host
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , barHeight = 20
                                        , barPosition = Bottom
                                        , endWidgets = [ tray, wea,
                                                         clock, staticLabel calendarIcon,
                                                         mem, staticLabel verilogIcon,
                                                         cpu, staticLabel vhdlIcon,
                                                         netup, net,
                                                         staticLabel globeIcon] ++
                                                       fsList ++
                                                       [staticLabel hddIcon,
                                                        mail, staticLabel mailIcon,
                                                        note]
                                        }
myFSList :: String -> [IO Widget]
myFSList host
  | ".shef.ac.uk" `isSuffixOf` host = [myFSMonitor "/",
                                       myFSMonitor "/data",
                                       myFSMonitor "/home"]
  | otherwise = [myFSMonitor "/"]
