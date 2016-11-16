import Data.Char (toLower)
import Data.Foldable (foldl')
import Data.List (isSuffixOf, isPrefixOf, isInfixOf)
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
import System.Information.CPU2
import System.Information.Network

import Graphics.UI.Gtk.Display.Image (Image, imageNewFromFile)
import Graphics.UI.Gtk.Abstract.Widget (Widget, toWidget)
import Graphics.UI.Gtk

import Data.IORef
import System.Process ( readProcess )
import System.Taffybar.Widgets.PollingLabel ( pollingLabelNew )

myPollingBar = pollingBarNew ((defaultBarConfig barColour){barBackgroundColor = const (0,0.169,0.212),
                                                           barPadding = 0,
                                                           barWidth = 9})


showAndReturn l = do
  widgetShowAll l
  return $ toWidget l

liveCount :: (Num a, Eq a) => (a -> String) -> IO a -> IO String
liveCount action value = do
  current <- value
  if current == 0
    then return ""
    else return $ action current

myMail :: IO Int
myMail = read <$> readProcess "/usr/local/bin/notmuch" ["count","tag:unread"] ""

myFSInfo :: String -> IO Double
myFSInfo fs = do
  fsOut <- readProcess "df" (["-kP", fs]) ""
  let x = (/100) . fromIntegral . read . init . head . drop 1 . reverse . words . last . lines $ fsOut
  return x

myFSMonitor :: String -> IO Widget
myFSMonitor fs = myPollingBar 6 $ myFSInfo fs

memCallback :: IO Double
memCallback = do
  mi <- parseMeminfo
  return $ memoryUsedRatio mi

-- CPU Charts

cpuCharts :: Int -> [IO Widget]
cpuCharts count = map makeCpuChart [0..count-1]

makeCpuChart :: Int -> IO Widget
makeCpuChart cpu = do
  myPollingBar 5 $ getCPULoad ("cpu" ++ show cpu)  >>= return . sum

cpuCount :: String -> Int
cpuCount host
  | ".shef.ac.uk" `isSuffixOf` host = 8
  | otherwise = 1
------------------------------

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
  showAndReturn box

-- Icon Font Handling
alltheicon code = "<span font_family='all-the-icons'> &#x" <> code <> ";</span>"
faicon code = "<span font_family='FontAwesome'> &#x" <> code <> ";</span>"
fileicon code = "<span font_family='file-icons'> &#x" <> code <> ";</span>"
octicon code = "<span font_family='github-octicons'> &#x" <> code <> ";</span>"
wicon code = "<span font_family='Weather Icons'> &#x" <> code <> ";</span>"

haskellIcon = alltheicon "e921"
googleDriveIcon = alltheicon "e91e"
hddIcon = faicon "f0a0"
mailIcon = octicon "f03b"
arrowsHIcon = faicon "f07e"
arrowsVIcon = faicon "f07d"
calendarIcon = octicon "f068"
commentsIcon = faicon "f086"
emacsIcon = fileicon "e926"
firefoxIcon = faicon "f269"
globeIcon = faicon "f0ac"
listIcon = faicon "f03a"
rssIcon = faicon "f09e"
pictureIcon = faicon "f03e"
squareIcon = faicon "f0c8"
tableIcon = faicon "f0ce"
terminalIcon = faicon "f120"
textFileIcon = faicon "f15c"
thIcon = faicon "f00a"
verilogIcon = fileicon "e949"
vhdlIcon = fileicon "e9aa"
wifiIcon = faicon "f1eb"
youtubeIcon = faicon "F167"
ycombinatorIcon = faicon "F23B"
wikipediaIcon = faicon "F266"
vimeoIcon = faicon "F27D"
twitterIcon = faicon "F099"
twitchIcon = faicon "F1E8"
tumblrIcon = faicon "F173"
tripadvisorIcon = faicon "F262"
trelloIcon = faicon "F181"
trainIcon = faicon "F238"
streetViewIcon = faicon "F21D"
steamIcon = faicon "F1B6"
stackExchangeIcon = faicon "F18D"
stackOverflowIcon = faicon "F16C"
spotifyIcon = faicon "F1BC"
soundcloudIcon = faicon "F1BE"
snapchatIcon = faicon "F2AB"
slideshareIcon = faicon "F1E7"
slackIcon = faicon "F198"
skypeIcon = faicon "F17E"
skyatlasIcon = faicon "F216"
scribdIcon = faicon "F28A"
redditSquareIcon = faicon "F1A2"
redditAlienIcon = faicon "F281"
redditIcon = faicon "F1A1"
pinterestIcon = faicon "F0D2"
paypalIcon = faicon "F1ED"
operaIcon = faicon "F26A"
openidIcon = faicon "F19B"
mixcloudIcon = faicon "F289"
linuxIcon = faicon "F17C"
linkedinIcon = faicon "F0E1"
leanpubIcon = faicon "F212"
leafIcon = faicon "F06C"
lastfmIcon = faicon "F202"
instagramIcon = faicon "F16D"
hackerNewsIcon = faicon "F1D4"
googleWalletIcon = faicon "F1EE"
googleIcon = faicon "F1A0"
googlePlusIcon = faicon "F0D5"
githubIcon = faicon "F09B"
foursquareIcon = faicon "F180"
flickrIcon = faicon "F16E"
facebookOfficialIcon = faicon "F230"
compassIcon = faicon "F14E"
chromeIcon = faicon "F268"
bitbucketIcon = faicon "F171"
appleIcon = faicon "F179"
androidIcon = faicon "F17B"
amazonIcon = faicon "F270"



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

layoutMangler :: String -> String
layoutMangler l
  | "Tall" == l = arrowsVIcon
  | "Mirror Tall" == l = arrowsHIcon
  | "Full" == l = squareIcon
  | "Grid False" == l = thIcon
  | "Tabbed Bottom Simplest" == l = listIcon
  | otherwise = l


windowShortcuts :: [(String, String)]
windowShortcuts = [("- Mozilla Firefox", firefoxIcon),
                   ("Haskell", haskellIcon),
                   ("Google Drive", googleDriveIcon),
                   ("Google Sheets", googleDriveIcon),
                   ("Google Docs", googleDriveIcon),
                   ("YouTube", youtubeIcon),
                   ("- Wikipedia", wikipediaIcon),
                   ("Vimeo", vimeoIcon),
                   ("Twitter", twitterIcon),
                   ("Twitch", twitchIcon),
                   ("TripAdvisor:", tripadvisorIcon),
                   ("Trello", trelloIcon),
                   ("Trainline", trainIcon),
                   ("Google Maps", streetViewIcon),
                   ("Welcome to Steam", steamIcon),
                   ("Steam", steamIcon),
                   ("Stack Exchange", stackExchangeIcon),
                   ("Stack Overflow", stackOverflowIcon),
                   ("Reddit", redditIcon),
                   ("Hacker News", hackerNewsIcon),
                   ("Google+", googlePlusIcon),
                   ("Google Wallet", googleWalletIcon),
                   ("Google", googleIcon),
                   ("Github", githubIcon),
                   ("Facebook", facebookOfficialIcon),
                   ("Apple", appleIcon),
                   ("Amazon", amazonIcon)]

windowMangler :: String -> String
windowMangler w = foldl' mangle w windowShortcuts
  where
    mangle x (before, after) =
      if map toLower before `isInfixOf` map toLower x
      then swap before after x
      else x
    swap _ _ [] = []
    swap before after x =
      if map toLower before `isPrefixOf` map toLower x
      then after ++ (drop (length before) x)
      else head x:(swap before after $ tail x)

main = do
  netref <- newIORef [0, 0]
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew defaultPagerConfig {
        activeLayout = layoutMangler,
        activeWindow = windowMangler . escape,
        activeWorkspace = colorize "#b58900" "" . workspaceMangler,
        hiddenWorkspace = workspaceMangler,
        urgentWorkspace = colorize "#002b36" "#839496" . workspaceMangler,
        visibleWorkspace = colorize "#cb4b16" "" . workspaceMangler,
        widgetSep = " | "}
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew (defaultWeatherConfig "EGCN"){ weatherTemplate = "$tempC$ C @ $humidity$" } 10
      mem = myPollingBar 5 memCallback
      net = myPollingBar 1 $ netCallback netref 0
      netup = myPollingBar 1 $ netCallback netref 1
      tray = systrayNew
  host <- getHostName
  let fsList = myFSList host
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , barHeight = 20
                                        , barPosition = Bottom
                                        , endWidgets = [ tray, wea,
                                                         clock, staticLabel calendarIcon,
                                                         mem, staticLabel verilogIcon] ++
                                                       cpuCharts (cpuCount host) ++
                                                         [staticLabel vhdlIcon,
                                                         netup, net,
                                                         staticLabel globeIcon] ++
                                                       fsList ++
                                                       [staticLabel hddIcon,
                                                        pollingLabelNew "" 10 (liveCount (\x -> mailIcon <> " " <> show x) myMail) >>= showAndReturn,
                                                        note]
                                        }
myFSList :: String -> [IO Widget]
myFSList host
  | ".shef.ac.uk" `isSuffixOf` host = [myFSMonitor "/",
                                       myFSMonitor "/data",
                                       myFSMonitor "/home",
                                       myFSMonitor "/mnt/NAS"]
  | otherwise = [myFSMonitor "/"]
