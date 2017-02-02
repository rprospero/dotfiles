{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty)
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Char (toLower)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Foldable (foldl')
import Data.List (isSuffixOf, isPrefixOf, isInfixOf)
import Data.Monoid ((<>))
import Data.Text (unpack)
import Graphics.Icons.AllTheIcons
import Graphics.Icons.FileIcon hiding (appleCode)
import Graphics.Icons.FontAwesome hiding (terminalCode)
import Graphics.Icons.Octicon hiding (terminalCode, calendarCode, globeCode, fileTextCode)
import Graphics.Icons.Types
import Network.Download (openURI)
import Network.HostName
import System.Process
import Text.Printf

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
import System.Taffybar.Widgets.Util

import System.Information.Battery
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
  | "Walter" `isSuffixOf` host = 4
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

wifiConnected :: IO Bool
wifiConnected = (/= "disconnected") . head . words . head . tail . lines <$> readProcess "/usr/bin/nmcli" ["general"] ""

wifiStatus :: IO String
wifiStatus = do
  state <- wifiConnected
  let colour = if state
        then ""
        else "#dc322f"
  return $ colorize colour "" $ iconPango wifiCode

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
wicon code = "<span font_family='Weather Icons'> &#x" <> code <> ";</span>"

rawWeatherIcon :: Int -> Bool -> String
rawWeatherIcon 200 = thunderstormIcon
rawWeatherIcon 201 = thunderstormIcon
rawWeatherIcon 202 = thunderstormIcon
rawWeatherIcon 210 = thunderstormIcon
rawWeatherIcon 211 = thunderstormIcon
rawWeatherIcon 212 = thunderstormIcon
rawWeatherIcon 221 = thunderstormIcon
rawWeatherIcon 230 = thunderstormIcon
rawWeatherIcon 231 = thunderstormIcon
rawWeatherIcon 232 = thunderstormIcon
rawWeatherIcon 300 = drizzleIcon
rawWeatherIcon 301 = drizzleIcon
rawWeatherIcon 302 = drizzleIcon
rawWeatherIcon 310 = drizzleIcon
rawWeatherIcon 311 = drizzleIcon
rawWeatherIcon 312 = drizzleIcon
rawWeatherIcon 313 = drizzleIcon
rawWeatherIcon 314 = drizzleIcon
rawWeatherIcon 321 = drizzleIcon
rawWeatherIcon 500 = rainIcon
rawWeatherIcon 501 = rainIcon
rawWeatherIcon 502 = rainIcon
rawWeatherIcon 503 = rainIcon
rawWeatherIcon 504 = rainIcon
rawWeatherIcon 511 = sleetIcon
rawWeatherIcon 520 = showerIcon
rawWeatherIcon 521 = showerIcon
rawWeatherIcon 522 = showerIcon
rawWeatherIcon 531 = showerIcon
rawWeatherIcon 600 = snowIcon
rawWeatherIcon 601 = snowIcon
rawWeatherIcon 602 = snowIcon
rawWeatherIcon 611 = snowIcon
rawWeatherIcon 612 = snowIcon
rawWeatherIcon 615 = snowIcon
rawWeatherIcon 616 = snowIcon
rawWeatherIcon 620 = snowIcon
rawWeatherIcon 621 = snowIcon
rawWeatherIcon 622 = snowIcon
rawWeatherIcon 701 = mistIcon
rawWeatherIcon 711 = mistIcon
rawWeatherIcon 721 = mistIcon
rawWeatherIcon 731 = mistIcon
rawWeatherIcon 741 = mistIcon
rawWeatherIcon 751 = mistIcon
rawWeatherIcon 761 = mistIcon
rawWeatherIcon 762 = mistIcon
rawWeatherIcon 771 = mistIcon
rawWeatherIcon 781 = mistIcon
rawWeatherIcon 800 = clearIcon
rawWeatherIcon 801 = cloudyIcon
rawWeatherIcon 802 = cloudyIcon
rawWeatherIcon 803 = cloudyIcon
rawWeatherIcon 804 = cloudyIcon
rawWeatherIcon 900 =  const "?"
rawWeatherIcon 901 =  const "?"
rawWeatherIcon 902 =  const "?"
rawWeatherIcon 903 =  const "?"
rawWeatherIcon 904 =  const "?"
rawWeatherIcon 905 =  const "?"
rawWeatherIcon 906 =  const "?"
rawWeatherIcon 951 =  const "?"
rawWeatherIcon 952 =  const "?"
rawWeatherIcon 953 =  const "?"
rawWeatherIcon 954 =  const "?"
rawWeatherIcon 955 =  const "?"
rawWeatherIcon 956 =  const "?"
rawWeatherIcon 957 =  const "?"
rawWeatherIcon 958 =  const "?"
rawWeatherIcon 959 =  const "?"
rawWeatherIcon 960 =  const "?"
rawWeatherIcon 961 =  const "?"
rawWeatherIcon 962 =  const "?"
rawWeatherIcon _ = const "?"

clearIcon True = wicon "F00D"
clearIcon False = wicon "F02E"
cloudyIcon True = wicon "f002"
cloudyIcon False = wicon "f031"
overcastIcon _ = wicon "f013"
thunderstormIcon True = wicon "f010"
thunderstormIcon False = wicon "f03b"
drizzleIcon True = wicon "f00b"
drizzleIcon False = wicon "f039"
rainIcon True = wicon "f008"
rainIcon False = wicon "f036"
showerIcon True = wicon "f009"
showerIcon False = wicon "f037"
snowIcon True = wicon "f00a"
snowIcon False = wicon "f038"
sleetIcon True = wicon "f0b2"
sleetIcon False = wicon "f0b4"
mistIcon _ = wicon "f014"

---------------  Battery Icon Code

myBatteryInfo :: IO (Maybe BatteryInfo)
myBatteryInfo = do
  ctx <- batteryContextNew
  case ctx of
    Nothing -> return Nothing
    Just c -> getBatteryInfo c

batteryIcon :: IO String
batteryIcon = do
  minfo <- myBatteryInfo
  case minfo of
    Nothing -> return ""
    Just info -> case batteryState info of
      BatteryStateCharging -> return $ iconPango batteryChargingCode
      BatteryStateFullyCharged -> return $ iconPango batteryChargingCode
      BatteryStateDischarging -> return $ appropriateBattery info
      _ -> return $ iconPango batteryEmptyCode

batteryTime :: IO String
batteryTime = do
  minfo <- myBatteryInfo
  case minfo of
    Nothing -> return "Charged"
    Just info -> case batteryState info of
      BatteryStateCharging -> return $ secondsToTime (batteryTimeToFull info) <> " Till Full"
      BatteryStateDischarging -> return $ secondsToTime (batteryTimeToEmpty info) <> " Till Empty"
      BatteryStateFullyCharged -> return "Charged"
      x -> return $ show x

batteryWidget :: Double -> IO Widget
batteryWidget update = do
  base <- pollingLabelNew "" update batteryIcon
  child <- pollingLabelNew "" update batteryTime
  clickWidget base child

secondsToTime :: (Integral a, Show a, PrintfArg a) => a -> String
secondsToTime x = show hours <> ":" <> printf "%02d" minutes
  where
    hours = x `div` 3600
    minutes = (x `mod` 3600) `div` 60

appropriateBattery :: BatteryInfo -> String
appropriateBattery x
  | batteryPercentage x < 20.0 = colorize "#dc322f" "" $ iconPango batteryEmptyCode
  | batteryPercentage x < 40.0 = colorize "#cb4b16" "" $ iconPango batteryQuarterCode
  | batteryPercentage x < 60.0 = colorize "#b58900" "" $ iconPango batteryHalfCode
  | batteryPercentage x < 80.0 = iconPango batteryThreeQuartersCode
  | otherwise = iconPango batteryFullCode

-- Weather Applet

data Weather = Weather {
  weatherCoord :: Coord,
  weatherStates :: [WeatherState],
  weatherBase :: String,
  weatherMain :: WeatherStats,
  weatherDt :: Int,
  -- weatherSys :: WeatherSys
  weatherId :: Int,
  weatherName :: String
  }
  deriving (Show, Eq)

instance FromJSON Weather where
  parseJSON (Object v) = Weather <$>
    v .: "coord" <*>
    v .: "weather" <*>
    v .: "base" <*>
    v .: "main" <*>
    v .: "dt" <*>
    v .: "id" <*>
    v .: "name"

data WeatherStats = WeatherStats {
  wsTemp :: Double,
  wsTempMin :: Double,
  wsTempMax :: Double,
  wsPressure :: Int,
  wsHumidity :: Int
  }
  deriving (Show, Eq)

instance FromJSON WeatherStats where
  parseJSON (Object v) = WeatherStats <$>
    v .: "temp" <*>
    v .: "temp_min" <*>
    v .: "temp_max" <*>
    v .: "pressure" <*>
    v .: "humidity"

data WeatherState = WeatherState {
  wsId :: Int,
  wsMain :: String,
  wsDescription :: String,
  wsIcon :: String
  }
  deriving (Show, Eq)

instance FromJSON WeatherState where
  parseJSON (Object v) = WeatherState <$>
    v .: "id" <*>
    v .: "main" <*>
    v .: "description" <*>
    v .: "icon"

data Coord = Coord {
  coordLat :: Double,
  coordLong :: Double
  }
  deriving (Show, Eq)

instance FromJSON Coord where
  parseJSON (Object v) = Coord <$>
    v .: "lat" <*>
    v .: "lon"
  parseJSON _ = empty

getWeather :: ByteString -> Maybe Weather
getWeather = decode . fromStrict

localWeather :: String -> IO (Either String Weather)
localWeather city = do
  text <- openURI $ "http://api.openweathermap.org/data/2.5/weather?q=" <> city <> "&appid=932841262ade6126df76f1196989509e"
  return $ case text of
    Left error -> Left error
    Right body -> case getWeather body of
      Nothing -> Left $ "Cannot parse weather: " <> take 20 (show text)
      Just w -> Right w

weatherIcon :: Weather -> String
weatherIcon w =
  case weatherStates w of
    [] -> "No Weather"
    x:_ ->
      let daytime = (== 'd') . head . reverse . wsIcon $ x
      in flip rawWeatherIcon daytime $ wsId x

weather :: String -> IO String
weather city = do
  wea <- localWeather city
  case wea of
    Left err -> return err
    Right w -> return $ weatherIcon w

--  Widget Utilities

clickWidget :: Widget -> Widget -> IO Widget
clickWidget base child = do
  ebox <- eventBoxNew
  containerAdd ebox base
  eventBoxSetVisibleWindow ebox False
  window <- makeWindow $ return child
  _ <- on ebox buttonPressEvent $ onClick [SingleClick] (toggleChild "" base window)
  widgetShowAll ebox
  return (toWidget ebox)

clickCommand :: Widget -> IO () -> IO Widget
clickCommand base command = do
  ebox <- eventBoxNew
  containerAdd ebox base
  eventBoxSetVisibleWindow ebox False
  _ <- on ebox buttonPressEvent $ onClick [SingleClick] command
  widgetShowAll ebox
  return (toWidget ebox)

makeWindow :: (WidgetClass w) => IO w -> IO Window
makeWindow window = do
  container <- windowNew
  win <- window
  containerAdd container win
  -- _ <- onShow container $ liftIO $ resetCalendarDate cal
  _ <- on container deleteEvent $ do
    liftIO (widgetHideAll container)
    return True
  return container

toggleChild :: WidgetClass w => String -> w -> Window -> IO Bool
toggleChild title widget child = do
  isVis <- get child widgetVisible
  if isVis
    then widgetHideAll child
    else do
      attachPopup widget title child
      displayPopup widget child
  return True

staticLabel :: String -> IO Widget
staticLabel label = do
  l <- labelNew $ Just label -- (Nothing :: Maybe String)
  labelSetMarkup l label
  let w = (toWidget l)
  widgetShowAll w
  return w

staticIcon :: IconCode a => a -> IO Widget
staticIcon = staticLabel . iconPango

workspaceMangler :: String -> String
workspaceMangler "main" = iconPango terminalCode
workspaceMangler "web" = iconPango firefoxCode
workspaceMangler "emacs" = iconPango emacsCode
workspaceMangler "documents" = iconPango fileTextCode
workspaceMangler "chat" = iconPango commentsCode
workspaceMangler "media" = iconPango pictureOCode
workspaceMangler x = escape x

layoutMangler :: String -> String
layoutMangler l
  | "Tall" == l = iconPango arrowsVCode
  | "Mirror Tall" == l = iconPango arrowsHCode
  | "Full" == l = iconPango squareCode
  | "Grid False" == l = iconPango thCode
  | "Tabbed Bottom Simplest" == l = iconPango listCode
  | otherwise = l


windowShortcuts :: [(String, String)]
windowShortcuts = [("- Mozilla Firefox", iconPango firefoxCode),
                   ("Haskell", iconPango haskellCode),
                   ("Google Drive", iconPango googleDriveCode),
                   ("Google Sheets", iconPango googleDriveCode),
                   ("Google Docs", iconPango googleDriveCode),
                   ("YouTube", iconPango youtubeCode),
                   ("- Wikipedia", iconPango wikipediaWCode),
                   ("Vimeo", iconPango vimeoCode),
                   ("Twitter", iconPango twitterCode),
                   ("Twitch", iconPango twitchCode),
                   ("TripAdvisor:", iconPango tripadvisorCode),
                   ("Trello", iconPango trelloCode),
                   ("Trainline", iconPango trainCode),
                   ("Google Maps", iconPango streetViewCode),
                   ("Welcome to Steam", iconPango steamCode),
                   ("Steam", iconPango steamCode),
                   ("Stack Exchange", iconPango stackExchangeCode),
                   ("Stack Overflow", iconPango stackOverflowCode),
                   ("Reddit", iconPango redditCode),
                   ("Hacker News", iconPango hackerNewsCode),
                   ("Google+", iconPango googlePlusCode),
                   ("Google Wallet", iconPango googleWalletCode),
                   ("Google", iconPango googleCode),
                   ("Github", iconPango githubCode),
                   ("Facebook", iconPango facebookOfficialCode),
                   ("Apple", iconPango appleCode),
                   ("Amazon", iconPango amazonCode)]

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
        activeWindow = windowMangler . escape . take 40,
        activeWorkspace = colorize "#859900" "" . workspaceMangler,
        hiddenWorkspace = colorize "#268bd2" "" . workspaceMangler,
        urgentWorkspace = colorize "#002b36" "#268bd2" . workspaceMangler,
        visibleWorkspace = colorize "#2aa198" "" . workspaceMangler,
        widgetSep = " | "}
      note = notifyAreaNew defaultNotificationConfig
      wea = pollingLabelNew "waiting" 10 (weather "Didcot") >>= showAndReturn
      mem = myPollingBar 5 memCallback
      net = myPollingBar 1 $ netCallback netref 0
      netup = myPollingBar 1 $ netCallback netref 1
      tray = systrayNew
  -- wifiWidget <- pollingLabelNew "" 5 wifiStatus >>= showAndReturn
  -- let wifi = clickCommand wifiWidget $ callProcess "/usr/bin/urxvt" ["-e", "/usr/bin/nmtui"]
  host <- getHostName
  let fsList = myFSList host
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , barHeight = 20
                                        , barPosition = Bottom
                                        , endWidgets = [ tray,
                                                         batteryWidget 5.0, wea,
                                                         clock, staticIcon calendarCode,
                                                         mem, staticIcon verilogCode] ++
                                                       cpuCharts (cpuCount host) ++
                                                         [staticIcon vhdlCode,
                                                         netup, net,
                                                         staticIcon globeCode] ++
                                                       fsList ++
                                                       [staticIcon hddOCode,
                                                        pollingLabelNew "" 10 (liveCount (\x -> colorize "#fdf6e3" "" (iconPango mailCode) <> " " <> show x) myMail) >>= showAndReturn,
                                                        note]
                                        }
myFSList :: String -> [IO Widget]
myFSList host
  | ".shef.ac.uk" `isSuffixOf` host = [myFSMonitor "/",
                                       myFSMonitor "/data",
                                       myFSMonitor "/home",
                                       myFSMonitor "/mnt/NAS"]
  | otherwise = [myFSMonitor "/"]
