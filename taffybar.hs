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
import Data.String.Utils (replace, join)
import Data.Text (unpack)
import Graphics.Icons.AllTheIcons
import Graphics.Icons.FileIcon hiding (appleCode)
import Graphics.Icons.FontAwesome hiding (terminalCode)
import Graphics.Icons.Octicon hiding (terminalCode, calendarCode, globeCode, fileTextCode)
import Graphics.Icons.Types
import Graphics.Icons.Weather hiding (trainCode)
import Network.Download (openURI)
import Network.HostName
import System.Process
import Text.Parsec (Parsec, anyChar, choice, eof, many, many1, parse, skipMany1, string, try)
import Text.Parsec.Char (noneOf, char)
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

-- CPU Widget

cpuWidget :: String -> Double -> [IO Widget]
cpuWidget host update = map go $ cpuCharts (cpuCount host)
  where
    go chart = do
      -- base <- pollingLabelNew "" update batteryIcon
      base <- chart
      child <- pollingLabelNew "" update batteryTime >>= showAndReturn
      clickWidget base child

cpuHog :: IO String
cpuHog = readProcess "ps" ["aux","--sort","%cpu"] "" >>= return . unwords . drop 10 . words . last . lines

memHog :: IO String
memHog = readProcess "ps" ["aux","--sort","%mem"] "" >>= return . unwords . drop 10 . words . last . lines

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

clearIcon True = iconPango forecastIoClearDayCode
clearIcon False = iconPango forecastIoClearNightCode
cloudyIcon True = iconPango dayCloudyCode
cloudyIcon False = iconPango nightAltCloudyCode
overcastIcon _ = iconPango cloudyCode
thunderstormIcon True = iconPango dayThunderstormCode
thunderstormIcon False = iconPango nightAltThunderstormCode
drizzleIcon True = iconPango daySprinkleCode
drizzleIcon False = iconPango nightAltSprinkleCode
rainIcon True = iconPango dayRainCode
rainIcon False = iconPango nightAltRainCode
showerIcon True = iconPango dayShowersCode
showerIcon False = iconPango nightAltShowersCode
snowIcon True = iconPango daySnowCode
snowIcon False = iconPango nightAltSnowCode
sleetIcon True = iconPango daySleetCode
sleetIcon False = iconPango nightAltSleetCode
mistIcon _ = iconPango fogCode

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

weatherDesc :: Weather -> String
weatherDesc w =
  let
    temp = show . round . (+ (-273.15)) . wsTemp . weatherMain $ w
    desc = join ", " . map wsDescription . weatherStates $ w
  in
    temp ++ iconPango celsiusCode ++ " " ++ desc

redErr :: Either String String -> String
redErr (Left err) = colorize "#dc322f" "" err
redErr (Right value) = value

weatherConditions :: String -> IO String
weatherConditions w = do
  wea <- localWeather w
  return . redErr $ weatherDesc <$> wea

weather :: String -> IO String
weather city = do
  wea <- localWeather city
  return . redErr $ weatherIcon <$> wea

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
  | "SmartSpacing " `isPrefixOf` l = layoutMangler . unwords . drop 2 . words $ l
  | "Tall" == l = iconPango arrowsVCode
  | "Mirror Tall" == l = iconPango arrowsHCode
  | "Full" == l = iconPango squareCode
  | "Grid False" == l = iconPango thCode
  | "Tabbed Bottom Simplest" == l = iconPango listCode
  | otherwise = l


windowShortcuts :: [Parsec String () String]
windowShortcuts = [
                   string "- Mozilla Firefox" >> return (iconPango firefoxCode),
                   string "Haskell" >> return (iconPango haskellCode),
                   string "Google Drive" >> return (iconPango googleDriveCode),
                   string "Google Sheets" >> return (iconPango googleDriveCode),
                   string "Google Docs" >> return (iconPango googleDriveCode),
                   string "YouTube" >> return (iconPango youtubeCode),
                   string "- Wikipedia" >> return (iconPango wikipediaWCode),
                   string "Vimeo" >> return (iconPango vimeoCode),
                   string "Twitter" >> return (iconPango twitterCode),
                   string "Twitch" >> return (iconPango twitchCode),
                   string "TripAdvisor:" >> return (iconPango tripadvisorCode),
                   string "Trello" >> return (iconPango trelloCode),
                   string "Trainline" >> return (iconPango trainCode),
                   string "*Slack - Channel*" >> return (iconPango slackCode),
                   string "Slack" >> return (iconPango slackCode),
                   string "Python" >> return (iconPango pythonCode),
                   string "Google Maps" >> return (iconPango streetViewCode),
                   string "Welcome to Steam" >> return (iconPango steamCode),
                   string "Steam" >> return (iconPango steamCode),
                   string "Stack Exchange" >> return (iconPango stackExchangeCode),
                   string "Stack Overflow" >> return (iconPango stackOverflowCode),
                   string "Skype" >> return (iconPango skypeCode),
                   string "Reddit" >> return (iconPango redditCode),
                   string "Jenkins" >> return (iconPango jenkinsCode),
                   string "Inbox " >> return "📧",
                   string "Hacker News" >> return (iconPango hackerNewsCode),
                   string "Google+" >> return (iconPango googlePlusCode),
                   string "Google Wallet" >> return (iconPango googleWalletCode),
                   string "Google Search" >> return (iconPango googleCode),
                   string "Gmail " >> return "📧",
                   string "Google" >> return (iconPango googleCode),
                   string "Github" >> return (iconPango githubCode),
                   string "Facebook" >> return (iconPango facebookOfficialCode),
                   string "Emacs" >> return (iconPango emacsCode),
                   string "Apple" >> return (iconPango appleCode),
                   string "Amazon" >> return (iconPango amazonCode),
                   string "emacs@" >> skipMany1 (anyChar) >> return (iconPango emacsCode),
                   skipMany1 (noneOf " @") >> char '@' >> skipMany1 (noneOf " :") >> char ':' >> return (iconPango terminalCode)]

mangler :: Parsec String () [String]
mangler = do
  result <- many1 $ choice $ map try windowShortcuts ++ [fmap return anyChar]
  eof
  return result


windowMangler :: Int -> String -> String
windowMangler cnt w =
  case parse mangler "" w of
    Right x -> concat . take cnt $ x
    Left x -> drop 2 . dropWhile (/= '\n') $ show x

main = do
  netref <- newIORef [0, 0]
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew defaultPagerConfig {
        activeLayout = layoutMangler,
        activeWindow = windowMangler 80 . escape,
        activeWorkspace = colorize "#859900" "" . workspaceMangler,
        hiddenWorkspace = colorize "#268bd2" "" . workspaceMangler,
        urgentWorkspace = colorize "#002b36" "#268bd2" . workspaceMangler,
        visibleWorkspace = colorize "#2aa198" "" . workspaceMangler,
        widgetSep = " | "}
      note = notifyAreaNew defaultNotificationConfig
      mem = myPollingBar 5 memCallback
      net = myPollingBar 1 $ netCallback netref 0
      netup = myPollingBar 1 $ netCallback netref 1
      tray = systrayNew
  wifiWidget <- pollingLabelNew "" 5 wifiStatus >>= showAndReturn
  let wifi = clickCommand wifiWidget $ callProcess "/usr/bin/urxvt" ["-e", "/usr/bin/nmtui"]
  host <- getHostName
  let fsList = myFSList host
  chog <- pollingLabelNew "" 5 cpuHog >>= showAndReturn
  mhog <- pollingLabelNew "" 5 memHog >>= showAndReturn
  wcond <- pollingLabelNew "" 300 (weatherConditions "Didcot") >>= showAndReturn
  cpuIcon <- staticIcon vhdlCode
  memIcon <- staticIcon verilogCode
  weaIcon <- pollingLabelNew "waiting" 300 (weather "Didcot") >>= showAndReturn
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , barHeight = 20
                                        , barPosition = Bottom
                                        , endWidgets = [ tray, wifi,
                                                         clickWidget weaIcon wcond,
                                                         batteryWidget 300.0,
                                                         clock, staticIcon calendarCode,
                                                         mem, clickWidget memIcon mhog] ++
                                                       cpuWidget host 5.0 ++
                                                         [clickWidget cpuIcon chog,
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
