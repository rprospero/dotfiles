{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty)
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.List (isSuffixOf, isPrefixOf)
import Data.Monoid ((<>))
import Graphics.Icons.AllTheIcons
import Graphics.Icons.FileIcon hiding (appleCode)
import Graphics.Icons.FontAwesome hiding (terminalCode)
import Graphics.Icons.Octicon hiding (terminalCode, calendarCode, globeCode, fileTextCode)
import Graphics.Icons.Types
import Graphics.Icons.Weather hiding (trainCode)
import Network.Download (openURI)
import Network.HostName
import Text.Parsec (Parsec, anyChar, choice, eof, many1, parse, skipMany1, string, try)
import Text.Parsec.Char (noneOf, char)
import Text.Printf

import System.Taffybar

import System.Taffybar.Pager (colorize, escape)
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.Util

import System.Information.Battery
import System.Information.Memory
import System.Information.CPU2
import System.Information.Network

import Graphics.UI.Gtk.Abstract.Widget (Widget, toWidget)
import Graphics.UI.Gtk

import Data.IORef
import System.Process ( readProcess, callProcess )
import System.Taffybar.Widgets.PollingLabel ( pollingLabelNew )

join :: Monoid a => a -> [a] -> a
join _ [] = mempty
join _ [x] = x
join connect (x:xs) = x <> connect <> join connect xs

myPollingBar :: Double -> IO Double -> IO Widget
myPollingBar = pollingBarNew ((defaultBarConfig barColour){barBackgroundColor = const (0,0.0,0.0),
                                                           barPadding = 0,
                                                           barWidth = 9})

showAndReturn :: WidgetClass w => w -> IO Widget
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
myMail = do
  read <$> readProcess "/usr/bin/notmuch" ["count","query:important"] ""

mailWidget :: Double -> IO Widget
mailWidget update = do
  mvar <- mvarThread update 0 myMail
  mvarWidget mvar (\count ->
                      if count == 0
                      then ""
                      else colorize "#FFFFFF" "" $ iconPango mailCode <> " " <> show count)

myFSInfo :: String -> IO Double
myFSInfo fs = do
  fsOut <- readProcess "df" ["-kP", fs] ""
  let x = (/100) . fromIntegral . read . init . (!! 1) . reverse . words . last . lines $ fsOut
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
makeCpuChart cpu =
  myPollingBar 5 $ sum <$> getCPULoad ("cpu" ++ show cpu)

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
      child <- pollingLabelNew "" update cpuHog >>= showAndReturn
      clickWidget base child

cpuIcon :: IO Widget
cpuIcon =
  genericWidget 10 cpuHog "" (const $ iconPango vhdlCode) id

cpuHog :: IO String
cpuHog = (unwords . drop 10 . words . last . lines) <$> readProcess "ps" ["aux","--sort","%cpu"] ""

memIcon :: IO Widget
memIcon =
  genericWidget 10 memHog "" (const $ iconPango verilogCode) id

memHog :: IO String
memHog = (unwords . drop 10 . words . last . lines) <$> readProcess "ps" ["aux","--sort","%mem"] ""

------------------------------

netCallback :: IORef [Integer] -> Int -> IO Double
netCallback ref idx = do
  old <- readIORef ref
  minfo <- getNetInfo "eno1"
  case minfo of
    Nothing -> return 0
    Just [] -> return 0
    Just xs -> do
      writeIORef ref xs
      return $ fromIntegral (xs !! idx - old !! idx) / 30000000

wifiConnected :: IO Bool
wifiConnected = (/= "disconnected") . head . words . head . tail . lines <$> readProcess "/usr/bin/nmcli" ["general"] ""

wifiIcon :: Bool -> String
wifiIcon True = iconPango wifiCode
wifiIcon False = colorize "#7a2d00" "" $ iconPango wifiCode

wifiWidget :: IO Widget
wifiWidget =
  genericWidgetSpawn 5 wifiConnected False wifiIcon $
  callProcess "/usr/bin/urxvt" ["-e", "/usr/bin/nmtui"]

barColour :: Double -> (Double, Double, Double)
barColour x
  | x < 1.0/3.0 = (0,3.0*x,0)
  | x < 2.0/3.0 = (3*x-1,1,0)
  | otherwise = (1,abs (3-3*x),0)

-- Insert image icons
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

clearIcon :: Bool -> String
clearIcon True = iconPango forecastIoClearDayCode
clearIcon False = iconPango forecastIoClearNightCode
cloudyIcon :: Bool -> String
cloudyIcon True = iconPango dayCloudyCode
cloudyIcon False = iconPango nightAltCloudyCode
overcastIcon :: Bool -> String
overcastIcon _ = iconPango cloudyCode
thunderstormIcon :: Bool -> String
thunderstormIcon True = iconPango dayThunderstormCode
thunderstormIcon False = iconPango nightAltThunderstormCode
drizzleIcon :: Bool -> String
drizzleIcon True = iconPango daySprinkleCode
drizzleIcon False = iconPango nightAltSprinkleCode
rainIcon :: Bool -> String
rainIcon True = iconPango dayRainCode
rainIcon False = iconPango nightAltRainCode
showerIcon :: Bool -> String
showerIcon True = iconPango dayShowersCode
showerIcon False = iconPango nightAltShowersCode
snowIcon :: Bool -> String
snowIcon True = iconPango daySnowCode
snowIcon False = iconPango nightAltSnowCode
sleetIcon :: Bool -> String
sleetIcon True = iconPango daySleetCode
sleetIcon False = iconPango nightAltSleetCode
mistIcon :: Bool -> String
mistIcon _ = iconPango fogCode

---------------  Battery Icon Code

myBatteryInfo :: IO (Maybe BatteryInfo)
myBatteryInfo = do
  ctx <- batteryContextNew
  case ctx of
    Nothing -> return Nothing
    Just c -> getBatteryInfo c

batteryIcon :: BatteryInfo -> String
batteryIcon info =
  case batteryState info of
    BatteryStateCharging -> iconPango batteryChargingCode
    BatteryStateFullyCharged -> iconPango batteryChargingCode
    BatteryStateDischarging -> appropriateBattery info
    _ -> iconPango batteryEmptyCode

batteryTime :: BatteryInfo -> String
batteryTime info =
  case batteryState info of
    BatteryStateCharging -> secondsToTime (batteryTimeToFull info) <> " Till Full"
    BatteryStateDischarging -> secondsToTime (batteryTimeToEmpty info) <> " Till Empty"
    BatteryStateFullyCharged -> "Charged"
    x -> show x

batteryValue :: IO (Either String BatteryInfo)
batteryValue = do
  minfo <- myBatteryInfo
  case minfo of
    Nothing -> return $ Left "Battery Not Found"
    Just info -> return $ Right info

batteryWidget :: Double -> IO Widget
batteryWidget update =
  genericErrorWidget update batteryValue batteryIcon batteryTime

secondsToTime :: (Integral a, Show a, PrintfArg a) => a -> String
secondsToTime x = show hours <> ":" <> printf "%02d" minutes
  where
    hours = x `div` 3600
    minutes = (x `mod` 3600) `div` 60

appropriateBattery :: BatteryInfo -> String
appropriateBattery x
  | batteryPercentage x < 20.0 = colorize "#7a2d00" "" $ iconPango batteryEmptyCode
  | batteryPercentage x < 40.0 = colorize "#f8ca12" "" $ iconPango batteryQuarterCode
  | batteryPercentage x < 60.0 = colorize "#37b349" "" $ iconPango batteryHalfCode
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
  parseJSON _ = empty

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
  parseJSON _ = empty

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
  parseJSON _ = empty

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
    Left err -> Left err
    Right body -> case getWeather body of
      Nothing -> Left $ "Cannot parse weather: " <> take 20 (show text)
      Just w -> Right w

weatherIcon :: Weather -> String
weatherIcon w =
  case weatherStates w of
    [] -> "No Weather"
    x:_ ->
      let daytime = (== 'd') . last . wsIcon $ x
      in flip rawWeatherIcon daytime $ wsId x

weatherDesc :: Weather -> String
weatherDesc w =
  let
    temp = show . round . (+ (-273.15)) . wsTemp . weatherMain $ w
    desc = join ", " . map wsDescription . weatherStates $ w
  in
    temp ++ iconPango celsiusCode ++ " " ++ desc

weatherWidget :: String -> Double -> IO Widget
weatherWidget location update = do
  genericErrorWidget update (localWeather "Didcot") weatherIcon weatherDesc

redErr :: Either String String -> String
redErr (Left err) = colorize "#7a2d00" "" err
redErr (Right value) = value

--  Widget Utilities

genericWidget :: Double -> IO a -> a -> (a -> String) -> (a -> String) -> IO Widget
genericWidget update action def render fullRender = do
  m <- mvarThread update def action
  base <- mvarWidget m render
  child <- mvarWidget m fullRender
  clickWidget base child

genericErrorWidget :: Double -> IO (Either String a) -> (a -> String) -> (a -> String) -> IO Widget
genericErrorWidget update action render fullRender =
  genericWidget update action (Left "Not Loaded") (redErr . fmap render) (redErr . fmap fullRender)

genericWidgetSpawn update action def render command = do
  m <- mvarThread update def action
  base <- mvarWidget m render
  clickCommand base command

mvarThread :: Double -> a -> IO a -> IO (MVar a)
mvarThread delay def action = do
  m <- newMVar def
  _ <- forkIO . forever $ do
    _ <- action >>= swapMVar m
    threadDelay (round $ delay * 1e6)
  return m


mvarWidget :: MVar a -> (a -> String) -> IO Widget
mvarWidget m f =
  let
    status = f <$> readMVar m
  in
    pollingLabelNew "" 1 status >>= showAndReturn

intoNewBox :: WidgetClass w => w -> IO EventBox
intoNewBox base = do
  ebox <- eventBoxNew
  containerAdd ebox base
  eventBoxSetVisibleWindow ebox False
  return ebox

clickWidget :: Widget -> Widget -> IO Widget
clickWidget base child = do
  ebox <- intoNewBox base
  window <- makeWindow $ return child
  _ <- on ebox buttonPressEvent $ onClick [SingleClick] (toggleChild "" base window)
  widgetShowAll ebox
  return (toWidget ebox)

clickCommand :: Widget -> IO () -> IO Widget
clickCommand base command = do
  ebox <- intoNewBox base
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
  let w = toWidget l
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
                   string "org-mode" >> return (iconPango orgCode),
                   string "Jenkins" >> return (iconPango jenkinsCode),
                   string "Inbox " >> return "📧",
                   string "Hacker News" >> return (iconPango hackerNewsCode),
                   string "Google+" >> return (iconPango googlePlusCode),
                   string "Google Wallet" >> return (iconPango googleWalletCode),
                   string "Google Search" >> return (iconPango googleCode),
                   string "Gmail " >> return "📧",
                   string "Google" >> return (iconPango googleCode),
                   string "Github" >> return (iconPango githubCode),
                   string "github" >> return (iconPango githubCode),
                   string "GitHub" >> return (iconPango githubCode),
                   string "Facebook" >> return (iconPango facebookOfficialCode),
                   string "Emacs" >> return (iconPango emacsCode),
                   string "Apple" >> return (iconPango appleCode),
                   string "Amazon" >> return (iconPango amazonCode),
                   string "emacs@" >> skipMany1 anyChar >> return (iconPango emacsCode),
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
        activeWorkspace = colorize "#37b349" "" . workspaceMangler,
        hiddenWorkspace = colorize "#505050" "" . workspaceMangler,
        urgentWorkspace = colorize "#000000" "#FFFFFF" . workspaceMangler,
        visibleWorkspace = colorize "#00aabb" "" . workspaceMangler,
        widgetSep = " | "}
      note = notifyAreaNew defaultNotificationConfig
      mem = myPollingBar 5 memCallback
      net = myPollingBar 1 $ netCallback netref 0
      netup = myPollingBar 1 $ netCallback netref 1
      tray = systrayNew
  host <- getHostName
  let fsList = myFSList host
  defaultTaffybar defaultTaffybarConfig {
    startWidgets = [ pager ]
    , barHeight = 20
    , barPosition = Bottom
    , endWidgets = [ tray, wifiWidget,
                     weatherWidget "Didcot" 300.0,
                     batteryWidget 300.0,
                     clock, staticIcon calendarCode,
                     mem, memIcon] ++
                   cpuWidget host 5.0 ++
                   [cpuIcon,
                     netup, net,
                     staticIcon globeCode] ++
                   fsList ++
                   [staticIcon hddOCode,
                     mailWidget 10,
                     note]
    }
myFSList :: String -> [IO Widget]
myFSList host
  | ".shef.ac.uk" `isSuffixOf` host = [myFSMonitor "/",
                                       myFSMonitor "/data",
                                       myFSMonitor "/home",
                                       myFSMonitor "/mnt/NAS"]
  | otherwise = [myFSMonitor "/"]
