import           Control.Monad                (liftM2, filterM)
import           Data.List (isInfixOf, isPrefixOf,nub)
import           Graphics.X11.ExtraTypes.XF86
import           System.Directory (getDirectoryContents, doesDirectoryExist)
import           System.FilePath ((</>),splitFileName)
import           System.Posix.Env (putEnv)
import           System.Taffybar.Hooks.PagerHints (pagerHints)
import           XMonad
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.Search
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.HintedGrid
import           XMonad.Layout.Tabbed
import qualified XMonad.StackSet              as W
import           XMonad.Prompt
import           XMonad.Prompt.RunOrRaise
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Ssh
import           XMonad.Prompt.Theme
import           XMonad.Prompt.Window
import           XMonad.Prompt.XMonad
import           XMonad.Util.EZConfig         (additionalKeys)
import           XMonad.Util.Themes

myWorkspaces :: [String]
myWorkspaces = ["main","web","emacs","documents","chat","media","7","8","9"]

myManageHook = composeAll . concat $
  [
      [ className =? b --> doF W.focusDown | b <- myClassWebShifts]
    , [ className =? b --> viewShift "web" | b <- myClassWebShifts]
    , [ className =? b --> viewShift "emacs" | b <- myClassEmacsShifts]
    , [ className =? b --> viewShift "chat" | b <- myClassChatShifts]
    , [ className =? b --> viewShift "documents" | b <- myClassDocumentsShifts]
    , [ className =? b --> viewShift "media" | b <- myClassMediaShifts]
    , [ stringProperty "WM_NAME" =? ("Figure "++show b) --> doShift "documents" | b <- [1..9] :: [Int]]
    , [ (role =? "gimp-toolbox" <||> role =? "gimp-dock") --> doFloat]
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    myClassWebShifts = ["Firefox","Opera","google-chrome"]
    myClassEmacsShifts = ["Emacs"]
    myClassChatShifts = ["Pidgin","Thunderbird","Geary","mutt"]
    myClassDocumentsShifts = ["Evince"]
    myClassMediaShifts = ["Gimp"]
    role = stringProperty "WM_WINDOW_ROLE"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

main :: IO ()
-- main = xmonad . ewmh =<< statusBar "xmobar" myPP toggleStrutsKey myConfig
main = do
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"
  putEnv "SAL_USE_VCLPLUGIN=gen"
  xmonad . pagerHints $ withUrgencyHook NoUrgencyHook $ myConfig

myLayoutHook = tabbed shrinkText myTheme ||| layoutHook def ||| Grid False

iconifyWorkspaces "web" = "<icon=/home/adam/Downloads/fox.xbm/>"
iconifyWorkspaces "emacs" = "<icon=/home/adam/Downloads/code.xbm/>"
iconifyWorkspaces "documents" = "<icon=/home/adam/Downloads/docs.xbm/>"
iconifyWorkspaces x = x

myPP :: PP
myPP = xmobarPP { ppLayout = myLayoutPrinter
                , ppVisible = xmobarColor "#bbbb00" "black" . wrap "(" ")" . iconifyWorkspaces
                , ppHidden = xmobarColor "#888888" "black" . iconifyWorkspaces
                , ppUrgent = xmobarColor "#FF0000" "black" . wrap "!" "!" . iconifyWorkspaces
                , ppCurrent = xmobarColor "#FFFF00" "black" . wrap "[" "]" . iconifyWorkspaces
  }

myLayoutPrinter :: String -> String
myLayoutPrinter "Full" = "<icon=/home/adam/dotfiles/layout_full.xbm/>"
myLayoutPrinter "Tall" = "<icon=/home/adam/dotfiles/layout_tall.xbm/>"
myLayoutPrinter "Mirror Tall" = "<icon=/home/adam/dotfiles/layout_mirror_tall.xbm/>"
myLayoutPrinter x = x

myConfig = def {
               focusedBorderColor = pBC subTheme,
               handleEventHook = handleEventHook def <+> fullscreenEventHook <+> ewmhDesktopsEventHook,
               manageHook = manageDocks <+> myManageHook,
               layoutHook = avoidStruts myLayoutHook,
               logHook = logHook def <+> ewmhDesktopsLogHook,
               modMask = mod4Mask,
               workspaces = myWorkspaces
             }
             `additionalKeys`
             [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -l")
             , ((controlMask, xK_Print), spawn "shutter -e -a -n -o '/home/adam/Documents/screenshot-%F-%T.png'")
             , ((0, xK_Print), spawn "shutter -e -f -n -o '/home/adam/Documents/screenshot-%F-%T.png'")
             , ((mod4Mask, xK_f), spawn "firefox")
             , ((mod4Mask .|. mod1Mask, xK_e), spawn "emacsclient -c")
             , ((mod4Mask, xK_t), spawn "thunar")
             , ((mod4Mask .|. mod1Mask, xK_t), themePrompt mySearchPrompt)
             , ((mod4Mask .|. shiftMask, xK_t), thunarPrompt)
             , ((mod4Mask .|. shiftMask, xK_Return), spawn "urxvt")
             , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer set Master 2%-")
             , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer set Master 2%+")
             , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")
             , ((mod4Mask, xK_v), selectWorkspace mySearchPrompt)
             , ((mod4Mask .|. shiftMask, xK_v), withWorkspace mySearchPrompt (windows . W.shift))
             , ((mod4Mask .|. shiftMask, xK_m), addWorkspacePrompt defPrompt)
             , ((mod4Mask, xK_BackSpace), removeEmptyWorkspace)
             , ((mod4Mask, xK_s), sshPrompt mySearchPrompt)
             , ((mod4Mask, xK_p), runOrRaisePrompt defPrompt)
             , ((mod4Mask .|. shiftMask, xK_p), shellPrompt defPrompt)
             , ((mod4Mask, xK_g), windowPromptGoto mySearchPrompt)
             , ((mod4Mask .|. shiftMask, xK_g), windowPromptBring mySearchPrompt)
             , ((mod4Mask, xK_x), xmonadPrompt mySearchPrompt)
             , ((mod4Mask, xK_i),
                promptSearch defPrompt
                (moreIntelligent $
                  searchEngine "DuckDuckGo" "https://duckduckgo.com/?q="))
             , ((mod4Mask .|. shiftMask , xK_n), withFocused $ windows . W.sink)
             ]

moreIntelligent :: SearchEngine -> SearchEngine
moreIntelligent (SearchEngine name site) = searchEngineF name f
  where
    f s = if or ["http://" `isPrefixOf` s,
                 "https://" `isPrefixOf` s,
                 "ftp://" `isPrefixOf` s,
                 and ['.' `elem` s, not $ ' '`elem` s ]]
          then s
          else site s

thunarPrompt = mkXPrompt Thunar defPrompt directoryComplete (spawn . ("thunar "++))

directoryComplete :: String -> IO [String]
directoryComplete x = do
  let (dir, cur) = splitFileName x
  dirs <- getDirectoryContents dir
  realDirs <- filterM doesDirectoryExist $ map (dir </>) $ filter (cur `isPrefixOf`) dirs
  return realDirs

data Thunar = Thunar

instance XPrompt Thunar where
  showXPrompt Thunar = "Directory:  "

mySearchPrompt :: XPConfig
mySearchPrompt = defPrompt {searchPredicate = mySearchPredicate,
                            autoComplete = Just 1}


mySearchPredicate :: String -> String -> Bool
mySearchPredicate query item = and . map (`isInfixOf` item) . words $ query

defPrompt = promptTheme subTheme def {historyFilter = nub}

colourTheme :: Theme -> XPConfig -> XPConfig
colourTheme t x = x {fgColor = inactiveTextColor t,
                     bgColor = "black",
                     fgHLight = activeTextColor t,
                     bgHLight = "black",
                     borderColor = activeBorderColor t,
                     font = fontName t}

promptTheme :: PromptTheme -> XPConfig -> XPConfig
promptTheme t x = x {fgColor = pFg t,
                     bgColor = pBg t,
                     fgHLight = pFgH t,
                     bgHLight = pBgH t,
                     borderColor = pBC t,
                     font = pFont t}

tabTheme :: PromptTheme -> Theme -> Theme
tabTheme p x = x {inactiveTextColor = pFg p,
                  inactiveColor = pBg p,
                  inactiveBorderColor = pBgH p,
                  activeTextColor = pFgH p,
                  activeColor = pBgH p,
                  activeBorderColor = pBgH p}

myTheme :: Theme
myTheme = tabTheme subTheme $ theme kavonForestTheme

subTheme = solarizedTheme

data PromptTheme = PromptTheme {
  pFg :: String,
  pBg :: String,
  pFgH :: String,
  pBgH :: String,
  pBC :: String,
  pFont :: String}

moeTheme = PromptTheme {
  pFg = "#c6c6c6",
  pBg = "#303030",
  pFgH = "#4e4e4e",
  pBgH = "#d7ff5f",
  pBC = "#c6c6c6",
  pFont = fontName def}

grandShellTheme = PromptTheme {
  pBg = "black",
  pFg = "gray",
  pFgH = "gray",
  pBgH = "#34004A",
  pBC = "gray",
  pFont = fontName def}

monokaiTheme = PromptTheme {
  pFg = "#F8F8F2",
  pBg = "#272822",
  pFgH = "#F8F8F2",
  pBgH = "#49483E",
  pBC = "#F8F8F2",
  pFont = fontName def}

sanityBrightTheme = PromptTheme {
  pFg = "#eaeaea",
  pBg = "#000000",
  pFgH = "#eaeaea",
  pBgH = "#424242",
  pBC = "#eaeaea",
  pFont = fontName def}

sanityEightiesTheme = PromptTheme {
  pFg = "#cccccc",
  pBg = "#2d2d2d",
  pFgH = "#cccccc",
  pBgH = "#515151",
  pBC = "#cccccc",
  pFont = fontName def}

cyberPunkTheme = PromptTheme {
  pFg = "#d3d3d3",
  pBg = "#000000",
  pFgH = "#d3d3d3",
  pBgH = "#7F073F",
  pBC = "#d3d3d3",
  pFont = fontName def}

darkToothTheme = PromptTheme {
  pFg = "#FDF4C1",
  pBg = "#282828",
  pFgH = "#FDF4C1",
  pBgH = "#30434C",
  pBC = "#FDF4C1",
  pFont = fontName def}

materialTheme = PromptTheme {
  pFg = "#ffffff",
  pBg = "#263238",
  pFgH = "white",
  pBgH = "#555555",
  pBC = "#ffffff",
  pFont = fontName def}

tronTheme = PromptTheme {
  pFg = "#d3f9ee",
  pBg = "#081724",
  pFgH = "#d3f9ee",
  pBgH = "#1d5483",
  pBC = "#d3f9ee",
  pFont = fontName def}

solarizedTheme = PromptTheme {
  pFg = "#839496",
  pBg = "#002b36",
  pFgH = "#93a1a1",
  pBgH = "073642",
  pBC = "#859900",
  pFont = fontName def}
