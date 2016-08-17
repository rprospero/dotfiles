import           Control.Monad                (liftM2, filterM)
import           Data.List (isInfixOf, isPrefixOf)
import           Graphics.X11.ExtraTypes.XF86
import           System.Directory (getDirectoryContents, doesDirectoryExist)
import           System.FilePath ((</>),splitFileName)
import           System.Taffybar.Hooks.PagerHints (pagerHints)
import           XMonad
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.Search
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Circle
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
      [ className =? b --> viewShift "web" >> doF W.focusDown | b <- myClassWebShifts]
    , [ className =? b --> viewShift "emacs" | b <- myClassEmacsShifts]
    , [ className =? b --> viewShift "chat" | b <- myClassChatShifts]
    , [ className =? b --> viewShift "documents" | b <- myClassDocumentsShifts]
    , [ className =? b --> viewShift "media" | b <- myClassMediaShifts]
    , [ stringProperty "WM_NAME" =? ("Figure "++show b) --> doShift "documents" | b <- [1..9] :: [Int]]
    , [ (role =? "gimp-toolbox" <||> role =? "gimp-dock") --> doFloat]
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    myClassWebShifts = ["Firefox","Opera"]
    myClassEmacsShifts = ["Emacs"]
    myClassChatShifts = ["Pidgin","Thunderbird","Geary","mutt"]
    myClassDocumentsShifts = ["Evince"]
    myClassMediaShifts = ["Gimp"]
    role = stringProperty "WM_WINDOW_ROLE"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

main :: IO ()
-- main = xmonad . ewmh =<< statusBar "xmobar" myPP toggleStrutsKey myConfig
main = xmonad . pagerHints $ withUrgencyHook NoUrgencyHook $ myConfig

myLayoutHook = layoutHook def ||| Circle ||| simpleTabbed

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
               handleEventHook = handleEventHook def <+> fullscreenEventHook,
               manageHook = manageDocks <+> myManageHook,
               layoutHook = avoidStruts myLayoutHook,
               logHook = logHook def <+> ewmhDesktopsLogHook,
               modMask = mod4Mask,
               workspaces = myWorkspaces,
               startupHook = setWMName "LG3D"
             }
             `additionalKeys`
             [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -l")
             , ((controlMask, xK_Print), spawn "shutter -e -a -n -o '/home/adam/Documents/screenshot-%F-%T.png'")
             , ((0, xK_Print), spawn "shutter -e -f -n -o '/home/adam/Documents/screenshot-%F-%T.png'")
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
                (intelligent $
                  searchEngine "DuckDuckGo" "https://duckduckgo.com/?q="))
             ]

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
mySearchPrompt = defPrompt {searchPredicate = isInfixOf}

defPrompt = colourTheme myTheme def

colourTheme :: Theme -> XPConfig -> XPConfig
colourTheme t x = x {fgColor = inactiveTextColor t,
                     bgColor = "black",
                     fgHLight = activeTextColor t,
                     bgHLight = "black",
                     borderColor = activeBorderColor t,
                     font = fontName t}

myTheme :: Theme
myTheme = theme kavonForestTheme