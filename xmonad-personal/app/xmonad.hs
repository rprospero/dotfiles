{-# LANGUAGE FlexibleContexts, DeriveGeneric #-}
import           Control.Monad                (liftM2, filterM,msum)
import           Data.List (isSuffixOf, isInfixOf, isPrefixOf,nub,stripPrefix)
import           Data.Map.Strict as M (fromList, lookup)
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Yaml
import           GHC.Generics
import Graphics.Icons.AllTheIcons
import Graphics.Icons.FileIcon hiding (appleCode)
import Graphics.Icons.FontAwesome hiding (terminalCode)
import Graphics.Icons.Octicon hiding (terminalCode, calendarCode, globeCode, fileTextCode)
import Graphics.Icons.Types
import Graphics.Icons.Weather hiding (trainCode)
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
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.HintedGrid
import           XMonad.Layout.Named
import           XMonad.Layout.Spacing
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

data Base16 = Base16 {
  scheme :: String
  , author :: String
  , base00 :: String
  , base01 :: String
  , base02 :: String
  , base03 :: String
  , base04 :: String
  , base05 :: String
  , base06 :: String
  , base07 :: String
  , base08 :: String
  , base09 :: String
  , base0A :: String
  , base0B :: String
  , base0C :: String
  , base0D :: String
  , base0E :: String
  , base0F :: String
  } deriving (Generic)

instance FromJSON Base16

defaultBase16 :: Base16
defaultBase16 = Base16 {
  scheme = "#Default"
  , author = "#No one"
  , base00 = "#F0F000"
  , base01 = "#F00000"
  , base02 = "#00F000"
  , base03 = "#0000F0"
  , base04 = "#000000"
  , base05 = "#000000"
  , base06 = "#F00000"
  , base07 = "#F0F000"
  , base08 = "#F0F0F0"
  , base09 = "#00F0F0"
  , base0A = "#000000"
  , base0B = "#000000"
  , base0C = "#000000"
  , base0D = "#000000"
  , base0E = "#000000"
  , base0F = "#000000"
  }

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

main :: IO ()
main = do
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"
  putEnv "SAL_USE_VCLPLUGIN=gen"
  spawn "sh ~/setroot.sh"
  spawn "systemctl --user start taffybar"
  theme <- fromMaybe defaultBase16 <$> decodeFile "/home/adam/Code/dotfiles/base16/theme.yaml"
  xmonad . docks . pagerHints $ withUrgencyHook NoUrgencyHook (myConfig theme)

data NameSegment = Prefix String | Suffix String | Subst String String


replace :: String -> String -> String -> String
replace "" _ _ = ""
replace s old new =
  if old `isPrefixOf` s
  then new ++ replace (drop (length old) s) old new
  else head s:replace (tail s) old new

shrinkSegment :: String -> NameSegment -> Maybe String
shrinkSegment s (Main.Prefix prefix) = stripPrefix prefix s
shrinkSegment s (Suffix suffix)
  | suffix `isSuffixOf` s = Just $ reverse $ drop (length suffix) $ reverse s
  | otherwise = Nothing
shrinkSegment s (Subst before after)
  | before `isInfixOf`  s = Just $ replace s before after
  | otherwise = Nothing

mySegments :: [NameSegment]
mySegments = [
  -- Subst "=>" "⇒",
  Suffix " - Mozilla Firefox",
  Suffix " - GIMP",
  Main.Prefix "adam@dyn006107",
  Suffix " - Google Search",
  Suffix " - Hoogle"]

dropSegment :: String -> String
dropSegment s = fromMaybe (init s) . msum $ map (shrinkSegment s) mySegments
  -- case msum $ map (shrinkSegment s) mySegments of
  --   Just x -> x
  --   Nothing -> init s
myShrinkText :: s -> String -> [String]
myShrinkText _ "" = [""]
myShrinkText s cs = cs:myShrinkText s (dropSegment cs)

data MyShrinker = MyShrinker
instance Show MyShrinker where show _ = ""
instance Read MyShrinker where readsPrec _ s = [(MyShrinker,s)]
instance Shrinker MyShrinker where
  shrinkIt = myShrinkText

myLayoutHook theme = named (iconPango listCode) (tabbedBottom MyShrinker $ myTabConfig theme)
  ||| named (iconPango squareCode) Full
  ||| named (iconPango arrowsVCode) (smartSpacing 10 (Tall 1 (3/100) (1/2)))
  ||| named (iconPango thCode) (smartSpacing 10 (Grid False))

myConfig theme = def {
               focusedBorderColor = "#" ++ base03 theme,
               normalBorderColor = "#" ++ base00 theme,
               handleEventHook = handleEventHook def <+> fullscreenEventHook <+> ewmhDesktopsEventHook,
               manageHook = manageDocks <+> myManageHook,
               layoutHook = avoidStruts $ myLayoutHook theme,
               logHook = logHook def <+> ewmhDesktopsLogHook <+> fadeInactiveCurrentWSLogHook 0.5,
               modMask = mod4Mask,
               terminal = "urxvt +sb",
               workspaces = myWorkspaces
             }
             `additionalKeys`
             [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -l")
             , ((controlMask, xK_Print), spawn "scrot -u")
             , ((0, xK_Print), spawn "scrot")
             , ((mod4Mask, xK_f), spawn "firefox")
             , ((mod4Mask .|. mod1Mask, xK_e), spawn "emacsclient -c")
             , ((mod4Mask, xK_t), spawn "thunar")
             , ((mod4Mask .|. mod1Mask, xK_t), themePrompt $ mySearchPrompt theme)
             , ((mod4Mask .|. shiftMask, xK_t), thunarPrompt theme)
             , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer set Master 2%-")
             , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer set Master 2%+")
             , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")
             , ((mod4Mask, xK_v), selectWorkspace $ mySearchPrompt theme)
             , ((mod4Mask .|. shiftMask, xK_v), withWorkspace (mySearchPrompt theme) (windows . W.shift))
             , ((mod4Mask .|. shiftMask, xK_m), addWorkspacePrompt $ defPrompt theme)
             , ((mod4Mask, xK_BackSpace), removeEmptyWorkspace)
             , ((mod4Mask, xK_s), sshPrompt $ mySearchPrompt theme)
             , ((mod4Mask, xK_p), runOrRaisePrompt $ defPrompt theme)
             , ((mod4Mask .|. shiftMask, xK_p), shellPrompt $ defPrompt theme)
             , ((mod4Mask, xK_g), windowPromptGoto $ mySearchPrompt theme)
             , ((mod4Mask .|. shiftMask, xK_g), windowPromptBring $ mySearchPrompt theme)
             , ((mod4Mask, xK_x), xmonadPrompt $ mySearchPrompt theme)
             , ((mod4Mask, xK_i),
                promptSearch (defPrompt theme)
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
                 ('.' `elem` s) && (' '`notElem` s )]
          then s
          else site s

thunarPrompt :: Base16 -> X ()
thunarPrompt theme = mkXPrompt Thunar (defPrompt theme) directoryComplete (spawn . ("thunar "++))

directoryComplete :: String -> IO [String]
directoryComplete x = do
  let (dir, cur) = splitFileName x
  dirs <- getDirectoryContents dir
  filterM doesDirectoryExist $ map (dir </>) $ filter (cur `isPrefixOf`) dirs

data Thunar = Thunar

instance XPrompt Thunar where
  showXPrompt Thunar = "Directory:  "

mySearchPrompt :: Base16 -> XPConfig
mySearchPrompt theme = (defPrompt theme){searchPredicate = mySearchPredicate,
                            autoComplete = Just 1}


mySearchPredicate :: String -> String -> Bool
mySearchPredicate query item = all (`isInfixOf` item) . words $ query

defPrompt :: Base16 -> XPConfig
defPrompt theme = promptTheme theme def {historyFilter = nub}

promptTheme :: Base16 -> XPConfig -> XPConfig
promptTheme t x = x {fgColor = "#" ++ base07 t,
                     bgColor = "#" ++ base00 t,
                     fgHLight = "#" ++ base06 t,
                     bgHLight = "#" ++ base01 t,
                     borderColor = "#" ++ base04 t}

myTabConfig theme = def {
  activeColor = "#" ++base01 theme
  , inactiveColor = "#" ++base00 theme
  , urgentColor = "#" ++base02 theme
  , activeTextColor = "#" ++base06 theme
  , inactiveTextColor = "#" ++base05 theme
  , urgentTextColor = "#" ++base07 theme
  , activeBorderColor = "#" ++base01 theme
  , inactiveBorderColor = "#" ++base00 theme
  , urgentBorderColor = "#" ++base02 theme
}
