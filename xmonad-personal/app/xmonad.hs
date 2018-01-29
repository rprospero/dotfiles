{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad                (liftM2, filterM,msum)
import           Data.List (isSuffixOf, isInfixOf, isPrefixOf,nub,stripPrefix)
import           Data.Map.Strict as M (fromList, lookup)
import           Data.Maybe (fromMaybe, fromJust)
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
  }

defaultBase16 :: Base16
defaultBase16 = Base16 {
  scheme = "000000"
  , author = "000000"
  , base00 = "000000"
  , base01 = "000000"
  , base02 = "000000"
  , base03 = "000000"
  , base04 = "000000"
  , base05 = "000000"
  , base06 = "F00000"
  , base07 = "F0F000"
  , base08 = "F0F0F0"
  , base09 = "00F0F0"
  , base0A = "000000"
  , base0B = "000000"
  , base0C = "000000"
  , base0D = "000000"
  , base0E = "000000"
  , base0F = "000000"
  }
readBase16Line :: String -> (String, String)
readBase16Line line = (take 6 line, ("#"++) . read . drop 8 $ line)

readBase16 :: FilePath -> IO (Maybe Base16)
readBase16 path = do
  ls <- readFile path
  let terms = fromList [readBase16Line l | l <- lines ls]
  return $ Base16 <$> M.lookup "scheme" terms
    <*> M.lookup "author" terms
    <*> M.lookup "base00" terms
    <*> M.lookup "base01" terms
    <*> M.lookup "base02" terms
    <*> M.lookup "base03" terms
    <*> M.lookup "base04" terms
    <*> M.lookup "base05" terms
    <*> M.lookup "base06" terms
    <*> M.lookup "base07" terms
    <*> M.lookup "base08" terms
    <*> M.lookup "base09" terms
    <*> M.lookup "base0A" terms
    <*> M.lookup "base0B" terms
    <*> M.lookup "base0C" terms
    <*> M.lookup "base0D" terms
    <*> M.lookup "base0E" terms
    <*> M.lookup "base0F" terms

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
  spawn "systemctl --user start taffybar"
  theme <- fromMaybe defaultBase16 <$> readBase16 "/home/adam/Code/dotfiles/base16/pop.yaml"
  xmonad . docks . pagerHints $ withUrgencyHook NoUrgencyHook (myConfig theme)

data NameSegment = Prefix String | Suffix String | Subst String String


replace :: String -> String -> String -> String
replace "" _ _ = ""
replace s old new =
  if old `isPrefixOf` s
  then new ++ replace (drop (length old) s) old new
  else head s:replace (tail s) old new

shrinkSegment :: String -> NameSegment -> Maybe String
shrinkSegment s (Prefix prefix) = stripPrefix prefix s
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
  Prefix "adam@dyn006107",
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

myLayoutHook theme = tabbedBottom MyShrinker (myTheme theme) ||| smartSpacing 10 (layoutHook def) ||| smartSpacing 10 (Grid False)

myConfig theme = def {
               focusedBorderColor = base03 theme,
               normalBorderColor = base00 theme,
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
promptTheme t x = x {fgColor = base07 t,
                     bgColor = base00 t,
                     fgHLight = base06 t,
                     bgHLight = base01 t,
                     borderColor = base04 t}

tabTheme :: Base16 -> Theme -> Theme
tabTheme p x = x {inactiveTextColor = base05 p,
                  inactiveColor = base02 p,
                  inactiveBorderColor = base03 p,
                  urgentColor = base08 p,
                  urgentTextColor = base09 p,
                  urgentBorderColor = base0A p,
                  activeTextColor = base07 p,
                  activeColor = base00 p,
                  activeBorderColor = base01 p}

myTheme :: Base16 -> Theme
myTheme th = tabTheme th $ theme kavonForestTheme
