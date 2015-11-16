import           Control.Monad                (liftM2)
import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         (additionalKeys)

myWorkspaces :: [String]
myWorkspaces = ["main","web","emacs","documents","chat","media","7","8","9"]

myManageHook = composeAll . concat $
  [
      [ className =? b --> viewShift "web" | b <- myClassWebShifts]
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

main :: IO ()
main = do
  xmproc <- spawn "conky -c ~/.xmonad/.conkyrc | dzen2 -fg cyan -fn \"inconsolata:pixelsize=11\" -w 832 -y -1 -bg black"
  xmonad myConfig


myLayoutHook :: Choose Tall (Choose (Mirror Tall) Full) Window
myLayoutHook = layoutHook defaultConfig

myConfig = defaultConfig {
               manageHook = manageDocks <+> myManageHook,
               layoutHook = avoidStruts myLayoutHook,
               modMask = mod4Mask,
               workspaces = myWorkspaces,
               startupHook = setWMName "LG3D",
               logHook = dynamicLogWithPP $ xmobarPP
             }
             `additionalKeys`
             [ ((mod4Mask .|. shiftMask, xK_z), spawn "slock")
             , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
             , ((0, xK_Print), spawn "scrot")
             , ((mod4Mask, xK_p), spawn "$(~/.cabal/bin/yeganesh -x -- -b -nb black)")
             , ((mod4Mask .|. mod1Mask, xK_e), spawn "emacsclient -c")
             , ((mod4Mask, xK_t), spawn "thunar")
             , ((mod4Mask .|. shiftMask, xK_Return), spawn "xterm")
             , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer set Master 2%-")
             , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer set Master 2%+")
             , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")
             ]
