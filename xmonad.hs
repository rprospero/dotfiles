import           Control.Monad                (liftM2)
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         (additionalKeys)
import           XMonad.Util.Run              (spawnPipe)
import           XMonad.Layout.IM
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.ResizableTile

myWorkspaces = ["main","web","emacs","documents","chat","media","7","8","9"]

myManageHook = composeAll . concat $
  [
      [ className =? b --> viewShift "web" | b <- myClassWebShifts]
    , [ className =? b --> viewShift "emacs" | b <- myClassEmacsShifts]
    , [ className =? b --> viewShift "chat" | b <- myClassChatShifts]
    , [ className =? b --> viewShift "documents" | b <- myClassDocumentsShifts]
    , [ className =? b --> viewShift "media" | b <- myClassMediaShifts]
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    myClassWebShifts = ["Firefox","Opera"]
    myClassEmacsShifts = ["Emacs"]
    myClassChatShifts = ["Pidgin","Thunderbird","Geary","mutt"]
    myClassDocumentsShifts = ["Evince"]
    myClassMediaShifts = ["Gimp"]

main = do
  xmproc <- xmobar myConfig
  xmonad xmproc


toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myLayoutHook = onWorkspace "*" gimpLayout $ layoutHook defaultConfig
    where
      gimpLayout = withIM (11/64) (Role "gimp-toolbox") $ ResizableTall 2 (1/118) (11/20) [1] ||| Full

myConfig = defaultConfig {
               manageHook = manageDocks <+> myManageHook,
               layoutHook = avoidStruts $ myLayoutHook,
               modMask = mod4Mask,
               workspaces = myWorkspaces,
               startupHook = setWMName "LG3D"
             }
             `additionalKeys`
             [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -l")
             , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
             , ((0, xK_Print), spawn "scrot")
             , ((mod4Mask, xK_p), spawn "$(~/.cabal/bin/yeganesh -x -- -b -nb black)")
             , ((mod4Mask, xK_e), spawn "emacsclient -c")
             , ((mod4Mask, xK_t), spawn "thunar")
             , ((mod4Mask .|. shiftMask, xK_Return), spawn "xterm")
             , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer set Master 2%-")
             , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer set Master 2%+")
             , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")
             ]
