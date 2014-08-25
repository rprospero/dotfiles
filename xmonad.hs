import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W
import System.IO
import Control.Monad (liftM2)
import Graphics.X11.ExtraTypes.XF86

myWorkspaces = ["main","web","emacs","documents","chat","media","7","8","9"]

myManageHook = composeAll . concat $
  [
      [ className =? b --> viewShift "web" | b <- myClassWebShifts]
    , [ className =? b --> viewShift "emacs" | b <- myClassEmacsShifts]
    , [ className =? b --> viewShift "chat" | b <- myClassChatShifts]
    , [ className =? b --> viewShift "documents" | b <- myClassDocumentsShifts]
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    myClassWebShifts = ["Firefox","Opera"]
    myClassEmacsShifts = ["Emacs"]
    myClassChatShifts = ["Pidgin","Thunderbird","Geary","mutt"]
    myClassDocumentsShifts = ["Evince"]

main = do
  xmproc <- spawn "conky -c ~/.xmonad/.conkyrc | dzen2 -fg cyan -fn \"inconsolata:pixelsize=12\" -w 832 -l 2 -y -1 -bg black"
  xmonad myConfig


--myStatusBar = statusBar "conky -c ~/.xmonad/.conkyrc | dzen2 -fg cyan -fn \"inconsolata:pixelsize=12\" -w 1100 -l 2 -y -1" myPP toggleStrutsKey

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


myConfig = defaultConfig {
               manageHook = manageDocks <+> myManageHook,
               layoutHook = avoidStruts $ layoutHook defaultConfig,
               modMask = mod4Mask,
               workspaces = myWorkspaces,
               startupHook = setWMName "LG3D",
               logHook = dynamicLogWithPP $ myPP
             }
             `additionalKeys`
             [ ((mod4Mask .|. shiftMask, xK_z), spawn "slock")
             , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
             , ((0, xK_Print), spawn "scrot")
             , ((mod4Mask, xK_p), spawn "$(~/.cabal/bin/yeganesh -x -- -b -nb black)")
             , ((mod4Mask, xK_e), spawn "emacsclient -c")
             , ((mod4Mask .|. shiftMask, xK_Return), spawn "xterm")
             , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer set Master 2%-")
             , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer set Master 2%+")
             , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")
             ]



myPP = dzenPP {ppOutput = printStatus . ("^tw()"++)}


printStatus :: String -> IO ()
printStatus s = do
   h <- openFile "/tmp/urgent" WriteMode
   hPutStrLn h s --(filter isLower s)
--   hClose h
