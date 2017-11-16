{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import System.IO
import XMonad.Util.SpawnOnce
import XMonad.Layout.NoBorders
import XMonad.Util.Dmenu
import Control.Monad (when)
import System.Exit
import Data.List (isPrefixOf)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

confirm :: String -> X () -> X ()
confirm m f = do
  result <- dmenu [m]
  when ("y" `isPrefixOf` result) f

xmobarHighlight :: String
xmobarHighlight = "#88e5fc"

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "discord" --> doShift "8discord"
  , className =? "Keepassx" --> doShift "9pw"
  ]

rofiOptions :: String
rofiOptions = "-font 'Changa Medium 18'"

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  spawn "killall nautilus"
  spawn "feh --bg-fill /home/mtolly/Pictures/1497743550594.png"
  xmonad $ docks $ ewmh def
    { manageHook = myManageHook <+> manageHook def
    , layoutHook = let
      tiled = Tall 1 (3/100) (3/5)
      adjust = avoidStruts
      in adjust tiled ||| noBorders Full
    , workspaces = ["1", "2", "3", "4", "5", "6", "7", "8discord", "9pw"]
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = xmobarColor xmobarHighlight "" . wrap "{" "}"
      , ppTitle = xmobarColor xmobarHighlight "" . shorten 100
      , ppLayout = const ""
      }
    , focusFollowsMouse = False
    , borderWidth = 2
    , focusedBorderColor = "#eee"
    , normalBorderColor = "#222"
    , modMask = mod4Mask
    , startupHook = do
      spawnOnce "xscreensaver -no-splash"
      spawnOnce "udiskie --no-notify"
      spawnOnce "trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --transparent true --tint 0x000000 --height 30"
      spawnOnce "nm-applet"
      spawnOnce "pasystray"
      spawnOnce "redshift-gtk -l43.0481434:-89.3455351"
      spawnOnce "keepassx"
      spawnOnce "discord-canary"
      setWMName "LG3D"
    } `additionalKeys`
    [ ((mod1Mask .|. controlMask, xK_Delete), spawn "xscreensaver-command -lock; xset dpms force off")
    , ((mod4Mask .|. shiftMask, xK_f), spawn "firefox-trunk")
    , ((mod4Mask .|. shiftMask, xK_t), spawn "thunar")
    , ((mod4Mask .|. shiftMask, xK_Return), spawn "gnome-terminal")
    , ((mod4Mask .|. shiftMask, xK_Home), spawn "unity-control-center")
    , ((mod4Mask, xK_p), spawn $ "rofi -show drun " ++ rofiOptions)
    , ((mod4Mask, xK_g), spawn $ "rofi -show run " ++ rofiOptions)
    , ((mod4Mask .|. shiftMask, xK_q), confirm "Exit" $ liftIO $ exitWith ExitSuccess)
    , ((mod4Mask, xK_q), restart "xmonad" True)
    ]
