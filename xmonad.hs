import System.IO
import System.Exit
import XMonad
import Graphics.X11.ExtraTypes.XF86

import XMonad.StackSet as W

import XMonad.Config.Desktop
import XMonad.Config.Azerty

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (doFullFloat, doCenterFloat, doRectFloat, isFullscreen, isDialog, doLower)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.ManageHook

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ShowWName
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.CircleEx
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull)
import XMonad.Layout.ShowWName
import XMonad.Layout.NoBorders (noBorders, smartBorders)

import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig  -- Simplifies keybindings
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook, javaHack, trayerAboveXmobarEventHook, trayAbovePanelEventHook, trayerPaddingXmobarEventHook, trayPaddingXmobarEventHook, trayPaddingEventHook)
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)

import Graphics.X11.ExtraTypes.XF86  -- Multimedia keys

------------------------------------------------------------------------
-- Colors --
------------------------------------------------------------------------

-- Dracula colors --
normBord = "#100c08"
focdBord = "#BD93F9"
fore     = "#BD93F9"
back     = "#282A36"
winType  = "#BD93F9"

---------------------------------------------------------------------------------
-- Theme for showWName which prints current workspace when you change workspaces.
---------------------------------------------------------------------------------

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font = "xft:Crisp:size=75"
    , swn_fade    = 0.5
    , swn_bgcolor = "#1c1f24"
    , swn_color   = "#9580FF"
    }

myModMask = mod4Mask
myFocusFollowsMouse = True
myBorderWidth = 2

------------------------------------------------------------------------
-- Layouts --
------------------------------------------------------------------------

myLayout = spacingRaw True (Border 0 0 0 0) True (Border 5 5 5 5) True $
           smartBorders $
           avoidStruts $
           layoutHook def

-------------------------------------------------------------------------
-- Window Management Hook --
------------------------------------------------------------------------

myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]  -- Center dialog boxes properly
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [className =? "vlc" --> doFloat]  -- Make VLC float
    , [className =? "Conky" --> doIgnore]
    , [title =? "system_conky" --> doIgnore]
    , [isFullscreen --> doFullFloat]
    , [manageDocks]
    ]

    where
    myCFloats = ["Arandr", "Galculator", "feh", "mpv", "Xfce4-terminal"]
    myTFloats = ["Downloads", "Save As...", "vlc"]
    myRFloats = []
    myIgnores = ["desktop_window", "Conky", "system_conky"]

-------------------------------------------------------------------------
-- Startup Hook --
------------------------------------------------------------------------

myStartupHook = do
    spawnOnce "picom --config ~/.config/picom/picom.conf &"
    spawnOnce "/home/j3ll0/.config/scripts/wallpaper.sh &"
    spawnOnce "nm-applet &"
    spawnOnce "copyq &"
    spawnOnce "blueberry-tray &"
    spawnOnce "redshift -P -l 43.8:-79.3 -O 4000 &"
    spawnOnce "sleep 5 && volumeicon &"
    spawnOnce "polybar mainbar-xmonad &" 
    spawnOnce "polybar mainbar-xmonad-extra &" 
    spawnOnce "/home/j3ll0/.config/dunst/dunstrc &"
    spawnOnce "xsetroot -cursor_name left_ptr &"
    spawnOnce "/run/wrappers/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh"
    spawnOnce "/nix/store/75lv1npbf6zhsy1lk3v7z9indvwgm96d-polkit-gnome-0.105/libexec/polkit-gnome-authentication-agent-1 &"
    spawnOnce "conky -c /home/j3ll0/.config/scripts/AUR-Allinone.conkyrc &"

------------------------------------------------------------------------
-- Main Configuration --
------------------------------------------------------------------------

main = xmonad $ ewmh $ docks def 
    { terminal    = "kitty" 
    , modMask     = mod4Mask  -- Use Super (Windows) key 
    , borderWidth = 3 
    , startupHook = myStartupHook 
    , manageHook  = myManageHook
    , layoutHook  = showWName' myShowWNameTheme $  -- Show workspace name when switching 
                    gaps [(U, 5), (D, 5), (R, 5), (L, 5)] $ 
                    smartBorders $ 
                    avoidStruts $ 
                    myLayout 
    , focusedBorderColor = focdBord 
    , normalBorderColor = normBord 
    } 

        `additionalKeysP`
    [ -- Window Management
      ("M-q", kill)  -- Close the focused window
    , ("M-<Return>", spawn "kitty")  -- Open terminal
    , ("M-z", spawn "/home/j3ll0/.config/rofi/launchers/type-4/launcher.sh")  -- Open application launcher
    , ("M-d", spawn "/home/j3ll0/.config/scripts/dmenu.sh")  -- Alternative launcher

    -- Window Resizing & Swapping
    , ("M-C-<Left>", sendMessage Shrink)  -- Shrink the master area
    , ("M-C-<Right>", sendMessage Expand)  -- Expand the master area
    , ("<KP_Delete>", withFocused $ windows . W.sink)  -- Push window back into tiling
    , ("M-C-<Down>", windows W.swapDown)  -- Swap the focused window with the next window
    , ("M-C-<Up>", windows W.swapUp)  -- Swap the focused window with the previous window

    -- Web Browsers
    , ("M-w", spawn "vivaldi")  -- Open Vivaldi browser
    , ("M-S-w", spawn "microsoft-edge")  -- Open Edge browser

    -- Programs
    , ("M-c", spawn "code")
    -- Evolution Email
    , ("M-e", spawn "evolution")
    -- OneNote
    , ("M-o", spawn "p3x-onenote")
    -- Rclone Browser
    , ("M-r", spawn "rclone-browser")
    -- Thunar
    , ("M-t", spawn "thunar") 
    -- QEMU Virt-Manager
    , ("M-v", spawn "virt-manager") 

    -- Places
    , ("<XF86Launch1>", spawn "/home/j3ll0/.config/scripts/dwmfolders.sh")  -- Open places menu

    -- Picom
    , ("<KP_Multiply>", spawn "/home/j3ll0/.config/scripts/picom-toggle.sh")  -- Picom

    -- Pywal Wallpaper
    , ("<XF86AudioMicMute>", spawn "/home/j3ll0/.config/scripts/wallpaper.sh")  -- Pywal

    -- Power Menu
    --, ("M-x", spawn "/home/j3ll0/.config/scripts/power_rofi.sh")  -- Open power menu

    -- Recompile 
    , ("<KP_Add>", spawn "exec kitty -e /etc/nixos/nix_update.sh")  -- Recompile XMonad

    -- Screen Management
    , ("M-<F9>", spawn "arandr")  -- Open screen management tool
    , ("<F9>", spawn "/home/j3ll0/.config/scripts/redshift.sh")

    , ("<XF86DisplayOff>", spawn "/home/j3ll0/.config/scripts/brightOFF.sh")  -- Turn off brightness
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")  -- Decrease brightness
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +5%")  -- Increase brightness

    -- Volume Control
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")  -- Mute volume
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")  -- Lower volume
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")  -- Raise volume

    -- Key Hints
    --, ("M-F5", spawn "/home/j3ll0/.config/polybar/scripts/keyhintdwm.sh")  -- Show key hints
    --, ("M-S-F5", spawn "/home/j3ll0/.config/polybar/scripts/keyhintvim.sh")  -- Show Vim key hints

    -- Display Power Management
    , ("<F11>", spawn "xset dpms force off")  -- Turn off display

    -- Multimedia Keys
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")  -- Play/Pause media
    , ("<XF86AudioNext>", spawn "playerctl next")  -- Next track
    , ("<XF86AudioPrev>", spawn "playerctl previous")  -- Previous track
    , ("<XF86AudioStop>", spawn "playerctl stop")  -- Stop playback
    ]
