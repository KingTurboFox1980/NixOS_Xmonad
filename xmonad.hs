-- | XMonad Configuration
------------------------------------------------------------------------

-- System & Base
import System.IO
import Graphics.X11.ExtraTypes.XF86
import Control.Monad (liftM2)
import Data.Char (ord)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad

-- Actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Actions.OnScreen

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (docks, manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog, doLower)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, fullscreenEventHook)
import XMonad.ManageHook
import XMonad.Config.Desktop (desktopConfig) -- For myBaseConfig

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ShowWName
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??), Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS)) -- Corrected (GHC 9.x fix)
import XMonad.Layout.CenteredMaster(centerMaster)
import XMonad.Layout.CircleEx (CircleEx)
import XMonad.Layout.NoBorders -- FIX: Imports 'noBorders' and 'smartBorders'

-- Utilities
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook, javaHack, trayerAboveXmobarEventHook, trayAbovePanelEventHook, trayerPaddingXmobarEventHook, trayPaddingXmobarEventHook, trayPaddingEventHook)
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.ClickableWorkspaces (clickablePP)

------------------------------------------------------------------------
-- Configuration Variables
------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod4Mask -- Use Super key

encodeCChar :: String -> [Int]
encodeCChar = map ord . UTF8.encodeString

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 2

-- Colors
normBord :: String
normBord = "#100C08"
focdBord :: String
focdBord = "#FF9580"

-- Workspaces
myWorkspaces :: [String]
myWorkspaces    = ["<1> HOME","<2> VM","<3> E-MAIL","<4> WEB","<5> CODE","<6> TORRENT","<7> OTHER","<8> MEDIA","<9> FILES"]

myBaseConfig = desktopConfig

-- Theme for showWName
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font = "xft:Space Age:size=75"
    , swn_fade     = 0.5
    , swn_bgcolor = "#100C08"
    , swn_color    = "#FF9580"
    }

------------------------------------------------------------------------
-- Window Management / Manage Hook & Helpers
------------------------------------------------------------------------

-- Helper to shift the window and immediately view the new workspace (for ManageHook)
doShiftAndGo :: String -> ManageHook
doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws

-- Helper for keybindings: Moves focused window to a workspace AND switches view (for myKeys)
-- CORRECTED: The composition is now W.greedyView ws . W.shift ws, ensuring
-- the window is shifted first, and then the view is switched to the new workspace.
shiftAndFollowKey ws = W.greedyView ws . W.shift ws

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [isFullscreen --> doFullFloat, manageDocks]
    -- Auto-shift and view rules
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "<1> HOME" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "<2> VM" | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "<3> E-MAIL" | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "<4> WEB" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "<5> CODE" | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "<6> TORRENT" | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "<7> OTHER" | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "<8> MEDIA" | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "<9> FILES" | x <- my9Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "OTHER" | x <- my10Shifts] -- Changed from "" to "OTHER"
    , [className =? "trayer" --> doIgnore]
    , [className =? "Polybar" --> doLower]
    ]
    where
    myCFloats = ["Arandr", "kitty", "Galculator", "missioncenter", ".arandr-wrapped", "qBittorrent", "Org.gnome.Totem", "glide", "org.gnome.Terminal", "feh", "mpv", "Xfce4-terminal"]
    myTFloats = ["Downloads", "Save As...", "vlc"]
    myRFloats = []
    myIgnores = ["desktop_window"]
    my1Shifts = []
    my2Shifts = [".virt-manager-wrapped"]
    my3Shifts = ["Org.gnome.Evolution"]
    my4Shifts = ["Chromium", "Vivaldi-stable", "Firefox", "Microsoft-edge", "floorp", "zen", "Navigator"]
    my5Shifts = ["code", "kate", "geany"]
    my6Shifts = ["qBittorrent"]
    my7Shifts = []
    my8Shifts = ["vlc", "freetube", "red-app", "mpv", "tartube","Org.gnome.Totem", "glide"]
    -- Applied user's remembered configuration: xfce.thunar, xfce.thunar-archive-plugin, and xfce.thunar-volman
    my9Shifts = ["Thunar", "rclone-browser", "xfce.thunar", "xfce.thunar-archive-plugin", "xfce.thunar-volman"] 
    my10Shifts = ["discord"]

------------------------------------------------------------------------
-- Layouts
------------------------------------------------------------------------

tiled :: Tall Window
tiled = Tall nmaster delta tiled_ratio
  where
    nmaster = 1 -- number of windows in the master pane
    delta = 3/100 -- percentage of screen to increment by when resizing panes
    tiled_ratio = 1/2 -- initial ratio of master pane to rest

myLayout = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $
             avoidStruts $
             mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
             tiled |||
             Mirror tiled |||
             spiral (6/7) |||
             ThreeColMid 1 (3/100) (1/2) |||
             Full |||
             Tall 1 (3/100) (1/2) |||
             Mirror (Tall 1 (3/100) (1/2)) |||
             noBorders (fullscreenFull Full)

------------------------------------------------------------------------
-- Log Hook (The Clickable Fix)
------------------------------------------------------------------------

myXmobarCommand :: String
myXmobarCommand = "xmobar ~/.config/xmobar/xmobarrc"

-- 1. Define a base PP with all visual customizations.
myPPBase :: Handle -> PP
myPPBase h = xmobarPP
    { ppOutput          = hPutStrLn h 
    , ppCurrent         = xmobarColor "#9580FF" "" . wrap "<box type=Bottom width=3 color=#9580FF>" "</box>"
    , ppVisible         = xmobarColor "#ECBE7B" ""
    , ppHidden          = xmobarColor "#82AAFF" ""                     -- Workspaces with windows
    , ppHiddenNoWindows = \_ -> ""                                    -- *** THIS IS THE ONLY LINE FOR EMPTY WORKSPACES ***
    , ppTitle           = xmobarColor "#FF9580" "" . shorten 250
    , ppLayout          = xmobarColor "#9580FF" ""
    , ppSep             = " | "
    }

-- 2. Define the log hook using monadic bind (>>=) to correctly sequence the actions.
-- This applies clickablePP (an X PP action) and passes the result to dynamicLogWithPP.
myLogHook :: Handle -> X ()
myLogHook h = clickablePP (myPPBase h) >>= dynamicLogWithPP

------------------------------------------------------------------------
-- Startup Hook
------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "~/.config/xmonad/scripts/autostart.sh &"
    setWMName "LG3D"

------------------------------------------------------------------------
-- Key Bindings
------------------------------------------------------------------------

-- Key Bindings Configuration (minor fix to workspace switching)
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- SUPER + FUNCTION KEYS
    [ ((modMask, xK_e), spawn $ "atom" )
    , ((modMask .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    , ((modMask, xK_q), kill )
    , ((modMask, xK_r), spawn $ "rclone-browser" )
    , ((modMask, xK_d), spawn $ "exec ~/.config/scripts/dmenu.sh" )
    , ((0, xK_F10), spawn "polybar-msg cmd toggle" )
    , ((modMask, xK_x), spawn $ "archlinux-logout" )
    , ((modMask, xK_F1), spawn $ "vivaldi-stable" )
    , ((modMask, xK_F7), spawn $ "virt-manager" )
    , ((modMask, xK_F8), spawn $ "thunar" )
    , ((modMask, xK_p), spawn $ "rofi -theme-str 'window {width: 100%;height: 100%;}' -show drun" )
    , ((modMask, xK_space), sendMessage NextLayout)
    , ((0, xF86XK_Calculator), spawn $ "galculator" )
    , ((modMask, xK_o), spawn $ "p3x-onenote" )
    , ((modMask, xK_c), spawn $ "code" )
    , ((modMask, xK_e), spawn $ "exec evolution" )
    , ((modMask, xK_g), spawn $ "geany" )
    , ((modMask, xK_v), spawn $ "virt-manager" )
    , ((modMask, xK_w), spawn $ "vivaldi" )
    , ((modMask, xK_z), spawn $ "exec ~/.config/rofi/launchers/type-6/launcher.sh" )
    , ((modMask .|. shiftMask , xK_w ), spawn $ "flatpak run com.microsoft.Edge" )
    , ((modMask, xK_x), spawn $ "~/.config/scripts/powermenu/powermenu.sh" )
    , ((modMask, xK_Return), spawn $ "kitty" )
    , ((modMask, xK_Escape), spawn $ "missioncenter" )
    , ((0, xK_KP_Subtract), spawn $ "exec ~/.config/scripts/shortcut_key_scripts.sh" )
    , ((0, xK_KP_Divide), spawn $ "exec ~/.config/polybar/launch.sh" )
    , ((0, xK_KP_Add), spawn $ "exec ~/.config/scripts/wallpaper.sh" )
    , ((modMask, xK_KP_Add), spawn $ "exec ~/.config/scripts/select_wallpaper_pywal.sh" )
    , ((modMask, xK_t), spawn $ "thunar" )
    , ((0, xK_KP_Multiply), spawn $ "exec ~/.config/scripts/help.sh")
    , ((modMask, xK_KP_Multiply), spawn $ "exec ~/.config/polybar/scripts/keyhintarco.sh")
    , ((shiftMask, xK_KP_Multiply), spawn $ "exec ~/.config/polybar/scripts/keyhintvim.sh")
    , ((0, xK_F9), spawn $ "exec ~/.config/scripts/redshift.sh" )
    , ((0, xK_F6), spawn $ "exec ~/.config/scripts/screenoff.sh" )
    , ((modMask, xK_s), spawn $ "exec ~/.config/scripts/dmenu-websearch.sh" )
    -- Shrink/Expand
    , ((shiftMask .|. controlMask, xK_Left), sendMessage Shrink)
    , ((shiftMask .|. controlMask, xK_Right), sendMessage Expand)
    -- Sink/Swap
    , ((0, xK_KP_Delete), withFocused $ windows . W.sink)
    , ((shiftMask .|. controlMask, xK_Down), windows W.swapDown)
    , ((shiftMask .|. controlMask, xK_Up), windows W.swapUp)
    -- SUPER + SHIFT KEYS
    , ((modMask .|. shiftMask , xK_d ), spawn $ "dmenu_run -i -nb '#191919' -nf '#fea63c' -sb '#fea63c' -sf '#191919' -fn 'NotoMonoRegular:bold:pixelsize=14'")
    , ((modMask .|. shiftMask , xK_r ), spawn $ "xmonad --recompile && xmonad --restart")
    , ((modMask .|. shiftMask , xK_q ), kill)
    , ((modMask, xK_x ), spawn $ "~/.config/scripts/powermenu/powermenu.sh")
    -- CONTROL + ALT KEYS
    , ((controlMask .|. mod1Mask , xK_Next ), spawn $ "conky-rotate -n")
    , ((controlMask .|. mod1Mask , xK_Prior ), spawn $ "conky-rotate -p")
    , ((controlMask .|. mod1Mask , xK_a ), spawn $ "xfce4-appfinder")
    , ((controlMask .|. mod1Mask , xK_b ), spawn $ "thunar")
    , ((controlMask .|. mod1Mask , xK_a ), spawn $ "catfish") -- This binding is duplicated with xfce4-appfinder
    , ((controlMask .|. mod1Mask , xK_f ), spawn $ "firefox")
    , ((controlMask .|. mod1Mask , xK_i ), spawn $ "nitrogen")
    , ((controlMask .|. mod1Mask , xK_p ), spawn $ "~/.config/scripts/picom-toggle.sh")
    , ((controlMask .|. mod1Mask , xK_r ), spawn $ "rofi-theme-selector")
    , ((controlMask .|. mod1Mask , xK_u ), spawn $ "pavucontrol")
    --SCREENSHOTS
    , ((0, xK_Print), spawn $ "org.flameshot.Flameshot gui" )
    --MULTIMEDIA KEYS
    , ((0, xF86XK_AudioMute), spawn "$HOME/.config/scripts/volume.sh --toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "$HOME/.config/scripts/volume.sh --dec")
    , ((0, xF86XK_AudioRaiseVolume), spawn "$HOME/.config/scripts/volume.sh --inc")
    , ((0, xF86XK_MonBrightnessUp),  spawn $ "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown), spawn $ "xbacklight -dec 5")
    , ((0, xF86XK_AudioPlay), spawn $ "playerctl play-pause")
    , ((0, xF86XK_AudioNext), spawn $ "playerctl next")
    , ((0, xF86XK_AudioPrev), spawn $ "playerctl previous")
    , ((0, xF86XK_AudioStop), spawn $ "playerctl stop")
    -- XMONAD LAYOUT KEYS
    , ((mod1Mask, xK_Tab), nextWS)
    , ((modMask, xK_Tab), nextWS)
    , ((controlMask .|. mod1Mask , xK_Left ), prevWS)
    , ((controlMask .|. mod1Mask , xK_Right ), nextWS)
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_j), windows W.focusDown)
    , ((modMask, xK_k), windows W.focusUp)
    , ((modMask .|. shiftMask, xK_m), windows W.focusMaster)
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown)
    , ((controlMask .|. modMask, xK_Down), windows W.swapDown)
    , ((controlMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))
    , ((controlMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))
    ]
    ++
    -- Workspace Switching: Mod + Num = View; Mod + Shift + Num = Move & Follow
    [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9]
      , (f, m) <- [(W.greedyView, 0), (shiftAndFollowKey, shiftMask)]] -- Corrected function used here

    ++
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_Right, xK_Left] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse Bindings
------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, 1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modMask, 2), (\w -> focus w >> windows W.shiftMaster))
    , ((modMask, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

------------------------------------------------------------------------
-- Main Function
------------------------------------------------------------------------

main :: IO ()
main = do
    -- 1. Create the pipe and launch xmobar
    xmproc <- spawnPipe myXmobarCommand

    xmonad . ewmhFullscreen . ewmh $
        myBaseConfig
        { startupHook = myStartupHook
       
        -- 2. Pass the pipe handle to the corrected log hook function
        , logHook = myLogHook xmproc
       
        , layoutHook = showWName' myShowWNameTheme $
                         gaps [(U, 10), (D, 10), (R, 5), (L, 5)] $
                         smartBorders $
                         avoidStruts $
                         (myLayout ||| layoutHook myBaseConfig)

        , manageHook = manageSpawn <+> myManageHook <+> manageHook myBaseConfig
        , modMask = myModMask
        , borderWidth = myBorderWidth
        , handleEventHook = windowedFullscreenFixEventHook <+> handleEventHook myBaseConfig
        , focusFollowsMouse = myFocusFollowsMouse
        , workspaces = myWorkspaces
        , focusedBorderColor = focdBord
        , normalBorderColor = normBord
        , keys = myKeys
        , mouseBindings = myMouseBindings
        }
