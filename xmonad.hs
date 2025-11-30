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
import XMonad.Actions.CycleWS              -- Required for toggleWS, prevWS, shiftToNext/Prev
import XMonad.Actions.DwmPromote
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
import XMonad.Layout.MultiColumns ( multiCol )
import XMonad.Layout.Tabbed ( simpleTabbed )
import XMonad.Layout.ThreeColumns ( ThreeCol (ThreeCol, ThreeColMid) )
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

myTerminal      = "kitty"

encodeCChar :: String -> [Int]
encodeCChar = map ord . UTF8.encodeString

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 2

------------------------------------------------------------------------
-- Colors --
------------------------------------------------------------------------
normBord = "#100c08"
focdBord = "#9580ff"
fore     = "#9580ff"
back     = "#282A36"
winType  = "#BD93F9"

-- Workspaces
myWorkspaces :: [String]
myWorkspaces    = ["❶ HOME ❶","❷ VM ❷","❸ E-MAIL ❸","❹ WEB ❹","❺ CODE ❺","❻ TORRENT ❻","❼ OTHER ❼","❽ MEDIA ❽","❾ FILES ❾"]

myBaseConfig = desktopConfig

---------------------------------------------------------------------------------
-- Theme for showWName which prints current workspace when you change workspaces.
---------------------------------------------------------------------------------
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
-- Imports should be at the top of the file!

-- Helper to shift the window and immediately view the new workspace (for ManageHook)
doShiftAndGo :: String -> ManageHook
doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws

-- Helper to shift the window, immediately view the new workspace, AND center-float it
doShiftGoAndCenterFloat :: String -> ManageHook
doShiftGoAndCenterFloat ws = doShiftAndGo ws <+> doCenterFloat

-- Helper for keybindings: Moves focused window to a workspace AND switches view (for myKeys)
-- CORRECTED: The composition is now W.greedyView ws . W.shift ws, ensuring
-- the window is shifted first, and then the view is switched to the new workspace.
shiftAndFollowKey ws = W.greedyView ws . W.shift ws


myManageHook :: ManageHook
myManageHook = composeAll . concat $
    -- 1. Auto-shift and view rules (Prioritized: Run these first to move windows to the correct workspace)
    [ 
      -- EXPLICIT RULE: Shift to ❻ TORRENT AND Center Float qBittorrent
      [ (className =? "qBittorrent" <||> resource =? "qbittorrent") --> doShiftGoAndCenterFloat "❻ TORRENT ❻" ]

    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "❶ HOME ❶" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "❷ VM ❷" | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "❸ E-MAIL ❸" | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "❹ WEB ❹" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "❺ CODE ❺" | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "❻ TORRENT ❻" | x <- my6Shifts] 
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "❼ OTHER ❼" | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "❽ MEDIA ❽" | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "❾ FILES ❾" | x <- my9Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "OTHER" | x <- my10Shifts]

    -- 2. Floating rules (Applied to windows that were not shifted)
    , [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats] -- Classes that center-float
    , [title =? t --> doFloat | t <- myTFloats]          -- Titles that float
    , [resource =? r --> doFloat | r <- myRFloats]        -- Resources that float

    -- 3. Fullscreen and Docks
    , [isFullscreen --> (doF W.focusDown <+> doFullFloat)] -- Ensures focus remains correct when going fullscreen
    , [manageDocks] -- Standard dock management

    -- 4. Ignore/Lower rules
    , [className =? i --> doIgnore | i <- myIgnores]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [className =? "Polybar" --> doLower]
    ]

    where
    -- Classes that should float (but NOT shift)
    myCFloats =
        [ "Arandr", "kitty", "Galculator", "missioncenter", ".arandr-wrapped"
        , "Org.gnome.Totem", "glide", "org.gnome.Terminal", "feh", "mpv", "rclone-browser"
        , "Xfce4-terminal", "Steam", "Gimp", "MPlayer"
        ]
    -- Titles that should float
    myTFloats = ["Downloads", "Save As..."]
    -- Resources that should float
    myRFloats = ["gpicview"]

    -- Classes/Resources/Titles that should be ignored by the window manager
    myIgnores =
        [ "desktop_window", "Conky", "system_conky"
        , "trayer"
        ]

    -- Shift rules for ❶ HOME ❶
    my1Shifts = []
    -- Shift rules for ❷ VM ❷ (Removed from myCFloats to ensure shifting works)
    my2Shifts = [".virt-manager-wrapped"]
    -- Shift rules for ❸ E-MAIL ❸
    my3Shifts = ["Org.gnome.Evolution", "eu.betterbird.Betterbird"]
    -- Shift rules for ❹ WEB ❹
    my4Shifts =
        [ "Chromium", "Vivaldi-stable", "Firefox", "Microsoft-edge"
        , "floorp", "zen", "Navigator"
        ]
    -- Shift rules for ❺ CODE ❺
    my5Shifts = ["code", "kate", "geany", "Geany", "Code"] -- Covers multiple casing/names for Code/Geany
    -- Shift rules for ❻ TORRENT ❻
    my6Shifts = [] -- qBittorrent is handled by the explicit rule above
    -- Shift rules for ❼ OTHER ❼
    my7Shifts = []
    -- Shift rules for ❽ MEDIA ❽ (Removed 'vlc' from myTFloats to ensure shifting works)
    my8Shifts = ["vlc", "freetube", "red-app", "mpv", "tartube", "Totem", "glide"]
    -- Shift rules for ❾ FILES ❾ (Including specific Thunar components)
    my9Shifts =
        [ "Thunar", "rclone-browser"
        , "xfce.thunar", "xfce.thunar-archive-plugin", "xfce.thunar-volman"
        ]
    -- Shift rules for OTHER
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

-- ** Inner Gaps (Spacing between windows) set to 8 pixels **
myLayout = spacingRaw True (Border 0 8 8 8) True (Border 8 8 8 8) True $
             avoidStruts $
             mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
             tiled |||
             Mirror tiled |||
             spiral (6/7) |||
             ThreeColMid 1 (3/100) (1/2) |||
             Full |||
             Tall 1 (3/100) (1/2) |||
             Mirror (Tall 1 (3/100) (1/2)) |||
             multiCol [1] 1 0.01 (-0.5) |||
             ThreeCol nmaster delta (1/3) |||
             simpleTabbed |||
             noBorders (fullscreenFull Full)
  where
      -- Default tiling algorithm partitions the screen into two panes
      tiled   = Tall nmaster delta ratio

      -- Number of windows in the master pane
      nmaster = 1

      -- Proportion of screen occupied by master pane
      ratio   = 1/2

      -- Percent of screen to increment by when resizing panes
      delta   = 3/100

------------------------------------------------------------------------
-- Log Hook
------------------------------------------------------------------------

myXmobarCommand :: String
myXmobarCommand = "xmobar ~/.config/xmobar/xmobarrc"

-- 1. Define a base PP with all visual customizations.
-- NOTE: This snippet assumes 'windowCount' is defined elsewhere in your XMonad config
-- and that necessary imports (like XMonad.Hooks.DynamicLog, XMonad.Util.Run) are present.
myPPBase :: Handle -> PP
myPPBase h = xmobarPP
    { ppOutput          = hPutStrLn h
    , ppCurrent = xmobarColor "#282C34" "#9580FF" . wrap " " " " -- Text color on Background color
    , ppVisible         = xmobarColor "#ECBE7B" "" . wrap "(" ")"          -- Visible: Wrapped in ( )
    -- CHANGE: Removed the 'wrap' function and the box definition here.
    , ppHidden          = xmobarColor "#44475A" ""
    , ppHiddenNoWindows = \_ -> ""                                           -- Empty: No display
    , ppUrgent          = xmobarColor "#cc0000" "" . wrap "{" "}"          -- Urgent: Wrapped in { }
    , ppTitle           = xmobarColor "cadetblue3" "" . shorten 250
    -- CORRECTED: Use nested fmap to apply xmobarColor to the String,
    -- handling both the X monad and the Maybe wrapper.
    , ppExtras          = [fmap (fmap (xmobarColor "#FF9580" "")) windowCount]
    , ppLayout          = xmobarColor "#6272A4" ""
    , ppSep             = "<fc=#666666>|</fc>"                              -- Separators in xmobar
    -- ppOrder: [windowCount, workspaces, layout, title]
    , ppOrder           = \(ws:l:t:ex) -> ex++[ws,l,t]
    }

-- 2. Define the log hook using clickable workspaces.
myLogHook :: Handle -> X ()
myLogHook h = clickablePP (myPPBase h) >>= dynamicLogWithPP

windowCount :: X (Maybe String)
windowCount = gets $ Just . (++ " ") . ((++) "🪟 ") . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
-- Breakdown:
-- 1. `W.integrate' . W.stack . W.workspace . W.current . windowset`: Gets the list of windows on the current workspace.
-- 2. `length`: Counts the number of windows.
-- 3. `show`: Converts the number (Int) to a String.
-- 4. `((++) "\xe5c3 ")`: Prepends the icon and a space (e.g., "󰗃 3").
-- 5. `(++ " ")`: Appends another space for padding (e.g., "󰗃 3 ").
-- 6. `Just`: Wraps the resulting string in the Maybe Monad.

------------------------------------------------------------------------
myAppGrid = [ ("Telegram-desktop", "telegram-desktop")
                 , ("VScode", "code-insiders")
                 , ("Godot", "godot")
                 , ("Firefox", "firefox")
                 , ("VirtualBox", "virtualbox")
                 , ("Kdenlive", "kdenlive")
                 , ("WPS Writer", "wps")
                 , ("WPS Sheets", "et")
                 , ("WPS Presentation", "wpp")
                 , ("OBS", "obs")
                 , ("GitHub Desktop", "github-desktop")
                 , ("Control Center", "systemsettings5") -- this on kde plasma 5 it could have a different command on another DE
                 , ("Firefox Dev", "firefox-bin")
                 ]

------------------------------------------------------------------------
-- Startup Hook
------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "~/.config/xmonad/scripts/autostart.sh &"
    setWMName "LG3D"

------------------------------------------------------------------------
-- Move Windows Between Monitors --
------------------------------------------------------------------------
shiftToScreen :: ScreenId -> X ()
shiftToScreen sc = do
  ws <- gets windowset
  case lookup sc $ zip [0 ..] (W.screens ws) of
    Just screen -> windows $ W.shift (W.tag $ W.workspace screen)
    Nothing     -> return ()

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
    , ((mod1Mask, xK_space), spawn $ "rofi -theme-str 'window {width: 100%;height: 100%;}' -show drun" )
    , ((controlMask, xK_space), sendMessage NextLayout)
    , ((0, xF86XK_Calculator), spawn $ "galculator" )
    , ((modMask, xK_o), spawn $ "p3x-onenote" )
    , ((modMask, xK_c), spawn $ "code" )
    -- , ((modMask, xK_e), spawn $ "exec evolution" ) -- commented out 'e' to avoid conflict with 'atom'
    , ((modMask, xK_b), spawn $ "exec flatpak run eu.betterbird.Betterbird" )
    , ((modMask, xK_g), spawn $ "geany" )
    , ((modMask, xK_v), spawn $ "virt-manager" )
    , ((modMask, xK_w), spawn $ "vivaldi" )
    , ((modMask, xK_space), spawn $ "exec ~/.config/rofi/launchers/type-6/launcher.sh" )
    , ((modMask .|. shiftMask , xK_w ), spawn $ "flatpak run com.microsoft.Edge" )
    , ((modMask, xK_x), spawn $ "~/.config/scripts/powermenu/powermenu.sh" )
    , ((modMask, xK_Return), spawn $ "kitty" )
    , ((modMask, xK_Escape), spawn $ "missioncenter" )
    
    -- *** RESTORED SCRIPT KEYBINDINGS ***
    , ((0, xK_KP_Subtract), spawn $ "exec ~/.config/scripts/shortcut_key_scripts.sh" ) 
    , ((modMask, xK_KP_Subtract), spawn $ "exec kitty -e ~/nix_update.sh" )
    , ((modMask, xK_p), spawn $ "exec ~/.config/polybar/launch.sh" )              
    , ((0, xK_KP_Add), spawn $ "exec ~/.config/scripts/wallpaper.sh" )               
    , ((modMask, xK_KP_Add), spawn $ "exec ~/.config/scripts/select_wallpaper_pywal.sh" ) 
    , ((0, xK_KP_Multiply), spawn $ "exec ~/.config/scripts/help.sh")
    
    -- *** GAP CONTROL KEYBINDINGS ***
    , ((0, xK_KP_Divide), incWindowSpacing 2)           
    , ((shiftMask, xK_KP_Divide), incWindowSpacing (-2))    
    , ((modMask, xK_KP_Divide), setWindowSpacing (Border 0 0 0 0)) 

    , ((modMask, xK_t), spawn $ "thunar" )
    , ((shiftMask, xK_KP_Multiply), spawn $ "exec ~/.config/polybar/scripts/keyhintvim.sh")
    , ((0, xK_F9), spawn $ "exec ~/.config/scripts/redshift.sh" )
    , ((0, xK_F6), spawn $ "exec ~/.config/scripts/screenoff.sh" )
    , ((modMask, xK_s), spawn $ "exec ~/.config/scripts/dmenu-websearch.sh" )

    -- ----------------------------------------------------------------------
    -- ** NEW CYCLEWS & FOLLOW BINDINGS **
    -- ----------------------------------------------------------------------

    -- Toggle Workspace (Mod + Backtick)
    , ((modMask, xK_grave), toggleWS)

    -- Cycle Previous Workspace (Mod + Shift + Tab)
    , ((modMask .|. shiftMask, xK_Tab), prevWS)

    -- Shift Window to Next Workspace and Follow (Mod + Ctrl + J)
    , ((modMask .|. controlMask, xK_j), shiftToNext >> nextWS)

    -- Shift Window to Previous Workspace and Follow (Mod + Ctrl + K)
    , ((modMask .|. controlMask, xK_k), shiftToPrev >> prevWS)

    -- ----------------------------------------------------------------------
    -- ** EXISTING MOVEMENT & LAYOUT BINDINGS **
    -- ----------------------------------------------------------------------

    -- Shrink/Expand
    , ((shiftMask .|. controlMask, xK_Left), sendMessage Shrink)
    , ((shiftMask .|. controlMask, xK_Right), sendMessage Expand)
    -- Sink/Swap
    , ((0, xK_KP_Delete), withFocused $ windows . W.sink)
    , ((shiftMask .|. controlMask, xK_Down), windows W.swapDown)
    , ((shiftMask .|. controlMask, xK_Up), windows W.swapUp)
    -- SUPER + SHIFT KEYS
    , ((modMask .|. shiftMask , xK_r ), spawn $ "xmonad --recompile && xmonad --restart")
    , ((modMask .|. shiftMask , xK_q ), kill)
    , ((modMask, xK_x ), spawn $ "~/.config/scripts/powermenu/powermenu.sh")
    -- CONTROL + ALT KEYS
    , ((controlMask .|. mod1Mask , xK_Next ), spawn $ "conky-rotate -n")
    , ((controlMask .|. mod1Mask , xK_Prior ), spawn $ "conky-rotate -p")
    , ((controlMask .|. mod1Mask , xK_b ), spawn $ "thunar")
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
    -- XMONAD LAYOUT KEYS (Keep your existing cycle keys)
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
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
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
                         -- ** Outer Gaps (Screen Edge Padding) set to 5 pixels **
                         gaps [(U, 5), (D, 5), (R, 5), (L, 5)] $
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