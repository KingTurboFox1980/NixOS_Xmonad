{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wextra #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -XOverloadedStrings #-}
{-# OPTIONS_GHC -XLambdaCase #-}

-- DRACULA COLOR THEME = Line 149 --
-- KEYBINDINGS = Line 553 --
-- SCRATCHPADS = Line 373 / 628 --
-- XMOBAR = Line 456 -- 

--------------------------
-- | XMonad Configuration
--------------------------

-- System & Base
import System.IO
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib.Extras (getWindowProperty32)
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
import XMonad.Actions.GridSelect
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.WithAll
import XMonad.Actions.CopyWindow
import XMonad.Actions.MouseResize

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (docks, manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog, doLower)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, fullscreenEventHook)
import XMonad.ManageHook
import XMonad.Config.Desktop (desktopConfig) -- For myBaseConfig
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.WindowSwallowing

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ShowWName
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.Tabbed (tabbed, shrinkText, simpleTabbed)
import XMonad.Layout.Decoration (Theme(..))
import Data.Default (def)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeCol, ThreeColMid))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??), Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.CircleEx (CircleEx)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.LimitWindows
import XMonad.Layout.SubLayouts
import XMonad.Layout.Decoration (Theme(..))
import Data.Default (def)

-- Utilities
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, additionalMouseBindings)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook, javaHack, trayerAboveXmobarEventHook, trayAbovePanelEventHook, trayerPaddingXmobarEventHook, trayPaddingXmobarEventHook, trayPaddingEventHook)
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn)

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
-- Urgency notification (WM_URGENT → notify-send)
------------------------------------------------------------------------

data LibNotifyUrgencyHook = LibNotifyUrgencyHook
  deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    mIdx <- W.findTag w <$> gets windowset
    case mIdx of
      Just idx ->
        safeSpawn "notify-send"
          [ "Urgent: " ++ show name
          , "Workspace " ++ idx
          ]
      Nothing ->
        return ()

------------------------------------------------------------------------
-- Show PID of focused window (copies to clipboard)
------------------------------------------------------------------------

showActivePid :: X ()
showActivePid = withFocused $ \w -> do
  d <- asks display
  netWmPid <- getAtom "_NET_WM_PID"
  name     <- getName w
  mpid     <- io $ getWindowProperty32 d netWmPid w

  case mpid of
    Just (p:_) -> do
      -- copy PID to clipboard
      spawn $ "printf '%s' " ++ show p ++ " | xsel -ib"
      -- notify user
      safeSpawn "notify-send" [show name, "pid " ++ show p]

    Nothing ->
      safeSpawn "notify-send" ["XMonad", "focused window has no PID"]

------------------------------------------------------------------------
-- Dracula Color Palette
------------------------------------------------------------------------
dracBg      = "#282A36"
dracFg      = "#F8F8F2"

dracBlack   = "#21222C"
dracGray    = "#44475A"
dracRed     = "#FF5555"
dracGreen   = "#8AFF80"
dracPurple1 = "#6D128D"
dracPurple2 = "#9580FF"
dracPink    = "#FF79C6"
dracOrange  = "#FF9580"

------------------------------------------------------------------------
-- Borders / Common UI
------------------------------------------------------------------------
normBord = dracBlack
focdBord = dracOrange
fore     = dracFg
back     = dracBg
winType  = dracPurple1

------------------------------------------------------------------------
-- Semantic UI Colors
------------------------------------------------------------------------
colActive   = dracPurple1
colInactive = dracGray
colUrgent   = dracRed
colFocus    = dracPink

------------------------------------------------------------------------
-- Dracula Theme
------------------------------------------------------------------------
myTheme :: Theme
myTheme = def
  { activeColor         = colActive
  , inactiveColor       = colInactive
  , urgentColor         = colUrgent

  , activeTextColor     = dracBg
  , inactiveTextColor   = dracFg
  , urgentTextColor     = dracBg

  , activeBorderColor   = colFocus
  , inactiveBorderColor = dracBlack
  , urgentBorderColor   = colUrgent

  , fontName            = "xft:JetBrainsMono Nerd Font:style=Bold:size=10"
  , decoHeight          = 24
  }

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
    [
      -- 1. Consolidated Shift + View Rules
      -- Uses myWorkMap for easy maintenance
      [ (className =? a <||> title =? a <||> resource =? a) --> doShiftAndGo ws
      | (ws, apps) <- myWorkMap
      , a <- apps
      ]

      -- 2. Floating Rules
    , [ isDialog --> doCenterFloat ]
    , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
    , [ className =? c --> doCenterFloat | c <- myCFloats ]
    , [ title     =? t --> doFloat       | t <- myTFloats ]
    , [ resource  =? r --> doFloat       | r <- myRFloats ]
    
      -- 3. XFCE / Thunar Specific Dialog Fixes
    , [ (className =? "Thunar" <&&> title =? "File Operation Progress") --> doCenterFloat ]
    , [ (className =? "Thunar" <&&> title =? "Confirm to replace files") --> doCenterFloat ]

      -- 4. Ignore Rules (Conky, Trayer, etc.)
    , [ (className =? i <||> resource =? i) --> doIgnore | i <- myIgnores ]
    , [ className =? "Polybar" --> doLower ]

      -- 5. Infrastructure
    , [ manageDocks ]
    ]
  where
    -- Your central "Brain" for workspace assignments
    myWorkMap =
        [ ("❶ HOME ❶",    [])
        , ("❷ VM ❷",      [".virt-manager-wrapped"])
        , ("❸ E-MAIL ❸",  ["Org.gnome.Evolution", "eu.betterbird.Betterbird"])
        , ("❹ WEB ❹",     ["Chromium", "Vivaldi-stable", "Firefox", "floorp", "zen", "Navigator", "Microsoft-edge"])
        , ("❺ CODE ❺",    ["code", "Code", "kate", "geany", "Geany"])
        , ("❻ TORRENT ❻", ["qBittorrent"])
        , ("❼ OTHER ❼",   [])
        , ("❽ MEDIA ❽",   ["vlc", "freetube", "red-app", "mpv", "tartube", "Totem", "glide"])
        , ("❾ FILES ❾",   ["Thunar", "rclone-browser", "xfce.thunar", "xfce.thunar-archive-plugin", "xfce.thunar-volman"])
        , ("OTHER",       ["discord"])
        ]

    -- Apps that should always float
    myCFloats = [ "Arandr", "Galculator", "missioncenter", ".arandr-wrapped"
                , "gnome-calculator", "feh", "mpv", "Xfce4-terminal", "Steam"
                , "Gimp", "MPlayer", "Org.gnome.Totem", "glide", "rclone-browser" ]
    
    myTFloats = [ "Downloads", "Save As...", "Extension: (MetaMask)" ]
    
    myRFloats = [ "gpicview" ]

    -- Things for XMonad to ignore entirely
    myIgnores = [ "desktop_window", "Conky", "system_conky", "trayer" ]

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------
myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "terminal"
      "kitty --class scratchpad"
      (className =? "scratchpad")
      (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)

  , NS "mixer"
      "pavucontrol"
      (className =? "Pavucontrol")
      (customFloating $ W.RationalRect 0.2 0.15 0.6 0.7)

  , NS "notes"
      "kitty --class notes -e nvim ~/notes.md"
      (className =? "notes")
      (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    
  , NS "virt"
    "virt-manager --no-fork"
    (className =? ".virt-manager-wrapped")
    (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)

  , NS "vivaldi"
     "vivaldi --class=vivaldi-scratchpad"
     (className =? "vivaldi-scratchpad")
     (customFloating $ W.RationalRect 0.025 0.025 0.95 0.95)

  , NS "zen"
     "flatpak run app.zen_browser.zen"
     (className =? "zen" <||> resource =? "Navigator")
     (customFloating $ W.RationalRect 0.025 0.025 0.95 0.95)

  , NS "onenote"
     "p3x-onenote"
     (className =? "p3x-onenote")
     (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)

  , NS "thunar"
     "thunar --class=thunar-scratchpad"
     (className =? "thunar-scratchpad")
     (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)

  , NS "qbittorrent"
     "qbittorrent"
     (className =? "qBittorrent")
     (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
  ]
  
------------------------------------------------------------------------
-- Layouts
------------------------------------------------------------------------

-- Base Tall layout parameters
nmaster :: Int
nmaster = 1

delta :: Rational
delta = 3/100

ratio :: Rational
ratio = 1/2

-- Primary tiling layout
tiled :: Tall a
tiled = Tall nmaster delta ratio

-- Layout hook
myLayout =
  spacingRaw
    True                  -- enable screen edge gaps
    (Border 0 8 8 8)      -- screen edge gaps (top right bottom left)
    True
    (Border 8 8 8 8)      -- window gaps
    True
  $ avoidStruts
  $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
  $ (
         tiled
     ||| Mirror tiled
     ||| spiral (6/7)
     ||| ThreeColMid 1 delta ratio
     ||| multiCol [1] 1 0.01 (-0.5)
     ||| simpleTabbed
     ||| Full
    )

------------------------------------------------------------------------
-- Log Hook / Xmobar
------------------------------------------------------------------------

myXmobarCommand :: String
myXmobarCommand = "xmobar ~/.config/xmobar/xmobarrc"

-- Pretty-printer for xmobar
myPPBase :: Handle -> PP
myPPBase h = xmobarPP
  { ppOutput          = hPutStrLn h

  -- Workspaces
  -- Current: Solid Purple background (Dracula)
  , ppCurrent         = xmobarColor "#282C34" "#9580FF" . wrap " " " "
  -- Visible: Yellow-ish brackets
  , ppVisible         = xmobarColor "#ECBE7B" ""        . wrap "(" ")"
  -- Hidden: Dimmed Gray (only shows if windows are present)
  , ppHidden          = xmobarColor "#44475A" ""        . wrap " " " "
  , ppHiddenNoWindows = const ""

  -- Urgent workspace: Red alert bell
  , ppUrgent          = xmobarColor "#FFFF80" "#cc0000"
                         . wrap " 🔔 " " 🔔 "

  -- Window title: Limited to 80 chars for better bar space
  , ppTitle           = xmobarColor "cadetblue3" "" . shorten 80
  
  -- Layout
  , ppLayout          = xmobarColor "#6272A4" "" . (\x -> case x of
                          "Spacing Tall"                -> " TALL "
                          "Mirror Spacing Tall"         -> " MIRR "
                          "Spacing Spiral"              -> " SPRL "
                          "Spacing ThreeColMid"         -> " 3COL "
                          "Spacing multiCol"            -> " MCOL "
                          "Spacing simpleTabbed"        -> " TABB "
                          "Spacing Full"                -> " FULL "
                          _                             -> " " ++ x ++ " "
                      )

  -- Separators
  , ppSep             = "<fc=#666666> | </fc>"

  -- Extras (Window Count)
  , ppExtras          = [ fmap (fmap (xmobarColor "#FF9580" "")) windowCount ]

  -- Order: [WindowCount] [Workspaces] [Layout Icon] [Window Title]
  , ppOrder           = \(ws:l:t:ex) -> ex ++ [ws, l, t]
  }

-- Log hook (clickable workspaces + xmobar)
myLogHook :: Handle -> X ()
myLogHook h =
  clickablePP (myPPBase h) >>= dynamicLogWithPP

-- Window count for current workspace
windowCount :: X (Maybe String)
windowCount =
  gets $
    Just
    . (++ " ")
    . ("🪟 " ++)
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset

------------------------------------------------------------------------
-- App Grid
------------------------------------------------------------------------

myAppGrid :: [(String, String)]
myAppGrid =
  [ ("Telegram-desktop", "telegram-desktop")
  , ("VScode",           "code-insiders")
  , ("Godot",            "godot")
  , ("Firefox",          "firefox")
  , ("VirtualBox",       "virtualbox")
  , ("Kdenlive",         "kdenlive")
  , ("WPS Writer",       "wps")
  , ("WPS Sheets",       "et")
  , ("WPS Presentation", "wpp")
  , ("OBS",              "obs")
  , ("GitHub Desktop",   "github-desktop")
  , ("Control Center",   "systemsettings5")
  , ("Firefox Dev",      "firefox-bin")
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
    [ ((modMask .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    , ((modMask, xK_q), kill )
    , ((modMask, xK_r), spawn $ "rclone-browser" )
    , ((modMask, xK_d), spawn $ "exec ~/.config/scripts/dmenu.sh" )
    , ((0, xK_F10), spawn "polybar-msg cmd toggle" )
    --, ((modMask, xK_F1), spawn $ "vivaldi-stable" )
    , ((modMask, xK_F7), spawn $ "virt-manager" )
    --, ((modMask, xK_F8), spawn $ "thunar" )
    , ((mod1Mask, xK_space), spawn $ "rofi -theme-str 'window {width: 100%;height: 100%;}' -show drun" )
    , ((controlMask, xK_space), sendMessage NextLayout)
    , ((0, xF86XK_Calculator), spawn $ "flatpak run org.gnome.Calculator" )
    -- , ((modMask, xK_o), spawn $ "p3x-onenote" )
    , ((modMask, xK_c), spawn $ "code" )
    -- , ((modMask, xK_e), spawn $ "exec evolution" ) -- commented out 'e' to avoid conflict with 'atom'
    , ((modMask, xK_b), spawn $ "exec flatpak run eu.betterbird.Betterbird" )
    , ((modMask, xK_g), spawn $ "flatpak run org.geany.Geany" )
    --, ((modMask, xK_w), spawn $ "vivaldi" )
    , ((modMask, xK_space), spawn $ "exec ~/.config/rofi/launchers/type-6/launcher.sh" )
    , ((modMask .|. shiftMask , xK_w ), spawn $ "flatpak run com.microsoft.Edge" )
    , ((modMask, xK_x), spawn $ "~/.config/scripts/powermenu/powermenu.sh" )
    -- , ((modMask, xK_Return), spawn $ "kitty" )
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

    --, ((modMask, xK_t), spawn $ "thunar" )
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

    , ((modMask .|. shiftMask, xK_Tab), prevWS)
    
    , ((mod1Mask, xK_Tab), nextWS)
    
    , ((modMask, xK_Tab), nextWS)

    , ((modMask .|. controlMask, xK_j), shiftToNext >> nextWS)

    , ((modMask .|. controlMask, xK_k), shiftToPrev >> prevWS)

    -- ----------------------------------------------------------------------
    -- ** Scratchpads **
    -- ----------------------------------------------------------------------
    , ((myModMask, xK_Return), namedScratchpadAction myScratchpads "terminal")
    , ((myModMask, xK_F12), namedScratchpadAction myScratchpads "mixer")
    , ((myModMask .|. shiftMask, xK_grave), namedScratchpadAction myScratchpads "notes")
    , ((modMask, xK_v), namedScratchpadAction myScratchpads "virt")
    , ((myModMask .|. shiftMask, xK_w), namedScratchpadAction myScratchpads "zen")
    , ((modMask, xK_w), namedScratchpadAction myScratchpads "vivaldi")
    , ((modMask, xK_o), namedScratchpadAction myScratchpads "onenote")
    , ((modMask, xK_e), namedScratchpadAction myScratchpads "thunar")
    , ((modMask, xK_t), namedScratchpadAction myScratchpads "qbittorrent")
    
    -- ----------------------------------------------------------------------
    -- ** EXISTING MOVEMENT & LAYOUT BINDINGS **
    -- ----------------------------------------------------------------------

    -- Shrink/Expand
    , ((shiftMask .|. controlMask, xK_Left), sendMessage Shrink)
    , ((shiftMask .|. controlMask, xK_Right), sendMessage Expand)
    -- Sink/Swap
    , ((0, xK_KP_Delete), withFocused $ windows . W.sink)
    , ((modMask, xK_KP_Delete), sendMessage (Toggle NBFULL))
    , ((shiftMask .|. controlMask, xK_Down), windows W.swapDown)
    , ((shiftMask .|. controlMask, xK_Up), windows W.swapUp)
    -- SUPER + SHIFT KEYS
    , ((modMask .|. shiftMask, xK_r), spawn "xmonad --recompile && xmonad --restart || notify-send 'XMonad' 'Recompile failed' -u critical")
    , ((modMask .|. shiftMask , xK_q ), kill)
    -- CONTROL + ALT KEYS
    , ((controlMask .|. mod1Mask , xK_Next ), spawn $ "conky-rotate -n")
    , ((controlMask .|. mod1Mask , xK_Prior ), spawn $ "conky-rotate -p")
    --, ((controlMask .|. mod1Mask , xK_b ), spawn $ "thunar")
    , ((controlMask .|. mod1Mask , xK_f ), spawn $ "firefox")
    , ((controlMask .|. mod1Mask , xK_i ), spawn $ "nitrogen")
    , ((controlMask .|. mod1Mask , xK_p ), spawn $ "~/.config/scripts/picom-toggle.sh")
    , ((controlMask .|. mod1Mask , xK_r ), spawn $ "rofi-theme-selector")
    , ((controlMask .|. mod1Mask , xK_u ), spawn $ "pavucontrol")
    --SCREENSHOTS
    , ((0, xK_Print), spawn $ "flameshot" )
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
-- Window Swallowing Definition
------------------------------------------------------------------------
mySwallowHook = swallowEventHook (className =? "kitty") (return True)

------------------------------------------------------------------------
-- Main Function
------------------------------------------------------------------------
main :: IO ()
main = do
    -- Launch xmobar
    xmproc <- spawnPipe myXmobarCommand

    xmonad
      . withUrgencyHook LibNotifyUrgencyHook
      . ewmhFullscreen
      . ewmh
      $ myBaseConfig
          { startupHook = myStartupHook

          , logHook =
                workspaceHistoryHook
            <+> myLogHook xmproc

          , layoutHook =
                showWName' myShowWNameTheme
              $ gaps [(U, 5), (D, 5), (R, 5), (L, 5)]
              $ smartBorders
              $ avoidStruts
              $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
              $ myLayout
                ||| layoutHook myBaseConfig

          , manageHook =
                namedScratchpadManageHook myScratchpads
            <+> manageSpawn
            <+> myManageHook
            <+> manageHook myBaseConfig

          , handleEventHook =
                windowedFullscreenFixEventHook
            <+> mySwallowHook              -- <--- Added Window Swallowing here
            <+> handleEventHook myBaseConfig

          , modMask            = myModMask
          , borderWidth        = myBorderWidth
          , focusFollowsMouse  = myFocusFollowsMouse
          , workspaces         = myWorkspaces
          , focusedBorderColor = focdBord
          , normalBorderColor  = normBord
          , keys               = myKeys
          , mouseBindings      = myMouseBindings
          }
