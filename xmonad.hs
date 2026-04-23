{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wextra #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -XOverloadedStrings #-}
{-# OPTIONS_GHC -XLambdaCase #-}

-- DRACULA COLOR THEME = Line 159 --
-- KEYBINDINGS = Line 528 --
-- SCRATCHPADS = Line 305 / 617 --
-- XMOBAR = Line 407 -- 

--------------------------
-- | XMonad Configuration
--------------------------

-- System & Base
import Data.List (isInfixOf)
import System.IO (Handle, hPutStrLn)
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib.Extras (getWindowProperty32)
import Control.Monad (liftM2)
import Data.Char (ord)
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad
import XMonad (ScreenId(..))
import XMonad (screenWorkspace, windows, whenJust)

-- Actions
import XMonad.Actions.DynamicWorkspaces (addWorkspace)
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS              -- Required for toggleWS, prevWS, shiftToNext/Prev
import XMonad.Actions.PhysicalScreens
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
import XMonad.Hooks.ManageDocks (manageDocks, docks, avoidStruts, docksEventHook)
import XMonad.Hooks.ManageHelpers (doRectFloat, doCenterFloat, isDialog, isFullscreen, doFullFloat, doLower)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, fullscreenEventHook)
import XMonad.ManageHook
import XMonad.Config.Desktop (desktopConfig) -- For myBaseConfig
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.WindowSwallowing

-- Layouts
import XMonad.Layout.IndependentScreens (marshallPP)
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace (onWorkspace)
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
import XMonad.Util.WorkspaceCompare

import Data.List (isInfixOf)

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
myWorkspaces = ["1 HOME", "2 VM", "3 MAIL", "4 WEB", "5 CODE", "6 TOR", "7 OTHER", "8 MEDIA", "9 FILES", "0 MISC"]

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
    , [ className =? "floatterm" --> doRectFloat (W.RationalRect 0.2 0.2 0.6 0.6) ]
    
      -- 3. XFCE / Thunar Specific Dialog Fixes
    , [ (className =? "thunar-scratchpad" <&&> title =? "File Operation Progress") --> doRectFloat (W.RationalRect 0.35 0.45 0.3 0.1) ]
    , [ (className =? "thunar-scratchpad" <&&> title =? "Confirm to replace files") --> doRectFloat (W.RationalRect 0.3 0.3 0.4 0.3) ]

      -- 4. Ignore Rules (Conky, Trayer, etc.)
    , [ (className =? i <||> resource =? i) --> doIgnore | i <- myIgnores ]
    , [ className =? "Conky" --> doIgnore <+> doLower ] -- Use doIgnore to stay on top
    , [ className =? "Polybar" --> doLower ]

      -- 5. Infrastructure
    , [ manageDocks ]
    ]
  where
    -- Your central "Brain" for workspace assignments
    myWorkMap =
        [ ("1 HOME",    [])
        , ("2 VM",      [".virt-manager-wrapped"])
        , ("3 MAIL",    ["Org.gnome.Evolution", "eu.betterbird.Betterbird", "Mail"])
        , ("4 WEB",     ["Chromium", "Vivaldi-stable", "Firefox", "floorp", "Navigator"])
        , ("5 CODE",    ["code", "Code", "kate", "geany-bin", "Geany-bin"])
        , ("6 TOR",     ["qBittorrent"])
        , ("7 OTHER",   [])
        , ("8 MEDIA",   ["vlc", "io.github.celluloid_player.Celluloid", "mpv"])
        , ("9 FILES",   ["Thunar", "rclone-browser"])
        , ("0 MISC",    ["discord"])
        ]

    -- Apps that should always float
    myCFloats = [ "Arandr", "Galculator", "missioncenter", ".arandr-wrapped"
                , "gnome-calculator", "feh", "mpv", "Xfce4-terminal", "Steam"
                , "Gimp", "MPlayer", "Org.gnome.Totem", "glide", "rclone-browser" ]
    
    myTFloats = [ "Downloads", "Save As...", "Extension: (MetaMask)"
            , "File Operation Progress", "Confirm File Replace", "Unlock Drive" ]
    
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

  , NS "edge"
      "microsoft-edge-stable --new-window --class=EdgeScratchpad"
      (className =? "EdgeScratchpad")
      (customFloating $ W.RationalRect 0.025 0.025 0.95 0.95)

  , NS "onenote"
     "p3x-onenote"
     (className =? "p3x-onenote")
     (customFloating $ W.RationalRect 0.05 0.05 0.9 0.9)

  , NS "thunar"
     "thunar --class=thunar-scratchpad"
     (className =? "thunar-scratchpad")
     (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)

  , NS "qbittorrent"
     "qbittorrent"
     (className =? "qBittorrent")
     (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)

  , NS "missioncenter" "missioncenter" (className =? "missioncenter")
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
    smartBorders                          -- Removes gaps/borders if only 1 window is open
  $ spacingRaw
      True                                -- enable screen edge gaps
      (Border 0 8 8 8)                    -- screen edge gaps (top right bottom left)
      True
      (Border 8 8 8 8)                    -- window gaps
      True
  $ avoidStruts
  $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
  $ layouts
  where
    -- This uses your Dracula myTheme colors (Purple/Pink/Orange)
    myTabs = tabbed shrinkText myTheme

    -- Per-Workspace Rules: Setting the "Default" for specific areas
    layouts = onWorkspace "❹ WEB ❹"   (myTabs ||| Full)
            $ onWorkspace "❺ CODE ❺"  (myTabs ||| tiled)
            $ onWorkspace "❽ MEDIA ❽" (Full   ||| tiled)
            -- Default rotation for everyone else
            $ ( tiled
            ||| Mirror tiled
            ||| spiral (6/7)
            ||| ThreeColMid 1 delta ratio
            ||| multiCol [1] 1 0.01 (-0.5)
            ||| myTabs
            ||| Full
            )

------------------------------------------------------------------------
-- Log Hook / Xmobar (Dual Monitor Support)
------------------------------------------------------------------------

-- 1. Window count for current workspace
-- IMPORTANT: Must be at the top level (no indentation) so other functions can see it.
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

-- 2. Pretty-printer for both xmobar instances
myPPBase :: Handle -> Handle -> PP
myPPBase h0 h1 = xmobarPP
  { ppOutput = \s -> hPutStrLn h0 s >> hPutStrLn h1 s

  -- Workspaces
  -- Current = Active workspace on focused monitor
  -- Visible = Active workspace on unfocused monitor (the second screen)
  , ppCurrent = xmobarColor dracBlack dracPurple2 . wrap " " " "
  , ppVisible = xmobarColor dracOrange "" . wrap "(" ")"
  , ppHidden  = xmobarColor dracGray "" . wrap " " " "
  , ppHiddenNoWindows = const ""

  -- Urgent workspace
  , ppUrgent = xmobarColor "#FFFF80" dracRed . wrap " 🔔 " " 🔔 "

  -- Window title
  , ppTitle = xmobarColor dracOrange "" . shorten 80

  -- Layout renaming 
  , ppLayout = xmobarColor dracGray "" . (\x -> case x of
                          l | "Tabbed"   `isInfixOf` l -> " [TAB] "
                          l | "Full"     `isInfixOf` l -> " [FUL] "
                          l | "Tall"     `isInfixOf` l -> " [TAL] "
                          l | "ThreeCol" `isInfixOf` l -> " [3COL]"
                          l | "Spiral"   `isInfixOf` l -> " [SPRL]"
                          l | "multiCol" `isInfixOf` l -> " [MCOL]"
                          l | "Spacing"  `isInfixOf` l -> " " ++ drop 8 l ++ " " 
                          _ -> " " ++ x ++ " "
                      )

  -- Separators
  , ppSep = xmobarColor dracGray "" " | "

  -- Extras (window count)
  , ppExtras = [fmap (fmap (xmobarColor dracOrange "")) windowCount]

  -- Order: [WindowCount] [Workspaces] [Layout] [Title]
  , ppOrder = \(ws:l:t:ex) -> ex ++ [ws, l, t]
  }

-- 3. Log hook that sends data to both xmobar bars
myLogHook :: Handle -> Handle -> X ()
myLogHook h0 h1 = clickablePP (myPPBase h0 h1) >>= dynamicLogWithPP

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
-- Move Windows Between Monitors
------------------------------------------------------------------------

-- Move focused window to a specific screen
shiftToScreen :: ScreenId -> X ()
shiftToScreen sc = do
  ws <- gets windowset
  case lookup sc $ zip [0 ..] (W.screens ws) of
    Just screen -> windows $ W.shift (W.tag $ W.workspace screen)
    Nothing     -> return ()

shiftToScreenAndView :: ScreenId -> X ()
shiftToScreenAndView sc = do
  shiftToScreen sc
  screenWorkspace sc >>= flip whenJust (windows . W.view)

-- Move focused window to the other monitor and switch focus to it
-- Useful for Super + Tab on dual monitor setup
shiftToOtherScreenAndView :: X ()
shiftToOtherScreenAndView = do
    cur <- gets (W.screen . W.current . windowset)
    let target = if cur == 0 then 1 else 0
    screenWorkspace (S target) >>= flip whenJust (windows . W.shift)
    screenWorkspace (S target) >>= flip whenJust (windows . W.view)
    
------------------------------------------------------------------------
-- Key Bindings
------------------------------------------------------------------------

-- Key Bindings Configuration (minor fix to workspace switching)
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- SUPER + FUNCTION KEYS
    [ ((modMask .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    , ((modMask, xK_q), kill )
    , ((modMask, xK_r), spawn $ "$HOME/.config/scripts/onedrive-mount.sh" )
    , ((modMask .|. controlMask, xK_r), spawn $ "rclone-browser" )
    , ((modMask, xK_d), spawn $ "~/.config/scripts/dmenu.sh" )
    , ((0, xK_F10), spawn "polybar-msg cmd toggle" )
    --, ((modMask, xK_F1), spawn $ "vivaldi-stable" )
    , ((modMask, xK_F7), spawn $ "virt-manager" )
    --, ((modMask, xK_F8), spawn $ "thunar" )
    , ((mod1Mask, xK_space), spawn $ "rofi -theme-str 'window {width: 100%;height: 100%;}' -show drun" )
    , ((controlMask, xK_space), sendMessage NextLayout)
    , ((0, xF86XK_Calculator), spawn $ "flatpak run org.gnome.Calculator" )
    -- , ((modMask, xK_o), spawn $ "p3x-onenote" )
    , ((modMask, xK_c), spawn $ "code" )
    -- , ((modMask, xK_e), spawn $ "evolution" ) -- commented out 'e' to avoid conflict with 'atom'
    , ((modMask, xK_b), spawn $ "flatpak run eu.betterbird.Betterbird" )
    , ((modMask, xK_g), spawn $ "flatpak run org.geany.Geany" )
    --, ((modMask, xK_w), spawn $ "vivaldi" )
    , ((modMask, xK_space), spawn $ "~/.config/rofi/launchers/type-6/launcher.sh" )
    --, ((modMask .|. shiftMask , xK_w ), spawn $ "flatpak run com.microsoft.Edge" )
    , ((modMask, xK_x), spawn $ "~/.config/scripts/powermenu/powermenu.sh" )
    -- , ((modMask, xK_Return), spawn $ "kitty" )
    , ((modMask, xK_Escape), spawn $ "missioncenter" )

    -- Hardware HUD Toggle (Keypad Enter)
    , ((0, xK_KP_Subtract), spawn "~/.config/xmonad/scripts/toggle_conky.sh")
   
    -- *** RESTORED SCRIPT KEYBINDINGS ***
    -- , ((0, xK_KP_Subtract), spawn $ "~/.config/scripts/shortcut_key_scripts.sh" ) 
    , ((modMask, xK_KP_Subtract), spawn "kitty --class floatterm -e ~/nix_update.sh")
    , ((modMask, xK_p), spawn $ "~/.config/polybar/launch.sh" )              
    , ((0, xK_KP_Add), spawn $ "~/.config/scripts/wallpaper.sh" )               
    , ((modMask, xK_KP_Add), spawn $ "~/.config/scripts/select_wallpaper_pywal.sh" ) 
    , ((0, xK_KP_Multiply), spawn $ "~/.config/scripts/help.sh")
    
    -- *** GAP CONTROL KEYBINDINGS ***
    , ((0, xK_KP_Divide), incWindowSpacing 2)           
    , ((shiftMask, xK_KP_Divide), incWindowSpacing (-2))    
    , ((modMask, xK_KP_Divide), setWindowSpacing (Border 0 0 0 0)) 

    --, ((modMask, xK_t), spawn $ "thunar" )
    , ((modMask, xK_KP_Multiply), spawn $ "~/.config/polybar/scripts/keyhintvim.sh")
    , ((0, xK_F9), spawn $ "~/.config/scripts/redshift.sh" )
    , ((0, xK_F6), spawn $ "~/.config/scripts/screenoff.sh" )
    , ((modMask, xK_s), spawn $ "~/.config/scripts/dmenu-websearch.sh" )

    -- ----------------------------------------------------------------------
    -- ** NEW CYCLEWS & FOLLOW BINDINGS **
    -- ----------------------------------------------------------------------

    -- Shift Window to Next Workspace and Follow (Mod + Ctrl + J)
    , ((modMask .|. controlMask, xK_j), shiftToNext >> nextWS)

    -- Shift Window to Previous Workspace and Follow (Mod + Ctrl + K)
    , ((modMask .|. controlMask, xK_k), shiftToPrev >> prevWS)
    
    , ((mod1Mask, xK_Tab), nextWS)

    -- -------------------------------
    -- Workspace navigation 
    -- -------------------------------

    -- Next / Prev workspace
    , ((mod1Mask, xK_Tab), nextWS)
    , ((mod1Mask .|. shiftMask, xK_Tab), prevWS)

    -- Toggle last workspace
    , ((modMask, xK_grave), toggleWS)

    -- Move window + follow
    , ((modMask .|. controlMask, xK_j), shiftToNext >> nextWS)
    , ((modMask .|. controlMask, xK_k), shiftToPrev >> prevWS)

    -- -------------------------------
    -- Screen navigation (DUAL MONITOR MAGIC)
    -- -------------------------------

    -- Focus other screen
    , ((0, xK_KP_Enter), onNextNeighbour def W.shift >> nextScreen)

    -- Move window to other screen + follow it
    , ((modMask .|. shiftMask, xK_Tab), shiftNextScreen >> nextScreen)

    -- ----------------------------------------------------------------------
    -- ** Scratchpads **
    -- ----------------------------------------------------------------------
    , ((myModMask, xK_Return), namedScratchpadAction myScratchpads "terminal")
    , ((myModMask, xK_F12), namedScratchpadAction myScratchpads "mixer")
    , ((myModMask .|. shiftMask, xK_grave), namedScratchpadAction myScratchpads "notes")
    , ((modMask, xK_v), namedScratchpadAction myScratchpads "virt")
    , ((myModMask .|. shiftMask, xK_w), namedScratchpadAction myScratchpads "edge")
    , ((modMask, xK_w), namedScratchpadAction myScratchpads "vivaldi")
    , ((modMask, xK_o), namedScratchpadAction myScratchpads "onenote")
    , ((modMask, xK_e), namedScratchpadAction myScratchpads "thunar")
    , ((modMask, xK_t), namedScratchpadAction myScratchpads "qbittorrent")
    , ((myModMask, xK_Escape), namedScratchpadAction myScratchpads "missioncenter")
    
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
      | (i, k) <- zip (XMonad.workspaces conf)
      [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]
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
-- Main Function (Both Polybar + Xmobar)
------------------------------------------------------------------------
main :: IO ()
main = do
    -- 1. Launch Polybar (Ensure launch.sh handles multiple monitors)
    spawn "sh ~/.config/polybar/launch.sh"

    -- 2. Spawn Xmobar Pipes
    -- We use '-x' to explicitly target screen indices.
    xmproc0 <- spawnPipe "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    -- A tiny sleep ensures X11 doesn't get confused by two identical binaries starting at once
    xmproc1 <- spawnPipe "sleep 1 && xmobar -x 1 ~/.config/xmobar/xmobarrc"

    xmonad
      . withUrgencyHook LibNotifyUrgencyHook
      . ewmhFullscreen
      . ewmh
      . docks
      $ myBaseConfig
          { startupHook = myStartupHook

          , logHook = 
                workspaceHistoryHook 
            <+> myLogHook xmproc0 xmproc1 -- Using the dual-handle hook below

          , layoutHook =
                showWName' myShowWNameTheme
              $ gaps [(U, 5), (D, 5), (R, 5), (L, 5)]
              $ smartBorders
              $ avoidStruts
              $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
              $ myLayout
                ||| layoutHook myBaseConfig

          , manageHook =
                myManageHook
           <+> namedScratchpadManageHook myScratchpads
           <+> manageSpawn
           <+> manageDocks
           <+> manageHook myBaseConfig

          , handleEventHook =
                windowedFullscreenFixEventHook
            <+> mySwallowHook
            <+> handleEventHook myBaseConfig

          , modMask = myModMask
          , borderWidth = myBorderWidth
          , focusFollowsMouse = myFocusFollowsMouse
          , workspaces = myWorkspaces
          , focusedBorderColor = focdBord
          , normalBorderColor = normBord
          , keys = myKeys
          , mouseBindings = myMouseBindings
          }