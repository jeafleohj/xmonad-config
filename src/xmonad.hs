{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import XMonad
import Data.Maybe (fromJust)

import XMonad.Config.Desktop

import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.NamedScratchpad

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce

import Data.Monoid
-- import Data.Ratio

import System.Exit
import System.IO

import Graphics.X11.ExtraTypes.XF86

import XMonad.Layout.LimitWindows
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.Reflect
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowArranger
-- import XMonad.Layout.WorkspaceDir
import XMonad.Layout.ZoomRow
-- import qualified XMonad.Layout.ToggleLayouts as T

import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
-- import XMonad.Layout.SubLayouts
-- import XMonad.Layout.Tabbed
-- import XMonad.Layout.WindowNavigation

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.LayoutModifier
-- import XMonad.Layout.Simplest
import XMonad.Layout.Accordion
-- import XMonad.Layout.ShowWName
import Util.Volume
import Util.Brightness
import Util.ColorPalette
-- import XMonad.Layout.BoringWindows (boringWindows)
-- import XMonad.Layout.CircleEx


myterminal :: [Char]
myterminal = "alacritty"

myeditor :: [Char]
myeditor = "emacsclient -c"

myBorderWidth :: Dimension
myBorderWidth = 1

myWorkspaces :: [String]
myWorkspaces = ["dev","www","3","4","5","6","media","zoom","pdf"]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

clickable :: String -> [Char]
clickable ws = "<action=xdotool key super+"++show i++">"++ws++ "</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices

myModMask :: KeyMask
myModMask = mod4Mask

mynormalBorderColor :: String
mynormalBorderColor = "black"

myfocusedBorderColor :: String
myfocusedBorderColor = "#bcffe7"

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [
    className =? "firefox"      --> doShift (myWorkspaces !! 1)
  , className =? "Evince"       --> doShift (myWorkspaces !! 8)
  , className =? "Gimp"         --> doFloat
  , className =? "Peek"         --> doFloat
  , className =? "VirtualBoxVM" --> doFloat
  , className =? "VirtualBoxVM" --> doShift (myWorkspaces !! 6)
  , className =? "cmus"         --> doShift (myWorkspaces !! 6)
  , className =? "pentablet"    --> doFloat
  , className =? "zoom"         --> doShift (myWorkspaces !! 7)
  ] <+> namedScratchpadManageHook myScratchpads


-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
  -- Xmonad
  [ ((modm .|. shiftMask, xK_q     ), io exitSuccess)
  , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
  -- Windows
  , ((modm .|. shiftMask, xK_c     ), kill) -- close focused window

  -- Floating windows
  , ((modm,               xK_t     ), withFocused $ windows . W.sink) -- Push window back into tiling
  , ((modm .|. mod1Mask,  xK_t     ), sinkAll)

  , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- launch a terminal
  , ((modm,               xK_p     ), spawn "dmenu_run -fn 'UbuntuMono Nerd Font:size=16' -nb '#292d3e' -nf '#bbc5ff' -sb '#82AAFF' -sf '#292d3e' -p 'dmenu:'") -- launch dmenu

  , ((modm,               xK_space ), sendMessage NextLayout) -- Rotate through the available layout algorithms
  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) --  Reset the layouts on the current workspace to default
  , ((modm,               xK_n     ), refresh) -- Resize viewed windows to the correct size

  -- Windows navigation
  , ((modm,               xK_Tab   ), windows W.focusDown) -- Move focus to the next window
  , ((modm,               xK_j     ), windows W.focusDown) -- Move focus to the next window
  , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp) -- Move focus to the next window
  , ((modm,               xK_k     ), windows W.focusUp  ) -- Move focus to the previous window
  , ((modm,               xK_m     ), windows W.focusMaster  ) -- Move focus to the master window
  , ((modm,               xK_Return), windows W.swapMaster) -- Swap the focused window and the master window
  , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  ) -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    ) -- Swap the focused window with the previous window
  , ((modm,               xK_h     ), sendMessage Shrink) -- Shrink the master area
  , ((modm,               xK_l     ), sendMessage Expand) -- Expand the master area

  , ((modm              , xK_comma ), sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
  -- , ((modm              , xK_period), sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area

  , ((modm .|. controlMask .|. mod1Mask, xK_Up   ), sendMessage Arrange)
  , ((modm .|. controlMask .|. mod1Mask, xK_Down ), sendMessage DeArrange)
  , ((modm, xK_Up                                ), sendMessage (MoveUp 10)) --  Move focused window to up
  , ((modm, xK_Down                              ), sendMessage (MoveDown 10)) --  Move focused window to down
  , ((modm, xK_Right                             ), sendMessage (MoveRight 10)) --  Move focused window to right
  , ((modm, xK_Left                              ), sendMessage (MoveLeft 10)) --  Move focused window to lenft
  , ((modm .|. shiftMask, xK_Up                  ), sendMessage (IncreaseUp 10)) --  Increase size of focused window up
  , ((modm .|. shiftMask, xK_Down                ), sendMessage (IncreaseDown 10)) --  Increase size of focused window down
  , ((modm .|. shiftMask, xK_Right               ), sendMessage (IncreaseRight 10)) --  Increase size of focused window right
  , ((modm .|. shiftMask, xK_Left                ), sendMessage (IncreaseLeft 10)) --  Increase size of focused window left
  , ((modm .|. controlMask , xK_Up               ), sendMessage (DecreaseUp 10)) --  Decrease size of focused window up
  , ((modm .|. controlMask , xK_Down             ), sendMessage (DecreaseDown 10)) --  Decrease size of focused window down
  , ((modm .|. controlMask , xK_Right            ), sendMessage (DecreaseRight 10)) --  Decrease size of focused window right
  , ((modm .|. controlMask , xK_Left             ), sendMessage (DecreaseLeft 10)) --  Decrease size of focused window left


-- Layouts
  , ((modm              , xK_b     ), sendMessage ToggleStruts) --increase window

  --- Grid select
  , ((modm .|. shiftMask, xK_t), spawnSelected' spawnList)
  , ((modm .|. shiftMask, xK_b), bringSelected $ mygridConfig myColorizer)

  --- Layouts
  , ((modm .|. shiftMask, xK_n ), sendMessage $ Toggle NOBORDERS) -- Toggles noborder
  , ((modm, xK_Up), sendMessage (MoveUp 10)) --  Move focused window to up
  , ((modm, xK_z), sendMessage ZoomFullToggle)
--  , ((modm, xK_z), sendMessage MirrorShrink)
--  , ((modm, xK_a), sendMessage MirrorExpand)

  -- Volumen control
  , (( 0, xF86XK_AudioLowerVolume ), lowerVolume 5)
  , (( 0, xF86XK_AudioRaiseVolume ), raiseVolume 5)
  , (( 0, xF86XK_AudioMute        ), toggleMute)

  -- Brightness control
  , (( 0, xF86XK_MonBrightnessUp       ), brightnessUp 10)
  , (( 0, xF86XK_MonBrightnessDown     ), brightnessDown 10)

  , (( 0, xF86XK_WLAN                  ), spawn "~/.xmonad/wifi.sh")
  , ((modm , xK_c                      ), spawn "chromium")
  , ((modm , xK_f                      ), spawn "firefox")
  , (( mod1Mask .|. controlMask , xK_p ), spawn "firefox --private-window")
  , (( mod1Mask .|. controlMask , xK_n ), spawn "nautilus")
  -- Lock screen using lightdm
  , (( mod1Mask .|. controlMask , xK_l ), spawn "dm-tool lock")
  -- Edit custom files
  , ((mod1Mask .|. controlMask  , xK_e ), spawn "~/.dmenu/edit.sh")
  , ((modm                      , xK_s ), spawn "~/.dmenu/settings.sh")
  , ((modm .|. controlMask,       xK_e ), spawn myeditor)
  , ((mod1Mask .|. controlMask  , xK_t ), spawn "dunstctl close-all")
  , ((mod1Mask .|. controlMask  , xK_s ), spawn "dunstctl history-pop")
  ]
  ++
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++
  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  ++
  -- Testing scratchpads
  [
    ((modm, xK_u), namedScratchpadAction myScratchpads "terminal")
  ]

-- Mouse config
button8 :: Button
button8 = 8
button9 :: Button
button9 = 9

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [ ((modm, button1                  ), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((modm, button2                  ), \w -> focus w >> windows W.shiftMaster)
  , ((modm, button3                  ), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  , ((modm .|. controlMask , button1 ), \_ -> spawn "sleep 0.2 && xdotool click 3")
  , ((modm , button8                 ), \_ -> spawn "notify-send 'Without config'")
  , ((modm , button9                 ), \_ -> spawn "notify-send 'Without config'")
  ]

-- mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall = renamed [Replace "T"]
       $ limitWindows 4
       $ mySpacing' 5
       $ smartBorders
       $ ResizableTall 1 (1/100) (3/5) []

monocle = renamed [Replace "monocle"]
          $ smartBorders
          $ limitWindows 20 Full

floats = renamed [Replace "floats"]   $ limitWindows 20 simplestFloat

threeRow = renamed [Replace "threeRow"]
  $ smartBorders
  $ limitWindows 5
--  $ subLayout [0,1] (Simplest ||| circleEx {cDelta = -3*pi/4})
  $ Mirror
  $ ThreeCol 1 (3/100) (1/2)

threeCol = renamed [Replace "threeCol"]
  $ limitWindows 5
  $ ThreeCol 1 (3/100) (1/2)

tallAccordion = renamed [Replace "tallAccordion"] Accordion

myLayoutHook = onWorkspace (head myWorkspaces) devLayout
               otherLayout
  where
    devLayout = avoidStruts $ common $ mkToggle option
             threeCol ||| threeRow
    otherLayout = avoidStruts $ common $ mkToggle option myDefaultLayout
    common = mouseResize . windowArrange
    option = NBFULL ?? NOBORDERS ?? EOT
    myDefaultLayout =  tall
      ||| floats
      ||| monocle
      ||| tallAccordion
      ||| threeCol
--      ||| threeRow

-- myShowWNameTheme :: SWNConfig
-- myShowWNameTheme = def
--     { swn_font              = "xft:Ubuntu:bold:size=20"
--     , swn_fade              = 2.0
--     , swn_bgcolor           = "#000000"
--     , swn_color             = "#ffffff"
--     }


myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ myXmobarPP { ppOutput = hPutStrLn h }

myXmobarPP :: PP
myXmobarPP = xmobarPP
             { ppCurrent         = xmobarColor "#f8f8f8" "" . wrap "[" "]"
             , ppVisible         = xmobarColor "#f8f8f8" "LightSkyBlue4"
             , ppHidden          = xmobarColor modusVivendiBlue "" . clickable -- Hidden workspaces in xmobar
             , ppHiddenNoWindows = xmobarColor modusVivendiRed "" . clickable
             , ppLayout          = wrap "" "" . xmobarColor "DarkOrange" ""
             , ppTitle           = xmobarColor modusVivendiGreen "" . shorten 50
             , ppSep             = coloredText modusVivendiCyan " : "
             , ppWsSep           = " "
             , ppUrgent          = xmobarColor modusVivendiYellow "" . wrap "!" "!"  -- Urgent workspace
             , ppExtras          = [windowCount] -- # of windows current workspaces
             , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
             }
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  spawnOnce ""

spawnList :: [(String, String)]
spawnList  = [("Emacs"    , myeditor)
             ,("Firefox"  , "firefox")
             ,("Gimp"     , "gimp")
             ,("Dolphin"  , "dolphin")
             ,("Nautilus" , "nautilus"                    )
             ,("Okular"   , "okular")
             ,("Evince"   , "evince")
             ]

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect def lst >>= flip whenJust spawn

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
              (0x31,0x2e,0x39) -- lowest inactive bg
              (0x31,0x2e,0x39) -- highest inactive bg
              (0x61,0x57,0x72) -- active bg
              (0xc0,0xa7,0x9a) -- inactive fg
              (0xff,0xff,0xff) -- active fg

-- gridSelect menu layout
myFont :: String
myFont = "xft:Mononoki Nerd Font:regular:pixelsize=12"

mygridConfig :: p -> GSConfig Window
mygridConfig _ = (buildDefaultGSConfig myColorizer)
  { gs_cellheight     = 30
  , gs_cellwidth    = 200
  , gs_cellpadding  = 10
  , gs_originFractX = 0.5
  , gs_originFractY = 0.5
  , gs_font         = myFont
  }

-- Scratchpad
myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "cmus" spawnCmus findCmus manageCmus
                ]
  where
    spawnTerm  = myterminal ++  " --class scratchpad -t scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.900     -- terminal height, 10%
        w = 0.950     -- terminal width, 100%
        t = 0.950 - h -- distance from top edge, 90%
        l = 0.975 - w -- distance from left edge, 0%
    spawnCmus  = myterminal ++  " -name cmus -e cmus "
    findCmus   = resource =? "cmus"
    manageCmus = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh desktopConfig
    {
      borderWidth        = myBorderWidth
    , manageHook         = myManageHook <+> manageHook desktopConfig
--    , layoutHook         = showWName' myShowWNameTheme myLayoutHook
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook xmproc
    , terminal           = myterminal
    , startupHook        = myStartupHook
    , normalBorderColor  = mynormalBorderColor
    , focusedBorderColor = myfocusedBorderColor
    , modMask            = myModMask
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , workspaces         = myWorkspaces
    }
