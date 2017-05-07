{-# LANGUAGE FlexibleContexts, PatternGuards #-}

import Data.List
import Data.Monoid
import System.Exit
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Script
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import qualified XMonad.StackSet as W
import qualified Data.Map as M

---
--- General options
---
myTerminal 		= "urxvt"
myModMask  		= mod4Mask
myNormalBorderColor 	= "#121212"
myFocusedBorderColor 	= "#435d75"
myBorderWidth 		= 2

---
--- dzen Dock
---
myDzen :: LayoutClass l Window
     => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myDzen conf = statusBar ("dzen2 " ++ flags) myPP toggleStrutsKey conf
 where
    fg      = "'#a8a3f7'"
    bg      = "'#000000'"
    flags   = "-dock -e 'onstart=lower' -fn 'profont-8' -x 0 -y 0 -w 500 -h 20 -ta l -fg " ++ fg ++ " -bg " ++ bg

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm .|. shiftMask, xK_Print )

myPP = dzenPP
    {
        ppCurrent           =   dzenColor "white"   "#121212" . pad
      , ppVisible           =   dzenColor "#6d9cbe" "#000000" . pad
      , ppHidden            =   dzenColor "#435d75" "#000000" . pad
      , ppHiddenNoWindows   =   dzenColor "#444444" "#000000" . pad
      , ppUrgent            =   dzenColor "red"     "#000000" . pad
      , ppWsSep             =   ""
      , ppSep               =   " | "
      , ppLayout            =   dzenColor "#435d75" "#000000" .
            (\x -> case x of
                "Spacing 0 Tall"        -> clickInLayout ++ "^i(/home/carlos/.xmonad/dzen/icons/stlarch/tile.xbm)^ca()"
                "Tall"                  -> clickInLayout ++ "^i(/home/carlos/.xmonad/dzen/icons/stlarch/monocle.xbm)^ca()"
                "Mirror Spacing 0 Tall" -> clickInLayout ++ "^i(/home/carlos/.xmonad/dzen/icons/stlarch/bstack.xbm)^ca()"
                "Full"                  -> clickInLayout ++ "^i(/home/carlos/.xmonad/dzen/icons/stlarch/monocle2.xbm)^ca()"
                _                       -> x
            )
      , ppTitle             =   (" " ++) . dzenColor "white" "#000000" . dzenEscape
--      , ppOutput            =   hPutStrLn 
    }
clickInLayout :: String
clickInLayout = "^ca(1,xdotool key super+space)"


---
--- Key bindings
---
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ 
    ((modm,xK_p), spawn "dmenu_run -i -fn 'profont-10' -sb '#435d75' -nb '#000000'") -- launch dmenu
    , ((modm .|. shiftMask, xK_c     ), kill)    						-- Close focused window
    , ((modm,               xK_space ), sendMessage NextLayout) 		-- Rotate through the available layout algorithmsv
    , ((modm,               xK_Tab   ), windows W.focusDown)    		-- Change Focused Windows
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp)
--    , ((modm,               xK_m     ), windows W.focusMaster)
    , ((modm,               xK_Tab   ), windows W.focusDown)    		-- Swap Focused Windows
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)
    , ((modm,               xK_h     ), sendMessage Shrink)     		-- Shrink the master area
    , ((modm,               xK_l     ), sendMessage Expand)     		-- Expand the master area
    , ((modm,               xK_t     ), withFocused $ windows . W.sink) -- Push window back into tiling
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))    	-- Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))  -- Deincrement the number of windows in the master area
    , ((modm,               xK_f     ), sendMessage $ Toggle FULL)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))    	-- Quit xmonad
    , ((modm,               xK_Return), spawn $ XMonad.terminal conf)   -- Application Spawning
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile && killall dzen2 && xmonad --restart")
--    , ((modm .|. shiftMask, xK_i     ), spawn "dwb")
    , ((modm .|. shiftMask, xK_b     ), spawn "pcmanfm")
    , ((modm 		  , xK_n     ), spawn "urxvt -e ncmpcpp")
    , ((modm 		  , xK_m     ), spawn "/home/carlos/Scripts/bin/mpd/launchMPD.sh")
    , ((modm .|. shiftMask, xK_m     ), spawn "/home/carlos/Scripts/bin/mpd/stopMPD.sh")
    , ((0, 0x1008ff11), spawn "amixer -q set Master 5%- unmute")    	-- Alsa Multimedia Control
    , ((0, 0x1008ff13), spawn "amixer -q set Master 5%+ unmute")
--    , ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
--    , ((0, 0x1008ff03), spawn "xbacklight -dec 20")    					-- Brightness Control
--    , ((0, 0x1008ff02), spawn "xbacklight -inc 20")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_q,xK_w,xK_e]
--        | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--        | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

   
---
--- Workspaces layout
---
--myWorkspaces :: [String]
--myWorkspaces = clickable . (map dzenEscape) $ [ "Principal",
--						"www",
--						"Media",
--						"Misc",
--						"Vistas",
--						"Redacs"]
--   where clickable l = [ x ++ ws ++ "^ca()" | 
--       		(i,ws) <- zip "123qwe" l,
--		        let n = i
--		            x =    "^ca(4,xdotool key super+Right)"
--				++ "^ca(4,xdotool key super+Left)"
--				++ "^ca(1,xdotool key super+" ++ show n ++ ")"]

myWorkspaces = ["Principal","www","Media","Misc","Vistas","Redacs"]

---
--- Layouts
---
myLayout = mkToggle (NOBORDERS ?? FULL ?? EOT) $
           avoidStruts $
           onWorkspace (myWorkspaces !! 2) webLayout $
           onWorkspace (myWorkspaces !! 5) vistasLayout $
           standardLayout
    where
     standardLayout = mirrorTiled ||| tiled     ||| fullTiled ||| noBor
     var1Layout     = fullTiled ||| tiled       ||| mirrorTiled ||| noBor
     webLayout      = var1Layout
     fullTiled      = Tall nmaster delta (1/4)
     mirrorTiled    = Mirror . spacing 0 $ Tall nmaster delta ratio
     noBor          = noBorders (fullscreenFull Full)
     vistasLayout   = webLayout
     tiled          = spacing 0 $ Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Percent of screen to increment by when resizing panes
     delta   = 5/100
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2      

---
--- Window rules 
---
-- To find the property name associated with a program, use xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.

myManageHook = manageDocks <+> composeAll
    [ className =? "MPlayer"             --> doFloat
    , className =? "MPlayer"             --> doShift (myWorkspaces !! 3)
    , className =? "Gimp"                --> doFloat
    , className =? "Gimp"                --> doShift (myWorkspaces !! 5)
    , className =? "Nautilus"            --> doShift (myWorkspaces !! 3)
    , className =? "Zathura"             --> doShift (myWorkspaces !! 2)
    , className =? "Dwb"                 --> doShift (myWorkspaces !! 2)
    , className =? "Chromium"            --> doShift (myWorkspaces !! 2)
    , className =? "Firefox"             --> doShift (myWorkspaces !! 2)
    , className =? "Google-chrome"       --> doShift (myWorkspaces !! 2)
    , className =? "Eclipse"             --> doShift (myWorkspaces !! 5)
    , isFullscreen --> doFullFloat ]


---
--- Mouse bindings
---
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


---
--- Event handling
---
myEventHook = fullscreenEventHook



---
--- Startup
---
myStartupHook = do
    spawn "~/.xmonad/autostart"
    setWMName "LG3D"

---
--- Main
--- 
main = do
    spawn "conky | dzen2 -x 500 -ta r -fn 'profont-8' -bg '#000000' -h 20 -e 'button3='"
    xmonad =<< myDzen myConfig

myConfig = desktopConfig {
    terminal 			= myTerminal,
    modMask  			= myModMask,
    focusFollowsMouse 		= True,
    borderWidth 		= myBorderWidth,
    normalBorderColor 		= myNormalBorderColor,
    focusedBorderColor 		= myFocusedBorderColor,
    workspaces 			= myWorkspaces,
    keys 			= myKeys,
    mouseBindings 		= myMouseBindings,
    layoutHook 			= smartBorders(myLayout),
--    startupHook 		= myStartupHook,
--    logHook 			= dynamicLogWithPP $ sjanssenPP,
    manageHook 			= myManageHook,
    handleEventHook 		= myEventHook
	
}
