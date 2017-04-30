------------------------------------------------------------------------
------------------------------- xmonad.hs ------------------------------
------------------------------------------------------------------------
----
-------------------------------- Author --------------------------------
------------------------------------------------------------------------
------------------- carlos Sánchez <kniren@gmail.com> --------------------
------------------------------------------------------------------------
----
-------------------------------- Source --------------------------------
------------------------------------------------------------------------
--- https://github.com/kniren/dotfiles/blob/master/.xmonad/xmonad.hs ---
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Imports --¬
------------------------------------------------------------------------

import Data.List
import Data.Monoid
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Script
import XMonad.Layout.Fullscreen
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- General Options --¬
------------------------------------------------------------------------

myTerminal          = "terminator"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myModMask           = mod4Mask

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Layout names and quick access keys --¬
------------------------------------------------------------------------

myWorkspaces = clickable . (map dzenEscape) $ ["Pr1nc1p4l",
                                               "W3b",
                                               "M3d14",
                                               "M1sc",
                                               "V1st4s",
                                               "R3d4cc10n"]
    where clickable l = [ x ++ ws ++ "^ca()" | 
--                        (i,ws) <- zip ['1','2','3','q','w','e'] l,
                        (i,ws) <- zip ['1','2','3','4','5','6'] l,
                        let n = i 
                            x = "^ca(1,xdotool key super+" ++ show (n) ++ ")"]

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Border Options --¬
------------------------------------------------------------------------

myNormalBorderColor  = "#121212"
myFocusedBorderColor = "#435d75"
myBorderWidth   = 1

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Key bindings --¬
------------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ 
    ((modm,xK_p), spawn "dmenu_run -i -fn '*-profont-*-*-*-*-12-*-*-*-*-*-*-*' -sb '#435d75' -nb '#000000'") -- launch dmenu
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
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")
--    , ((modm .|. shiftMask, xK_i     ), spawn "dwb")
    , ((modm .|. shiftMask, xK_b     ), spawn "pcmanfm")
	, ((modm 			  , xK_n 	 ), spawn "urxvt -e ncmpcpp")
    , ((modm 			  , xK_m     ), spawn "/home/carlos/Scripts/bin/mpd/launchMPD.sh")
    , ((modm .|. shiftMask, xK_m     ), spawn "/home/carlos/Scripts/bin/mpd/stopMPD.sh")
    -- Alsa Multimedia Control
    , ((0, 0x1008ff11), spawn "~/.xmonad/Scripts/voltctl down")
    , ((0, 0x1008ff13), spawn "~/.xmonad/Scripts/voltctl up")
    , ((0, 0x1008ff12), spawn "~/.xmonad/Scripts/voltctl toggle")

--    , ((0, 0x1008ff11), spawn "amixer -q set Master 5%- unmute")
--    , ((0, 0x1008ff13), spawn "amixer -q set Master 5%+ unmute")
--    , ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
--    , ((0, 0x1008ff03), spawn "xbacklight -dec 20")    					-- Brightness Control
--    , ((0, 0x1008ff02), spawn "xbacklight -inc 20")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
--        | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_q,xK_w,xK_e]
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--        | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Mouse bindings --¬
------------------------------------------------------------------------
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

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Layouts --¬
------------------------------------------------------------------------

myLayout = mkToggle (NOBORDERS ?? FULL ?? EOT) $
           avoidStruts $
           webLayout $
           standardLayout
    where
     standardLayout = tiled     ||| mirrorTiled ||| fullTiled ||| noBor
     var1Layout     = fullTiled ||| tiled       ||| noBor
     webLayout      = onWorkspace (myWorkspaces !! 1) var1Layout
     fullTiled      = Tall nmaster delta (1/4)
     mirrorTiled    = Mirror . spacing 1 $ Tall nmaster delta ratio
     noBor          = noBorders (fullscreenFull Full)
     tiled          = spacing 1 $ Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Percent of screen to increment by when resizing panes
     delta   = 5/100
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Window rules --¬
------------------------------------------------------------------------
-- NOTE: To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
------------------------------------------------------------------------

myManageHook = manageDocks <+> composeAll
    [ className =? "MPlayer"             --> doFloat
    , className =? "MPlayer"             --> doShift (myWorkspaces !! 2)
    , className =? "Gimp"                --> doFloat
    , className =? "Gimp"                --> doShift (myWorkspaces !! 2)
    , className =? "Nautilus"            --> doShift (myWorkspaces !! 3)
    , className =? "Zathura"             --> doShift (myWorkspaces !! 2)
    , className =? "Dwb"                 --> doShift (myWorkspaces !! 1)
    , className =? "Chromium"            --> doShift (myWorkspaces !! 1)
    , className =? "Firefox"             --> doShift (myWorkspaces !! 1)
    , className =? "Google-chrome"       --> doShift (myWorkspaces !! 1)
    , className =? "Eclipse"             --> doShift (myWorkspaces !! 5)
    , className =? "processing-app-Base" --> doShift (myWorkspaces !! 4)
    , className =? "processing-app-Base" --> doFloat
    , resource  =? "desktop_window"      --> doIgnore
    , resource  =? "kdesktop"            --> doIgnore  
    , isFullscreen --> doFullFloat ]

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Event handling --¬
------------------------------------------------------------------------

myEventHook = fullscreenEventHook

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Status bars and logging --¬
------------------------------------------------------------------------

myLogHook h = dynamicLogWithPP $ defaultPP
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
                "Spacing 1 Tall"        -> clickInLayout ++
                    "^i(/home/carlos/.xmonad/dzen/icons/stlarch/tile.xbm)^ca()"
                "Tall"                   -> clickInLayout ++
                    "^i(/home/carlos/.xmonad/dzen/icons/stlarch/monocle.xbm)^ca()"
                "Mirror Spacing 1 Tall" -> clickInLayout ++
                    "^i(/home/carlos/.xmonad/dzen/icons/stlarch/bstack.xbm)^ca()"
                "Full"                   -> clickInLayout ++
                    "^i(/home/carlos/.xmonad/dzen/icons/stlarch/monocle2.xbm)^ca()"
                _                        -> x
            )
      , ppTitle             =   (" " ++) . dzenColor "white" "#000000" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
clickInLayout = "^ca(1,xdotool key super+space)"

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Startup hook --¬
------------------------------------------------------------------------

myStartupHook = do
--	spawn "~/.xmonad/autostart"   -- bash script start apps
	setWMName "LG3D" 			  -- Java apps

-- -¬
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Run xmonad --¬
------------------------------------------------------------------------

main = do 
    d <- spawnPipe "dzen2 -dock -ta l -fn 'profont-8' -bg '#000000' -w 500 -h 20 -e 'button3='"
    spawn "conky | dzen2 -x 500 -ta r -fn 'profont-8' -bg '#000000' -h 20 -e 'button3='"
    xmonad $ defaults {
    logHook = myLogHook d
    }  

defaults = defaultConfig {
        -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        -- hooks, layouts
        layoutHook         = smartBorders(myLayout),
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook
}

-- -¬
------------------------------------------------------------------------
