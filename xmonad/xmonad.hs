import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.RotSlaves
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Master
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO
import qualified XMonad.StackSet as W

-- copied basic file from edder and serrghi
-- started with edders and tried to use
-- serrghis code style in order to tidy things a bit up

-- set vars here
myHome = "/home/qmeijer"

-- color settings
myBgColor = "#1b1d1e"
myFgColor = "#bbbbbb"
myBorderColor = "#292c2d"
myFocusedColor = "#57666a"
--myCurrentColor = "#cd5c5c"
myCurrentColor = "#ebac54"
myEmptyColor = "#4c4c4c"
myHiddenColor = "#dddddd"
--myLayoutColor = "#666666"
myLayoutColor = "#ebac54"
myUrgentColor = "#2b9ac8"
myIcon name = myHome ++ "/.xmonad/dzen/" ++ name ++ ".xbm"

myDzenFGColor = "#555555"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"

-- font settings
myFont = "xft:ProggyTiny:pixelsize=10"
--myFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-c-*-*-*"
--myFont = "-xos4-terminus-medium-r-normal-*-14-*-*-*-c-*-iso10646-1"


--myWorkspaces = map show $ [1 .. 9 :: Int] ++ [0]
myWorkspaces = ["1:main", "2:irc", "3:web", "4:dev", "5:misc", "6:ext", "7:gfx", "8:wine"]
myTerminal = "urxvt"
myBorderWidth = 1
myModMask = mod4Mask

myStartupHook = setWMName "LG3D"


myLayoutHook = avoidStruts $ myLayouts

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0x88888888

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

myManageHook2 = manageDocks <+> myManageHook <+> manageHook defaultConfig

--myLayouts = ( (layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) $ simpleTabbed)
--             $ (layoutAll (relBox 0.5 0 1 1)                         $ simpleTabbed)
--             ) ||| tiled ||| simpleTabbed  ||| Full 
myLayouts =  workspaceDir "~" $ mastered delta ratio simpleTabbed ||| tiled ||| noBorders simpleTabbed  ||| noBorders Full 
    where
     -- tabbedTwoPane = combineTwo (TwoPane delta ratio) (simpleTabbed) (simpleTabbed)
     -- tabbedTwoPane = addTabsAlways shrinkText defaultTheme $ TwoPane delta ratio
     -- tabbedTwoPane = simpleTabBar $ TwoPane delta ratio
     -- tabbedTwoPane = dragPane Vertical 0.03 0.5
		 -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


myKeys = [ ((mod4Mask,               xK_Right),  nextWS)
	, ((mod4Mask,               xK_Left),    prevWS)
	, ((mod4Mask .|. shiftMask, xK_Right),  shiftToNext)
	, ((mod4Mask .|. shiftMask, xK_Left),    shiftToPrev)
	, ((mod4Mask, xK_s),  swapNextScreen)

	, ((mod4Mask,               xK_z),     toggleWS)
	, ((mod4Mask, xK_g), goToSelected defaultGSConfig)
	, ((mod4Mask .|. shiftMask, xK_j   ), rotSlavesUp)
	, ((mod4Mask .|. shiftMask, xK_k   ), rotSlavesDown)
	, ((mod4Mask .|. controlMask, xK_x     ), changeDir defaultXPConfig)
	, ((mod4Mask, xK_p), shellPrompt defaultXPConfig)
	, ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
	, ((controlMask .|. shiftMask, xK_l), spawn "gnome-screensaver-command -l")
	--, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
	--, ((0, xK_Print), spawn "scrot")
	]
	++
	[((m .|. mod4Mask, k), windows $ f i)
	| (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0])
	, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
	-- [((mod4Mask .|. controlMask, k), windows $ swapWithCurrent i)
	-- | (i, k) <- zip myWorkspaces  [xK_1 ..]]
	++
	[((mod4Mask .|. mask, key), f sc)
	| (key, sc) <- zip [xK_w, xK_e] [0..]
	, (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

-- Color, font and iconpath definitions:

myStatusBar = "dzen2 -x '111' -y '0' -h '16' -w '598' -ta 'l'"
myTopBar = "conky -c .xmonad/conkyrc | dzen2 -x '700' -y '0' -h '16' -w '580' -ta 'r'"

main = do
    xmproc <- spawnPipe myStatusBar
    topBar <- spawnPipe myTopBar
    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
        { manageHook = myManageHook2
        , layoutHook = myLayoutHook
        , startupHook = myStartupHook
        , workspaces = myWorkspaces
        , logHook = dynamicLogWithPP ( dzenPP
						{ ppOutput = hPutStrLn xmproc
						, ppLayout   = dzenColor "green" "" .
						(\ x -> case x of
						 "Mastered Tabbed Simplest" -> "TwoPane"
						 "Full"			-> " Full "
						 "Tabbed Simplest"	-> " Tab "
						 _				-> pad x
						)
											}) >> myLogHook
        , modMask = myModMask
		, terminal = myTerminal
        } `additionalKeys` myKeys
