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
myIFGColor = myLayoutColor
myIcon name = myHome ++ "/.xmonad/dzen/" ++ name ++ ".xbm"
myIconDir = myHome ++ "/.dzen/dzenIcons"

-- size needed for conky
myConkyWidth = "360"
myScreenWidth = "1280"
myDzenX = "110"
-- use myConkyWidth to determine placing
-- myConkyWidth
-- myScreenWidth
-- myDzenX

myConky = "920"
myDzenWidth = "810"

-- font settings
-- verdana works with dzen-svn in from aur
myFont = "Verdana-8"

myWorkspaces = ["1:main", "2:misc", "3:web", "4:dev", "5:im", "6:mail", "7:rdesk", "8:wine"]
--myTerminal = "urxvt"
myTerminal = "gnome-terminal"
myBorderWidth = 1
myModMask = mod4Mask

myStartupHook = setWMName "LG3D"

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

myManageHook2 = manageDocks <+> myManageHook <+> manageHook defaultConfig

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

myLayoutHook = avoidStruts $ myLayouts

myKeys = [ ((mod4Mask,               xK_Right),  nextWS)
	, ((myModMask,               xK_Left),    prevWS)
	, ((myModMask .|. shiftMask, xK_Right),  shiftToNext)
	, ((myModMask .|. shiftMask, xK_Left),    shiftToPrev)
	, ((myModMask, xK_s),  swapNextScreen)

	, ((myModMask,               xK_z),     toggleWS)
	, ((myModMask, xK_g), goToSelected defaultGSConfig)
	, ((myModMask .|. shiftMask, xK_j   ), rotSlavesUp)
	, ((myModMask .|. shiftMask, xK_k   ), rotSlavesDown)
	, ((myModMask .|. controlMask, xK_x     ), changeDir defaultXPConfig)
	, ((myModMask, xK_p), shellPrompt defaultXPConfig)
	, ((myModMask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
	--, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
	--, ((0, xK_Print), spawn "scrot")
	]
	++
	[((m .|. myModMask, k), windows $ f i)
	| (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0])
	, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
	-- [((myModMask .|. controlMask, k), windows $ swapWithCurrent i)
	-- | (i, k) <- zip myWorkspaces  [xK_1 ..]]
	++
	[((myModMask .|. mask, key), f sc)
	| (key, sc) <- zip [xK_w, xK_e] [0..]
	, (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]



-- my info bars
myEmptyBar = "dzen2 -e 'onstartup=lower' -x '0' -y '0' -w '1280' -h '16'"
myStatusBar = "dzen2 -x '" ++ myDzenX  ++ "' -y '0' -h '16' -w '" ++ myDzenWidth ++ "' -ta 'l' -fn '" ++ myFont ++ "' -bg \"" ++ myBgColor ++ "\" -fg \"" ++ myFgColor ++ "\""

myTopBar = "conky -c .xmonad/conkyrc | dzen2 -x '" ++ myConky ++ "' -y '0' -h '16' -w '" ++ myConkyWidth ++ "' -ta 'r' -fn '" ++ myFont ++ "' -bg \"" ++ myBgColor ++ "\" -fg \"" ++ myFgColor ++ "\""

-- Log Hook
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0x88888888


-- Pretty Printing
myDzenPP h = defaultPP
             { ppOutput = hPutStrLn h
             , ppCurrent = corner . fg myCurrentColor
             , ppHidden = corner . fg myHiddenColor
             , ppHiddenNoWindows = corner . fg myEmptyColor
             , ppUrgent = corner . fg myUrgentColor . dzenStrip
             , ppLayout = fg myLayoutColor .
                          (\x -> case x of
                          "Mirror Tall" -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/mtall.xbm)"
                          "Tall"     -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/tall.xbm)"
                          "Full"     -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/full.xbm)"
                          "Tabbed Simplest"      -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/full.xbm)"
                          "Grid"     -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/grid.xbm)"
                          "TwoPane"      -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/two_pane.xbm)"
                          "Mastered Tabbed Simplest"     -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/two_pane.xbm)"
                          _ -> x
                          )
             , ppWsSep = "  "
             , ppSep = "     "
             , ppTitle = fg myFgColor . dzenEscape . shorten 100 . trim
             }
     where fg c = dzenColor c ""
           icon n = "^i(" ++ (myIcon n) ++ ")"
           corner = (++) (icon "corner")
           layout n = icon ("layout-" ++ n)




main = do
    xmproc <- spawnPipe myStatusBar
    topBar <- spawnPipe myTopBar
    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
        { manageHook = myManageHook2
        , layoutHook = myLayoutHook
        , startupHook = myStartupHook
        , workspaces = myWorkspaces
		, logHook = myLogHook >> (dynamicLogWithPP $ myDzenPP xmproc)
        , modMask = myModMask
		, terminal = myTerminal
       } `additionalKeys` myKeys
