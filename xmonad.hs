import qualified Data.Map as M
import Graphics.X11.ExtraTypes (xF86XK_AudioLowerVolume, xF86XK_AudioMicMute, xF86XK_AudioMute, xF86XK_AudioNext, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioRaiseVolume)
import XMonad.Hooks.EwmhDesktops
import XMonad
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.FloatSnap
import XMonad.Actions.MostRecentlyUsed (Location (workspace))
import XMonad.Actions.SwapPromote (masterHistoryHook)
import XMonad.Layout.IndependentScreens
import XMonad.Actions.TopicSpace (workspaceHistoryHook)
import XMonad.Hooks.CurrentWorkspaceOnTop (currentWorkspaceOnTop)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.ComboP (Property (ClassName), combineTwoP)
import XMonad.Layout.Combo (combineTwo)
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.OneBig (OneBig (OneBig))
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.ZoomRow (zoomRow)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Util.SpawnOnce

myScratchPads = [NS "terminal" spawnTerm findTerm manageTerm]
  where
    spawnTerm = myTerminal ++ " --class scratchpad"
    findTerm = className =? "scratchpad" -- classname "scratchpad" is the term
    manageTerm = customFloating $ W.RationalRect l t w h -- using the geometry below
      where
        h = 0.3
        w = 0.3
        t = (1 - h) / 2
        l = (1 - h) / 2

myTerminal = "tabbed -r 2 st -w '' -e zsh"

myFocusFollowsMouse = True

myClickJustFocuses = False

myBorderWidth = 0

-- alt key
myModMask = mod1Mask

toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s
    )

myMouseBinds (XConfig {XMonad.modMask = modMask}) =
  M.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w),
      ((modMask, button2), \w -> focus w >> Flex.mouseResizeWindow w),
      ((modMask, button3), windows . W.sink),
      ((modMask .|. shiftMask, button1), \w -> focus w >> snapMove D Nothing w),
      ((modMask .|. shiftMask, button2), \w -> focus w >> snapMove L Nothing w),
      ((modMask .|. controlMask, button1), \w -> focus w >> snapMagicResize [U, D] Nothing Nothing w),
      ((modMask .|. controlMask, button2), \w -> focus w >> snapMagicResize [L, R] Nothing Nothing w)
    ]

myManageHook =
  composeAll -- Rules for windows, see below
    [ className =? "Xmessage" --> doFloat,
      role =? "pop-up" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      appName =? "Firefox" --> doFloat,
      className =? "xdg-desktop-portal-gtk" --> doIgnore,
      role =? "GtkFileChooserDialog" --> doFloat,
      appName =? "Gimp" --> doFloat
    ]
    <+> namedScratchpadManageHook myScratchPads -- Allow scratchpads
  where
    role = stringProperty "WM_WINDOW_ROLE" -- Set rules for WM_CLASS

myLogHook =
  workspaceHistoryHook
    <> masterHistoryHook
    <> currentWorkspaceOnTop
    <> refocusLastLogHook

-- myLayoutHook = ifWider 1440 myLayout $ Mirror zoomRow
myLayoutHook = myLayout

myLayout =
  name "CorneredMstr" corner -- Master is in the top left, with subs surrounding it
    ||| name "CenteredMstr" center -- Master is centeted in screen, over grid
    ||| name "TwoCol" twocol -- Three tiled columns, honors masters
    ||| name "TriCol" tricol -- Three tiled columns, honors masters
    ||| name "Stacked" stacked -- Laid out horizontally/vertically
    ||| name "UnStack" unstack -- Laid out horizontally/vertically
    -- \||| name "Crunch" crunch -- Laid out horizontally/vertically, unfocused are compressed
    ||| name "Bighead" bighead -- Two layouts, Grid on left, CorneredMstr on right
  where
    name n = renamed [Replace n] -- Enable smart spacing for all custom layouts
    corner = OneBig (6 / 9) (6 / 9) -- OneBig (ScreenWidth) (ScreenHeight)
    twocol = Tall 2 (3 / 100) (1 / 2) -- Tall (# of masters) (Delta) (Ratio)
    tricol = ThreeColMid 2 (3 / 100) (1 / 2) -- Same as above, modified to have 3 columns
    -- crunch = noBorders (Mirror Accordion) -- Mirrored to the longest portion of screen
    unstack = Mirror zoomRow
    stacked = zoomRow -- Basically accordion
    center = centerMaster (noBorders Grid) -- Smart borders for master, none for subs
    portr = OneBig (1 / 1) (1 / 3) -- OneBig (ScreenWidth) (ScreenHeight)
    lands = OneBig (6 / 9) (5 / 8) -- OneBig (ScreenWidth) (ScreenHeight)
    bighead = combineTwo (Tall 1 (3 / 100) (5 / 8)) (portr) (stacked)

myEventHook = mempty

--myWorkspaces = ["", "ε", "ζ", "β", "δ", "η", "θ", "λ"] -- Workspace declaration
myWorkspaces    = ["q","w","e", "ε", "ζ", "β", "δ", "θ", "λ"] --Workspace declaration
--myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8"] -- Workspace declaration

myStartupHook = do
  spawnOnce "xset r rate 185 30"
  spawnOnce "xmobar &"

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 9,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- \| Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ ((modm .|. shiftMask, xK_s), unGrab *> spawn "screenshot"),
      ((modm .|. shiftMask, xK_a), unGrab *> spawn "screenshot2"),
      ((modm, xK_g), kill), -- rebind restart xmonad to quit current window
      -- ("M-S q", ""), -- unbind quit
      -- ("M-S <Return>", Nothing), -- unbind terminal
      ((modm, xK_Return), spawn myTerminal), -- rebind terminal (alacritty)
      -- ("M p", Nothing), -- unbind dmenu
      ((modm .|. shiftMask, xK_c), spawn "chromium"),
      ((modm .|. shiftMask, xK_f), spawn "firefox"),
      ((modm .|. shiftMask, xK_d), unGrab *> spawn "Discord"), -- this opens launcher and we don't want to focus on it
      ((modm .|. shiftMask, xK_f), spawn "pcmanfm"),
      ((modm .|. shiftMask, xK_l), spawn "sleep 0.5;xset dpms force off"),
      ((modm .|. shiftMask, xK_v), spawn "sleep 0.5;xdotool type $(xclip -o)"),
      ((0, xF86XK_AudioRaiseVolume), unGrab *> spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"), -- don't want to focus on any of these
      ((0, xF86XK_AudioLowerVolume), unGrab *> spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
      ((0, xF86XK_AudioPlay), unGrab *> spawn "playerctl play-pause"),
      ((0, xF86XK_AudioNext), unGrab *> spawn "playerctl next"),
      ((0, xF86XK_AudioPrev), unGrab *> spawn "playerctl previous"),
      ((0, xF86XK_AudioMute), unGrab *> spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
      ((0, xF86XK_AudioMicMute), unGrab *> spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle"),
      ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart"),
      ((modm, xK_j), windows W.focusDown),
      ((modm, xK_k), windows W.focusUp),
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      ((modm, xK_d), nextScreen),
      ((modm, xK_semicolon), windows W.focusMaster),
      ((modm, xK_bracketleft), sendMessage NextLayout),
      ((modm, xK_s), sendMessage $ IncMasterN 1),
      ((modm, xK_a), sendMessage $ IncMasterN (-1)),
      ((modm, xK_t), namedScratchpadAction myScratchPads "terminal"),
      ((modm, xK_space), spawn "rofi -show combi -sidebar-mode 2>&1 | tee ~/.local/log/rofi"),
      ((modm .|. shiftMask, xK_space), withFocused toggleFloat),
      ((modm .|. controlMask, xK_h), sendMessage Shrink),
      ((modm .|. controlMask, xK_l), sendMessage Expand)
    ]
    ++ [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (workspaces conf) [xK_q, xK_w, xK_e, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6],
         (f, m) <- [(W.view, 0), (W.shift, controlMask)]
     ]
    {-
      ++ [ ((m .|. modm, k), windows $ onCurrentScreen f i)
           | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_7],
             (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]
         ]
    -}

main :: IO ()
main = do
  nScreens <- countScreens
  xmonad
    $ ewmh
    $ ewmhFullscreen
    . withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
    $ def
      { terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        clickJustFocuses = myClickJustFocuses,
        borderWidth = 0,
        modMask = myModMask,
        focusedBorderColor = "#E8C381",
        normalBorderColor = "#257283",
        layoutHook = myLayoutHook,
        manageHook = myManageHook,
        handleEventHook = myEventHook,
        logHook = myLogHook,
        mouseBindings = myMouseBinds,
        --workspaces = withScreens nScreens myWorkspaces,
        workspaces = myWorkspaces,
        startupHook = myStartupHook,
        keys = myKeys
      }
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = m} = (m, xK_F24) -- never want to hide
