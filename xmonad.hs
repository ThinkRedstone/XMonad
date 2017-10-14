
import Data.Default
import qualified Data.Map as M
import System.Exit
import Data.List
import Data.Char

import XMonad
import System.Taffybar.Hooks.PagerHints
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)

import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Util.WindowProperties
import XMonad.Actions.CycleWS

import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.InsertPosition

import XMonad.Hooks.EwmhDesktops

instance Default (Tall a) where
    def = Tall 1 0.05 0.65

instance Default (TwoPane a) where
    def = TwoPane 0.05 0.5

myLayout = avoidStruts $ smartBorders $ onWorkspace "a" (Full ||| (def :: Tall a)) $ onWorkspace "s" ((def :: Tall a) ||| Full) $ onWorkspace "d" (Tall 1 0.05 0.5) $ onWorkspace "f" (def :: TwoPane a) $ (GridRatio (1/1)) ||| (def :: Tall a)

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. controlMask, xK_t), spawn "xfce4-terminal") -- %! Launch terminal
    , ((modMask .|. controlMask, xK_f), spawn "thunar") -- %! Launch file browser
    , ((modMask,                 xK_r), spawn "dmenu_run -sf '#ff0000' -sb '#111111'") -- %! Launch dmenu
    , ((modMask,                 xK_q), whenX (focusedHasProperty (Not $ ClassName "dota2")) kill ) -- %! Close the focused window

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_Down  ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_Up    ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- quit, or restart
    , ((modMask .|. mod4Mask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask .|. mod4Mask, xK_r     ), spawn "xmonad --recompile && xmonad --restart && killall taffybar-linux-x86_64 && taffybar") -- %! Restart xmonad and taffybar
    -- media controls
    , ((controlMask .|. shiftMask, xK_KP_Begin ), spawn "clementine --play-pause") -- %! play pause clementine
    , ((controlMask .|. shiftMask, xK_KP_Up ),    spawn "clementine --volume-up") -- %! volume up
    , ((controlMask .|. shiftMask, xK_KP_Down),   spawn "clementine --volume-down") -- %! volume down
    , ((controlMask .|. shiftMask, xK_KP_Right ), spawn "clementine --next") -- %! next song
    , ((controlMask .|. shiftMask, xK_KP_Left ),  spawn "clementine --previous") -- %! previous song
    -- Switch to nearby workspaces
    , ((controlMask .|. modMask, xK_Right              ),  nextWS) -- %! switch to workspace on the right
    , ((controlMask .|. modMask, xK_Left               ),  prevWS) -- %! switch to workspace on the left
    , ((shiftMask .|. controlMask .|. modMask, xK_Right),  shiftToNext >> nextWS) -- %! shift window to workspace on the right
    , ((shiftMask .|. controlMask .|. modMask, xK_Left ),  shiftToPrev >> prevWS) -- %! shift window to workspace on the left

    , (( modMask, xK_f ),  windows $ W.greedyView "f") -- %! switch to workspace f (we dont want the shortcuts to move windows into f)
    , ((controlMask .|. modMask, xK_r), spawn "roccatnythcontrol -a 1")
    ]
    ++
    --switch to, or switch window to, a specific workspace
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (delete "f" (XMonad.workspaces conf)) [xK_a, xK_s, xK_d, xK_z, xK_x, xK_c, xK_v]
        , (f, m) <- [(W.greedyView, 0), (\i -> W.greedyView i . W.shift i, shiftMask), (W.shift, controlMask)]]

myMouse (XConfig {XMonad.modMask = modMask}) = M.fromList [
    -- mod-button1 %! Set the window to floating mode and move by dragging
      ((modMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    -- mod-button2 %! unfloat the window
    , ((modMask, button2),  windows . W.sink)
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> Flex.mouseWindow Flex.resize w >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Sort windows
windowSortHook = composeAll . concat $
    [ [isDialog --> doFloat <+> insertPosition Master Newer] --insert dialog in master because otherwise it gets covered by other dialog and float it
    , [(className =? x <||> title =? "Steam") --> insertPosition Master Newer | x <- masters] --Insert windows in masters as the master window; steam has annoying window classes so we match it by title
    , [insertPosition Below Newer]
    , [(className =? x <&&> appName =? (map toLower x)) --> doShift "a" | x <- myShifts "a"]
    , [(className =? x ) --> doShift "s" | x <- myShifts "s"]
    , [(className =? x ) --> doShift "d" | x <- myShifts "d"]
    , [(className =? x ) --> doShift "f" | x <- myShifts "f"]
    , [(className =? x ) --> doShift "z" | x <- myShifts "z"]
    , [(className =? x ) --> doShift "x" | x <- myShifts "x"]
    , [(className =? x ) --> doShift "c" | x <- myShifts "c"]
    , [(className =? x ) --> doShift "v" | x <- myShifts "v"]
    , [ isFullscreen --> doFullFloat]
    ]
    where
    myShifts "a" = ["Chromium"]
    myShifts "s" = ["jetbrains-pycharm-ce", "jetbrains-idea-ce", "dota2", "Atom"]
    myShifts "d" = ["Skype", "Steam", "discord"]
    myShifts "f" = ["Clementine", "Deluge"]
    myShifts "z" = []
    myShifts "x" = []
    myShifts "c" = []
    myShifts "v" = []
    masters = ["jetbrains-pycharm-ce", "jetbrains-idea-ce", "Deluge"]


main = xmonad $ ewmh $ pagerHints $ def {focusFollowsMouse = False,
                                         clickJustFocuses = False,
                                         layoutHook = fullscreenFull myLayout,
                                         workspaces = [a:[]| a<-"asdfzxcv"],
                                         keys = myKeys,
                                         mouseBindings = myMouse,
                                         manageHook= manageDocks <+> windowSortHook <+> manageHook def,
                                         handleEventHook = fullscreenEventHook <+> docksEventHook,
                                         startupHook = setWMName "LG3D"}
