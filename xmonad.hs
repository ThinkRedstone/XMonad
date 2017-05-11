{-# LANGUAGE FlexibleContexts #-}

import Data.Default
import qualified Data.Map as M
import System.Exit

import XMonad

import XMonad.Layout
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane

import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties

import XMonad.ManageHook
import XMonad.Hooks.ManageDocks


instance Default (Tall a) where
    def = Tall 1 0.05 0.65

instance Default (TwoPane a) where
    def = TwoPane 0.05 0.5

myLayout = avoidStruts $ onWorkspace "a" (Full ||| (def :: Tall a)) $ onWorkspace "s" ((def :: Tall a) ||| Full) $ onWorkspace "d" (def :: Tall a) $ onWorkspace "f" (def :: TwoPane a) $ Grid ||| (def :: Tall a)

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. controlMask, xK_t), spawn "xfce4-terminal") -- %! Launch terminal
    , ((modMask .|. controlMask, xK_f), spawn "thunar") -- %! Launch file browser
    , ((modMask,                 xK_r), spawn "dmenu_run") -- %! Launch dmenu
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
--    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- quit, or restart
    , ((modMask .|. mod4Mask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask .|. mod4Mask, xK_r     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    ]
    ++
    --switch to, or switch window to, a specific workspace
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_a, xK_s, xK_d, xK_f, xK_z, xK_x, xK_c, xK_v]
        , (f, m) <- [(W.greedyView, 0), (\i -> W.greedyView i . W.shift i, shiftMask)]]


main = xmonad $ def {layoutHook = myLayout, workspaces = [a:[]| a<-"asdfzxcv"], keys = myKeys}