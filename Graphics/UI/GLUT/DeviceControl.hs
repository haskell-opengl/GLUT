--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.DeviceControl
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- GLUT offers some routines for setting the key repeat and polling the
-- joystick.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.DeviceControl (
   setGlobalKeyRepeat, setPerWindowKeyRepeat,
   forceJoystickCallback
) where

import Foreign.C.Types ( CInt )
import Graphics.UI.GLUT.State ( GlobalKeyRepeat, marshalGlobalKeyRepeat,
                                PerWindowKeyRepeat, marshalPerWindowKeyRepeat )

--------------------------------------------------------------------------------

-- | Set the key repeat mode for the window system on a global basis if
-- possible. If supported by the window system, the key repeat can either be
-- disabled, enabled, or set to the window system\'s default key repeat state.
--
-- /X Implementation Notes:/ X11 sends @KeyPress@ events repeatedly when the
-- window system\'s global auto repeat is enabled. 'setPerWindowKeyRepeat' can
-- prevent these auto repeated keystrokes from being reported as keyboard or
-- special callbacks, but there is still some minimal overhead by the X server
-- to continually stream @KeyPress@ events to the GLUT application. The
-- 'setGlobalKeyRepeat' routine can be used to actually disable the global
-- sending of auto repeated @KeyPress@ events. Note that 'setGlobalKeyRepeat'
-- affects the global window system auto repeat state so other applications
-- will not auto repeat if you disable auto repeat globally through
-- 'setGlobalKeyRepeat'. GLUT applications using the X11 GLUT implementation
-- should disable key repeat with glutSetKeyRepeat to disable key repeats most
-- efficiently, but are responsible for explicitly restoring the default key
-- repeat state on exit.
--
-- /Win32 Implementation Notes:/ The Win32 implementation of 'setGlobalKeyRepeat'
-- does nothing. The 'setPerWindowKeyRepeat' can be used in the Win32 GLUT
-- implementation to ignore repeated keys on a per-window basis without changing
-- the global window system key repeat.

setGlobalKeyRepeat :: GlobalKeyRepeat -> IO ()
setGlobalKeyRepeat = glutSetKeyRepeat . marshalGlobalKeyRepeat

foreign import CALLCONV unsafe "glutSetKeyRepeat" glutSetKeyRepeat ::
   CInt -> IO ()

--------------------------------------------------------------------------------

-- | Determine if auto repeat keystrokes are reported to the /current window./
-- The ignore auto repeat state of a window can be queried with
-- 'Graphics.UI.GLUT.State.getKeyboardInfo'. Ignoring auto repeated keystrokes
-- is generally done in conjunction with using the
-- 'Graphics.UI.GLUT.Callbacks.Window.setKeyboardMouseCallback'. If you do
-- not ignore auto repeated keystrokes, your GLUT application will experience
-- repeated release\/press callbacks. Games using the keyboard will typically
-- want to ignore key repeat.

setPerWindowKeyRepeat :: PerWindowKeyRepeat -> IO ()
setPerWindowKeyRepeat = glutIgnoreKeyRepeat . marshalPerWindowKeyRepeat

foreign import CALLCONV unsafe "glutIgnoreKeyRepeat" glutIgnoreKeyRepeat ::
   CInt -> IO ()

--------------------------------------------------------------------------------

-- | Execute the joystick callback set by
-- 'Graphics.UI.GLUT.Callbacks.Window.setJoystickCallback' once (if one exists).
-- This is done in a synchronous fashion within the current context, i.e. when
-- 'forceJoystickCallback' returns, the callback will have already happened.
		
foreign import CALLCONV unsafe "glutForceJoystickFunc" forceJoystickCallback ::
   IO ()
