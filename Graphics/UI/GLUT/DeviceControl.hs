--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.DeviceControl
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- GLUT offers some routines for controlling the key repeat and polling the
-- joystick.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.DeviceControl (
   GlobalKeyRepeat(..), globalKeyRepeat,
   PerWindowKeyRepeat(..), perWindowKeyRepeat,
   forceJoystickCallback
) where

import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
import Graphics.UI.GLUT.Constants (
   glut_KEY_REPEAT_OFF, glut_KEY_REPEAT_ON, glut_KEY_REPEAT_DEFAULT,
   glut_DEVICE_KEY_REPEAT, glut_DEVICE_IGNORE_KEY_REPEAT )
import Graphics.UI.GLUT.QueryUtils ( deviceGet )

--------------------------------------------------------------------------------

-- | The state of the global key repeat

data GlobalKeyRepeat
   = GlobalKeyRepeatOff
   | GlobalKeyRepeatOn
   | GlobalKeyRepeatDefault
   deriving ( Eq, Ord, Show )

marshalGlobalKeyRepeat :: GlobalKeyRepeat -> CInt
marshalGlobalKeyRepeat GlobalKeyRepeatOff     = glut_KEY_REPEAT_OFF
marshalGlobalKeyRepeat GlobalKeyRepeatOn      = glut_KEY_REPEAT_ON
marshalGlobalKeyRepeat GlobalKeyRepeatDefault = glut_KEY_REPEAT_DEFAULT

unmarshalGlobalKeyRepeat :: CInt -> GlobalKeyRepeat
unmarshalGlobalKeyRepeat r
   | r == glut_KEY_REPEAT_OFF     = GlobalKeyRepeatOff
   | r == glut_KEY_REPEAT_ON      = GlobalKeyRepeatOn
   | r == glut_KEY_REPEAT_DEFAULT = GlobalKeyRepeatDefault
   | otherwise = error "unmarshalGlobalKeyRepeat"

--------------------------------------------------------------------------------

-- | Controls the key repeat mode for the window system on a global basis if
-- possible. If supported by the window system, the key repeat can either be
-- disabled, enabled, or set to the window system\'s default key repeat state.
--
-- /X Implementation Notes:/ X11 sends @KeyPress@ events repeatedly when the
-- window system\'s global auto repeat is enabled. 'perWindowKeyRepeat' can
-- prevent these auto repeated keystrokes from being reported as keyboard or
-- special callbacks, but there is still some minimal overhead by the X server
-- to continually stream @KeyPress@ events to the GLUT application. The
-- 'globalKeyRepeat' state variable can be used to actually disable the global
-- sending of auto repeated @KeyPress@ events. Note that 'globalKeyRepeat'
-- affects the global window system auto repeat state so other applications
-- will not auto repeat if you disable auto repeat globally through
-- 'globalKeyRepeat'. GLUT applications using the X11 GLUT implementation
-- should disable key repeat with 'globalKeyRepeat' to disable key repeats most
-- efficiently, but are responsible for explicitly restoring the default key
-- repeat state on exit.
--
-- /Win32 Implementation Notes:/ The Win32 implementation of 'globalKeyRepeat'
-- does nothing. The 'perWindowKeyRepeat' can be used in the Win32 GLUT
-- implementation to ignore repeated keys on a per-window basis without changing
-- the global window system key repeat.

globalKeyRepeat :: StateVar GlobalKeyRepeat
globalKeyRepeat =
   makeStateVar (deviceGet unmarshalGlobalKeyRepeat glut_DEVICE_KEY_REPEAT)
                (glutSetKeyRepeat . marshalGlobalKeyRepeat)

foreign import CALLCONV unsafe "glutSetKeyRepeat" glutSetKeyRepeat ::
   CInt -> IO ()

--------------------------------------------------------------------------------

-- | The state of the per-window key repeat

data PerWindowKeyRepeat
   = PerWindowKeyRepeatOff
   | PerWindowKeyRepeatOn
   deriving ( Eq, Ord, Show )

marshalPerWindowKeyRepeat :: PerWindowKeyRepeat -> CInt
marshalPerWindowKeyRepeat PerWindowKeyRepeatOff = 0
marshalPerWindowKeyRepeat PerWindowKeyRepeatOn  = 1

unmarshalPerWindowKeyRepeat :: CInt -> PerWindowKeyRepeat
unmarshalPerWindowKeyRepeat 0 = PerWindowKeyRepeatOff
unmarshalPerWindowKeyRepeat _ = PerWindowKeyRepeatOn

--------------------------------------------------------------------------------

-- | Controls if auto repeat keystrokes are reported to the /current window./
-- Ignoring auto repeated keystrokes is generally done in conjunction with using
-- the 'Graphics.UI.GLUT.Callbacks.Window.keyboardMouseCallback'. If you do
-- not ignore auto repeated keystrokes, your GLUT application will experience
-- repeated release\/press callbacks. Games using the keyboard will typically
-- want to ignore key repeat.

perWindowKeyRepeat :: StateVar PerWindowKeyRepeat
perWindowKeyRepeat =
   makeStateVar
      (deviceGet unmarshalPerWindowKeyRepeat glut_DEVICE_IGNORE_KEY_REPEAT)
      (glutIgnoreKeyRepeat . marshalPerWindowKeyRepeat)

foreign import CALLCONV unsafe "glutIgnoreKeyRepeat" glutIgnoreKeyRepeat ::
   CInt -> IO ()

--------------------------------------------------------------------------------

-- | Execute the joystick callback set by
-- 'Graphics.UI.GLUT.Callbacks.Window.joystickCallback' once (if one exists).
-- This is done in a synchronous fashion within the current context, i.e. when
-- 'forceJoystickCallback' returns, the callback will have already happened.
		
foreign import CALLCONV unsafe "glutForceJoystickFunc" forceJoystickCallback ::
   IO ()
