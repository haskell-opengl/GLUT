--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Callbacks.Window
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Callbacks.Window (
   DisplayCallback, setDisplayCallback
) where

import Foreign.Ptr ( FunPtr )
import Graphics.UI.GLUT.Callbacks.Registration ( CallbackType(..), setCallback )

--------------------------------------------------------------------------------

type DisplayCallback = IO ()

-- | Set the display callback for the /current window./ When GLUT determines
-- that the normal plane for the window needs to be redisplayed, the display
-- callback for the window is called. Before the callback, the /current window/
-- is set to the window needing to be redisplayed and (if no overlay display
-- callback is registered) the /layer in use/ is set to the normal plane. The
-- display callback is called with no parameters. The entire normal plane region
-- should be redisplayed in response to the callback (this includes ancillary
-- buffers if your program depends on their state).
--
-- GLUT determines when the display callback should be triggered based on the
-- window\'s redisplay state. The redisplay state for a window can be either set
-- explicitly by calling 'Graphics.UI.GLUT.Window.postRedisplay' or implicitly
-- as the result of window damage reported by the window system. Multiple posted
-- redisplays for a window are coalesced by GLUT to minimize the number of
-- display callbacks called.
--
-- When an overlay is established for a window, but there is no overlay display
-- callback registered, the display callback is used for redisplaying both the
-- overlay and normal plane (that is, it will be called if either the redisplay
-- state or overlay redisplay state is set). In this case, the /layer in use/ is
-- not implicitly changed on entry to the display callback.
--
-- See 'setOverlayDisplayCallback' to understand how distinct callbacks for the
-- overlay and normal plane of a window may be established.
--
-- When a window is created, no display callback exists for the window. It is
-- the responsibility of the programmer to install a display callback for the
-- window before the window is shown. A display callback must be registered for
-- any window that is shown. If a window becomes displayed without a display
-- callback being registered, a fatal error occurs. There is no way to
-- \"deregister\" a display callback (though another callback routine can always
-- be registered).
--
-- Upon return from the display callback, the normal damaged state of the window
-- (returned by calling 'Graphics.UI.GLUT.State.isNormalDamaged') is cleared. If
-- there is no overlay display callback registered the overlay damaged state of
-- the window (returned by calling 'Graphics.UI.GLUT.State.isOverlayDamaged') is
-- also cleared.

setDisplayCallback :: DisplayCallback -> IO ()
setDisplayCallback =
   setCallback DisplayCB glutDisplayFunc makeDisplayCallback . Just

foreign import ccall "wrapper" makeDisplayCallback ::
   DisplayCallback -> IO (FunPtr DisplayCallback)

foreign import ccall unsafe "glutDisplayFunc" glutDisplayFunc ::
   FunPtr DisplayCallback -> IO ()

--------------------------------------------------------------------------------
