--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Callbacks.Global
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Callbacks.Global (
   -- * Menu status callback
   MenuUsage(..), MenuStatusCallback, setMenuStatusCallback,

   -- * Idle callback
   IdleCallback, setIdleCallback,

   -- * Timer callbacks
   Timeout, TimerCallback, setTimerCallback
) where

import Control.Monad.Fix ( MonadFix(..) )
import Foreign.C.Types ( CInt, CUInt )
import Foreign.Ptr ( FunPtr, nullFunPtr, freeHaskellFunPtr )
import Graphics.UI.GLUT.Constants
import Graphics.UI.GLUT.Initialization ( WindowPosition(..) )

--------------------------------------------------------------------------------

-- TODO
trackGlobalCallback ::
   (a -> IO (FunPtr b)) -> (FunPtr b -> IO ()) -> Maybe a -> IO ()
trackGlobalCallback makeCB registerCB mbAct = do
   funPtr <- case mbAct of
                Nothing  -> return nullFunPtr
                Just act -> makeCB act
   registerCB funPtr

--------------------------------------------------------------------------------

data MenuUsage
   = NotInUse
   | InUse
   deriving ( Eq, Ord )

unmarshalMenuUsage :: CInt -> MenuUsage
unmarshalMenuUsage u
   | u == glut_MENU_NOT_IN_USE  = NotInUse
   | u == glut_MENU_IN_USE      = InUse
   | otherwise = error "unmarshalMenuUsage"

type MenuStatusCallback  = MenuUsage -> WindowPosition -> IO ()

type MenuStatusCallback' = CInt -> CInt -> CInt -> IO ()

-- | Set the global menu status callback so a GLUT program can determine when a
-- menu is in use or not. When a menu status callback is registered, it will be
-- called with the value 'InUse' when pop-up menus are in use by the user; and
-- the callback will be called with the value 'NotInUse' when pop-up menus are
-- no longer in use. Additionally, the location in window coordinates of the
-- button press that caused the menu to go into use, or the location where the
-- menu was released (maybe outside the window). Other callbacks continue to
-- operate (except mouse motion callbacks) when pop-up menus are in use so the
-- menu status callback allows a program to suspend animation or other tasks
-- when menus are in use. The cascading and unmapping of sub-menus from an
-- initial pop-up menu does not generate menu status callbacks. There is a
-- single menu status callback for GLUT.
--
-- When the menu status callback is called, the /current menu/ will be set to
-- the initial pop-up menu in both the 'InUse' and 'NotInUse' cases. The
-- /current window/ will be set to the window from which the initial menu was
-- popped up from, also in both cases.
--
-- Passing 'Nothing' to 'setMenuStatusCallback' disables the generation of the
-- menu status callback.

setMenuStatusCallback :: Maybe MenuStatusCallback -> IO ()
setMenuStatusCallback =
   trackGlobalCallback (makeMenuStatusCallback . unmarshal) glutMenuStatusFunc
      where unmarshal cb s x y = cb (unmarshalMenuUsage s) (WindowPosition x y)

foreign import ccall "wrapper" makeMenuStatusCallback ::
   MenuStatusCallback' -> IO (FunPtr MenuStatusCallback')

foreign import ccall unsafe "glutMenuStatusFunc" glutMenuStatusFunc ::
   FunPtr MenuStatusCallback' -> IO ()

--------------------------------------------------------------------------------

type IdleCallback = IO ()

-- | Set the global idle callback so a GLUT program can perform background
-- processing tasks or continuous animation when window system events are not
-- being received. If enabled, the idle callback is continuously called when
-- events are not being received. The /current window/ and /current menu/ will
-- not be changed before the idle callback. Programs with multiple windows
-- and\/or menus should explicitly set the /current window/ and\/or /current
-- menu/ and not rely on its current setting.
--
-- The amount of computation and rendering done in an idle callback should be
-- minimized to avoid affecting the program\'s interactive response. In general,
-- not more than a single frame of rendering should be done in an idle callback.
--
-- Passing 'Nothing' to 'setIdleCallback' disables the generation of the idle
-- callback.

setIdleCallback :: Maybe IdleCallback -> IO ()
setIdleCallback = trackGlobalCallback makeIdleCallback glutIdleFunc

foreign import ccall "wrapper" makeIdleCallback ::
   IdleCallback -> IO (FunPtr IdleCallback)

foreign import ccall unsafe "glutIdleFunc" glutIdleFunc ::
   FunPtr IdleCallback -> IO ()

--------------------------------------------------------------------------------

-- | Timeout for the timer callback in milliseconds
type Timeout = CUInt

type TimerCallback  = IO ()

type TimerCallback' = CInt -> IO ()

-- | Register a one-shot timer callback to be triggered after at least the given
-- amount of time. Multiple timer callbacks at same or differing times may be
-- registered simultaneously. There is no support for canceling a registered
-- callback.
--
-- The number of milliseconds is a lower bound on the time before the callback
-- is generated. GLUT attempts to deliver the timer callback as soon as possible
-- after the expiration of the callback\'s time interval.

setTimerCallback :: Timeout -> TimerCallback -> IO ()
setTimerCallback msecs timerCallback = do
   ptr <- mfix (\self -> makeTimerCallback (\_ -> do freeHaskellFunPtr self
                                                     timerCallback))
   glutTimerFunc msecs ptr 0

foreign import ccall "wrapper" makeTimerCallback ::
   TimerCallback' -> IO (FunPtr TimerCallback')

foreign import ccall unsafe "glutTimerFunc" glutTimerFunc ::
   Timeout -> FunPtr TimerCallback' -> CInt -> IO ()
