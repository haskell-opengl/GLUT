-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Callbacks.Registration
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Callbacks.Registration (
   CallbackType(..), registerForCleanup, setCallback
) where

--------------------------------------------------------------------------------

import Control.Monad ( liftM, when )
import Data.FiniteMap ( FiniteMap, emptyFM, lookupFM, addToFM, delFromFM )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )
import Foreign.C.Types ( CInt, CUInt )
import Foreign.Ptr ( FunPtr, nullFunPtr, freeHaskellFunPtr )
import System.IO.Unsafe ( unsafePerformIO )
import Graphics.Rendering.OpenGL.GL.StateVar ( HasGetter(get) )
import Graphics.UI.GLUT.Window ( Window, currentWindow )

--------------------------------------------------------------------------------
-- No timer callback here, because they are one-shot and "self destroy"

data CallbackType
   = DisplayCB         | OverlayDisplayCB  | ReshapeCB
   | KeyboardCB        | KeyboardUpCB      | MouseCB
   | MotionCB          | PassiveMotionCB   | CrossingCB
   | VisibilityCB      | WindowStatusCB    | SpecialCB
   | SpecialUpCB       | SpaceballMotionCB | SpaceballRotateCB
   | SpaceballButtonCB | ButtonBoxCB       | DialsCB
   | TabletMotionCB    | TabletButtonCB    | JoystickCB
   | MenuStatusCB      | IdleCB
   deriving ( Eq, Ord, Show )

isGlobal :: CallbackType -> Bool
isGlobal MenuStatusCB = True
isGlobal IdleCB       = True
isGlobal _            = False

--------------------------------------------------------------------------------
-- To uniquely identify a particular callback, the associated window is needed
-- for window callbacks.

data CallbackID = CallbackID (Maybe Window) CallbackType
   deriving ( Eq, Ord, Show )

getCallbackID :: CallbackType -> IO CallbackID
getCallbackID callbackType = do
   maybeWindow <- if isGlobal callbackType
                     then return Nothing
                     else liftM Just $ get currentWindow
   return $ CallbackID maybeWindow callbackType

--------------------------------------------------------------------------------
-- This seems to be a common Haskell hack nowadays: A plain old global variable
-- with an associated mutator. Perhaps some language/library support is needed?

{-# notInline theCallbackTable #-}
theCallbackTable :: IORef (CallbackTable a)
theCallbackTable = unsafePerformIO (newIORef emptyCallbackTable)

getCallbackTable :: IO (CallbackTable a)
getCallbackTable = readIORef theCallbackTable

modifyCallbackTable :: (CallbackTable a -> CallbackTable a) -> IO ()
modifyCallbackTable = modifyIORef theCallbackTable

--------------------------------------------------------------------------------

type CallbackTable a = FiniteMap CallbackID (FunPtr a)

emptyCallbackTable :: CallbackTable a
emptyCallbackTable = emptyFM

lookupInCallbackTable :: CallbackID -> IO (Maybe (FunPtr a))
lookupInCallbackTable callbackID =
   liftM (flip lookupFM callbackID) getCallbackTable

deleteFromCallbackTable :: CallbackID -> IO ()
deleteFromCallbackTable callbackID =
   modifyCallbackTable (flip delFromFM callbackID)

addToCallbackTable :: CallbackID -> FunPtr a -> IO ()
addToCallbackTable callbackID funPtr =
   modifyCallbackTable (\table -> addToFM table callbackID funPtr)

--------------------------------------------------------------------------------
-- Another global mutable variable: The list of function pointers ready to be
-- freed by freeHaskellFunPtr

{-# notInline theCleanupList #-}
theCleanupList :: IORef [FunPtr a]
theCleanupList = unsafePerformIO (newIORef [])

getCleanupList :: IO [FunPtr a]
getCleanupList = readIORef theCleanupList

setCleanupList :: [FunPtr a] -> IO ()
setCleanupList = writeIORef theCleanupList

--------------------------------------------------------------------------------
-- And yet another mutable (write-once) variable: A function pointer to a
-- callback which frees all function pointers on the cleanup list.

{-# notInline theScavenger #-}
theScavenger :: IORef (FunPtr TimerCallback)
theScavenger = unsafePerformIO (newIORef =<< makeTimerCallback (\_ -> do
   cleanupList <- getCleanupList
   mapM_ freeHaskellFunPtr cleanupList
   setCleanupList []))

getScavenger :: IO (FunPtr TimerCallback)
getScavenger = readIORef theScavenger

-- More or less copied from Global.hs to avoid mutual dependencies

type TimerCallback = CInt -> IO ()

foreign import ccall "wrapper" makeTimerCallback ::
   TimerCallback -> IO (FunPtr TimerCallback)

foreign import CALLCONV unsafe "glutTimerFunc" glutTimerFunc ::
   CUInt -> FunPtr TimerCallback -> CInt -> IO ()

--------------------------------------------------------------------------------
-- Here is the really cunning stuff: If an element is added to the cleanup list
-- when it is empty, register an immediate callback at GLUT to free the list as
-- soon as possible.

registerForCleanup :: FunPtr a -> IO ()
registerForCleanup funPtr = do
   oldCleanupList <- getCleanupList
   setCleanupList (funPtr : oldCleanupList)
   when (null oldCleanupList) $ do
        scavenger <- getScavenger
        glutTimerFunc 0 scavenger 0

--------------------------------------------------------------------------------

setCallback :: CallbackType -> (FunPtr a -> IO ()) -> (b -> IO (FunPtr a))
            -> Maybe b -> IO ()
setCallback callbackType registerAtGLUT makeCallback maybeCallback = do
   callbackID <- getCallbackID callbackType
   maybeOldFunPtr <- lookupInCallbackTable callbackID
   case maybeOldFunPtr of
      Nothing -> return ()
      Just oldFunPtr -> do registerForCleanup oldFunPtr
                           deleteFromCallbackTable callbackID
   case maybeCallback of
      Nothing -> registerAtGLUT nullFunPtr
      Just callback -> do newFunPtr <- makeCallback callback
                          addToCallbackTable callbackID newFunPtr
                          registerAtGLUT newFunPtr
 