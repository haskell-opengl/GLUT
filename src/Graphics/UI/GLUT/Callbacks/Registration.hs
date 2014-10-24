{-# OPTIONS_GHC -fno-cse #-}

{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Callbacks.Registration
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Callbacks.Registration (
   CallbackType(..), registerForCleanup, setCallback, getCurrentWindow
) where

--------------------------------------------------------------------------------

import Control.Monad
import Data.IORef
import qualified Data.Map as Map ( empty, lookup, insert, delete )
import Data.Map ( Map )
import Foreign.Ptr
import Graphics.Rendering.OpenGL ( get )
import Graphics.UI.GLUT.Raw
import Graphics.UI.GLUT.Window

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
   -- freeglut-only callback types
   | CloseCB           | MouseWheelCB      | PositionCB
   | MultiEntryCB      | MultiMotionCB     | MultiButtonCB
   | MultiPassiveCB    | InitContextCB     | AppStatusCB
   deriving ( Eq, Ord )

isGlobal :: CallbackType -> Bool
isGlobal MenuStatusCB = True
isGlobal IdleCB       = True
isGlobal _            = False

--------------------------------------------------------------------------------
-- To uniquely identify a particular callback, the associated window is needed
-- for window callbacks.

data CallbackID = CallbackID (Maybe Window) CallbackType
   deriving ( Eq, Ord )

getCallbackID :: CallbackType -> IO CallbackID
getCallbackID callbackType = do
   maybeWindow <- if isGlobal callbackType
                     then return Nothing
                     else fmap Just $ getCurrentWindow "getCallbackID"
   return $ CallbackID maybeWindow callbackType

getCurrentWindow :: String -> IO Window
getCurrentWindow func = do
   win <- get currentWindow
   maybe (error (func ++ ": no current window")) return win

--------------------------------------------------------------------------------
-- This seems to be a common Haskell hack nowadays: A plain old global variable
-- with an associated mutator. Perhaps some language/library support is needed?

{-# NOINLINE theCallbackTable #-}
theCallbackTable :: IORef (CallbackTable a)
theCallbackTable = unsafePerformIO (newIORef emptyCallbackTable)

getCallbackTable :: IO (CallbackTable a)
getCallbackTable = readIORef theCallbackTable

modifyCallbackTable :: (CallbackTable a -> CallbackTable a) -> IO ()
modifyCallbackTable = modifyIORef theCallbackTable

--------------------------------------------------------------------------------

type CallbackTable a = Map CallbackID (FunPtr a)

emptyCallbackTable :: CallbackTable a
emptyCallbackTable = Map.empty

lookupInCallbackTable :: CallbackID -> IO (Maybe (FunPtr a))
lookupInCallbackTable callbackID =
   fmap (Map.lookup callbackID) getCallbackTable

deleteFromCallbackTable :: CallbackID -> IO ()
deleteFromCallbackTable callbackID =
   modifyCallbackTable (Map.delete callbackID)

addToCallbackTable :: CallbackID -> FunPtr a -> IO ()
addToCallbackTable callbackID funPtr =
   modifyCallbackTable (Map.insert callbackID funPtr)

--------------------------------------------------------------------------------
-- Another global mutable variable: The list of function pointers ready to be
-- freed by freeHaskellFunPtr

{-# NOINLINE theCleanupList #-}
theCleanupList :: IORef [FunPtr a]
theCleanupList = unsafePerformIO (newIORef [])

getCleanupList :: IO [FunPtr a]
getCleanupList = readIORef theCleanupList

setCleanupList :: [FunPtr a] -> IO ()
setCleanupList = writeIORef theCleanupList

--------------------------------------------------------------------------------
-- And yet another mutable (write-once) variable: A function pointer to a
-- callback which frees all function pointers on the cleanup list.

{-# NOINLINE theScavenger #-}
theScavenger :: IORef (FunPtr TimerFunc)
theScavenger = unsafePerformIO (newIORef =<< makeTimerFunc (\_ -> do
   cleanupList <- getCleanupList
   mapM_ freeHaskellFunPtr cleanupList
   setCleanupList []))

getScavenger :: IO (FunPtr TimerFunc)
getScavenger = readIORef theScavenger

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
