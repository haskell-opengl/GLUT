--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Begin
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- After a GLUT program has done initial setup such as creating windows and
-- menus, GLUT programs enter the GLUT event processing loop by calling
-- 'mainLoop' or handle events iteratively with 'mainLoopEvent'.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Begin (
   -- * Handling events
   mainLoop, mainLoopEvent, leaveMainLoop,

   -- * Controlling the behaviour when windows are closed
   ActionOnWindowClose(..), actionOnWindowClose
) where

import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
import Graphics.UI.GLUT.Constants (
   glut_ACTION_ON_WINDOW_CLOSE, glut_ACTION_EXIT,
   glut_ACTION_GLUTMAINLOOP_RETURNS, glut_ACTION_CONTINUE_EXECUTION )
import Graphics.UI.GLUT.QueryUtils ( simpleGet, glutSetOption )
import Graphics.UI.GLUT.Extensions

--------------------------------------------------------------------------------

#include "HsGLUTExt.h"

--------------------------------------------------------------------------------

-- | Enter the GLUT event processing loop; it will call as necessary any
-- callbacks that have been registered. This routine should be called at most
-- once in a GLUT program.

foreign import CALLCONV safe "glutMainLoop" mainLoop :: IO ()

--------------------------------------------------------------------------------

-- | (/freeglut only/) Process one iteration's worth of events in its event loop.
-- This allows the application to control its own event loop and still use the
-- GLUT package.

mainLoopEvent :: IO ()
mainLoopEvent = glutMainLoopEvent

EXTENSION_ENTRY(safe,"freeglut",glutMainLoopEvent,IO ())

--------------------------------------------------------------------------------

-- | (/freeglut only/) Stop the event loop. If 'actionOnWindowClose' contains
-- 'Exit', the application will exit; otherwise control will return to the
-- function which called 'mainLoop'.
--
-- If the application has two nested calls to 'mainLoop' and calls
-- 'leaveMainLoop', the behaviour is undefined. It may leave only the inner
-- nested loop or it may leave both loops. If the reader has a strong preference
-- for one behaviour over the other he should contact the freeglut Programming
-- Consortium and ask for the code to be fixed. 

leaveMainLoop :: IO ()
leaveMainLoop = glutLeaveMainLoop

EXTENSION_ENTRY(safe,"freeglut",glutLeaveMainLoop,IO ())

--------------------------------------------------------------------------------

-- | The behaviour when the user closes a window.

data ActionOnWindowClose
   = -- | Exit the whole program when any window is closed or 'leaveMainLoop'
     -- is called (default).
     Exit
   | -- | Return from mainLoop when any window is closed.
     MainLoopReturns
   | -- | Return from mainLoop after the last window is closed.
     ContinueExectuion
   deriving ( Eq, Ord, Show )

marshalActionOnWindowClose :: ActionOnWindowClose -> CInt
marshalActionOnWindowClose x = case x of
   Exit ->  glut_ACTION_EXIT
   MainLoopReturns -> glut_ACTION_GLUTMAINLOOP_RETURNS
   ContinueExectuion -> glut_ACTION_CONTINUE_EXECUTION

unmarshalActionOnWindowClose :: CInt -> ActionOnWindowClose
unmarshalActionOnWindowClose x
   | x == glut_ACTION_EXIT = Exit
   | x == glut_ACTION_GLUTMAINLOOP_RETURNS = MainLoopReturns
   | x == glut_ACTION_CONTINUE_EXECUTION = ContinueExectuion
   | otherwise = error ("unmarshalActionOnWindowClose: illegal value " ++ show x)

-----------------------------------------------------------------------------

-- | (/freeglut only/) Controls the behaviour when the user closes a window.

actionOnWindowClose :: StateVar ActionOnWindowClose
actionOnWindowClose =
   makeStateVar
      (simpleGet unmarshalActionOnWindowClose glut_ACTION_ON_WINDOW_CLOSE)
      (glutSetOption glut_ACTION_ON_WINDOW_CLOSE . marshalActionOnWindowClose)
