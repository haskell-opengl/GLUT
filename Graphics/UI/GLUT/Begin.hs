--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Begin
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- After a GLUT program has done initial setup such as creating windows and
-- menus, GLUT programs enter the GLUT event processing loop by calling
-- 'mainLoop'.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Begin (
   -- * Functions
   mainLoop, mainLoopEvent, leaveMainLoop
) where

import Graphics.UI.GLUT.Extensions

--------------------------------------------------------------------------------

#include "HsGLUTExt.h"

--------------------------------------------------------------------------------

-- | Enter the GLUT event processing loop. This routine should be called at most
-- once in a GLUT program. Once called, this routine will never return. It will
-- call as necessary any callbacks that have been registered.

foreign import CALLCONV safe "glutMainLoop" mainLoop :: IO ()

--------------------------------------------------------------------------------

-- | (/freeglut only/) Process one iteration's worth of events in its event loop.
-- This allows the application to control its own event loop and still use the
-- GLUT package.

mainLoopEvent :: IO ()
mainLoopEvent = glutMainLoopEvent

EXTENSION_ENTRY("freeglut",glutMainLoopEvent,IO ())

--------------------------------------------------------------------------------

-- | (/freeglut only/) Stop the event loop. If the GLUT_ACTION_ON_WINDOW_CLOSE
-- option has been set to GLUT_ACTION_CONTINUE_EXECUTION, control will return
-- to the function which called 'mainLoop'; otherwise the application will exit.
--
-- If the application has two nested calls to 'mainLoop' and calls
-- 'leaveMainLoop', the behaviour is undefined. It may leave only the inner
-- nested loop or it may leave both loops. If the reader has a strong preference
-- for one behaviour over the other he should contact the freeglut Programming
-- Consortium and ask for the code to be fixed. 

leaveMainLoop :: IO ()
leaveMainLoop = glutLeaveMainLoop

EXTENSION_ENTRY("freeglut",glutLeaveMainLoop,IO ())
