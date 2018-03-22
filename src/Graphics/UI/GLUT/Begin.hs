--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Begin
-- Copyright   :  (c) Sven Panne 2002-2018
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.StateVar ( StateVar, makeStateVar )
import Foreign.C.Types ( CInt )

import Graphics.UI.GLUT.QueryUtils
import Graphics.UI.GLUT.Raw

--------------------------------------------------------------------------------

-- | Enter the GLUT event processing loop; it will call as necessary any
-- callbacks that have been registered. This routine should be called at most
-- once in a GLUT program.

mainLoop :: MonadIO m => m ()
mainLoop = glutMainLoop

--------------------------------------------------------------------------------

-- | (/freeglut only/) Process one iteration's worth of events in its event loop.
-- This allows the application to control its own event loop and still use the
-- GLUT package.

mainLoopEvent :: MonadIO m => m ()
mainLoopEvent = glutMainLoopEvent

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

leaveMainLoop :: MonadIO m => m ()
leaveMainLoop = glutLeaveMainLoop

--------------------------------------------------------------------------------

-- | The behaviour when the user closes a window.

data ActionOnWindowClose
   = -- | Exit the whole program when any window is closed or 'leaveMainLoop'
     -- is called (default).
     Exit
   | -- | Return from mainLoop when any window is closed.
     MainLoopReturns
   | -- | Return from mainLoop after the last window is closed.
     ContinueExecution
   deriving ( Eq, Ord, Show )

marshalActionOnWindowClose :: ActionOnWindowClose -> CInt
marshalActionOnWindowClose x = case x of
   Exit ->  glut_ACTION_EXIT
   MainLoopReturns -> glut_ACTION_GLUTMAINLOOP_RETURNS
   ContinueExecution -> glut_ACTION_CONTINUE_EXECUTION

unmarshalActionOnWindowClose :: CInt -> ActionOnWindowClose
unmarshalActionOnWindowClose x
   | x == glut_ACTION_EXIT = Exit
   | x == glut_ACTION_GLUTMAINLOOP_RETURNS = MainLoopReturns
   | x == glut_ACTION_CONTINUE_EXECUTION = ContinueExecution
   | otherwise = error ("unmarshalActionOnWindowClose: illegal value " ++ show x)

-----------------------------------------------------------------------------

-- | (/freeglut only/) Controls the behaviour when the user closes a window.

actionOnWindowClose :: StateVar ActionOnWindowClose
actionOnWindowClose =
   makeStateVar
      (simpleGet unmarshalActionOnWindowClose glut_ACTION_ON_WINDOW_CLOSE)
      (glutSetOption glut_ACTION_ON_WINDOW_CLOSE . marshalActionOnWindowClose)
