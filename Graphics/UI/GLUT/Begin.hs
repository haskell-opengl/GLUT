--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Begin
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- After a GLUT program has done initial setup such as creating windows and
-- menus, GLUT programs enter the GLUT event processing loop by calling
-- 'mainLoop'.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Begin (
   -- * Functions
   mainLoop
) where

-- | Enter the GLUT event processing loop. This routine should be called at most
-- once in a GLUT program. Once called, this routine will never return. It will
-- call as necessary any callbacks that have been registered.

foreign import CALLCONV safe "glutMainLoop" mainLoop :: IO ()
