--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Debugging
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module contains a simple utility routine to report any pending GL
-- errors.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Debugging (
   reportErrors
) where

-- | Report any pending GL errors to stderr (which is typically the console).
-- If there are no pending errors, this routine does nothing. Note that the
-- error flags are reset after this action, i.e. there are no pending errors
-- left afterwards.

foreign import CALLCONV unsafe "glutReportErrors" reportErrors :: IO ()
