--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Debugging
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module contains a simple utility routine to report any pending GL
-- errors.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Debugging (
   reportErrors
) where

import Graphics.Rendering.OpenGL ( Error(..), errors, get )
import System.Environment
import System.IO

--------------------------------------------------------------------------------

-- | Report any pending GL errors to stderr (which is typically the console).
-- If there are no pending errors, this routine does nothing. Note that the
-- error flags are reset after this action, i.e. there are no pending errors
-- left afterwards.

reportErrors :: IO ()
reportErrors = get errors >>= mapM_ reportError

reportError :: Error -> IO ()
reportError (Error _ msg) = do
   pn <- getProgName
   hPutStrLn stderr ("GLUT: Warning in " ++ pn ++ ": GL error: " ++ msg)
