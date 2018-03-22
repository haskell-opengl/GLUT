--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Debugging
-- Copyright   :  (c) Sven Panne 2002-2018
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

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.StateVar ( get )
import Graphics.Rendering.OpenGL ( Error(..), errors )
import System.Environment ( getProgName )
import System.IO ( hPutStrLn, stderr )

--------------------------------------------------------------------------------

-- | Report any pending GL errors to stderr (which is typically the console).
-- If there are no pending errors, this routine does nothing. Note that the
-- error flags are reset after this action, i.e. there are no pending errors
-- left afterwards.

reportErrors :: MonadIO m => m ()
reportErrors = get errors >>= mapM_ reportError

reportError :: MonadIO m => Error -> m ()
reportError (Error _ msg) = liftIO $ do
   pn <- getProgName
   hPutStrLn stderr ("GLUT: Warning in " ++ pn ++ ": GL error: " ++ msg)
