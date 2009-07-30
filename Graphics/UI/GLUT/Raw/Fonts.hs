{-# LANGUAGE ForeignFunctionInterface #-}
-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Raw.Fonts
-- Copyright   :  (c) Sven Panne 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- Our own functions to access font identifiers in a portable way.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.Raw.Fonts (
   hs_GLUT_marshalBitmapFont,
   hs_GLUT_marshalStrokeFont
) where

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall unsafe "hs_GLUT_marshalBitmapFont"
   hs_GLUT_marshalBitmapFont :: CInt -> IO (Ptr a)

foreign import ccall unsafe "hs_GLUT_marshalStrokeFont"
   hs_GLUT_marshalStrokeFont :: CInt -> IO (Ptr a)
