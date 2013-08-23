{-# LANGUAGE ForeignFunctionInterface #-}
-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Raw.Fonts
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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
