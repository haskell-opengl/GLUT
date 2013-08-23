{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.QueryUtils
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module with utilities to query GLUT state.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.QueryUtils (
  Getter, simpleGet, layerGet, deviceGet
) where

import Foreign.C.Types
import Graphics.Rendering.OpenGL ( GLenum )
import Graphics.UI.GLUT.Raw

--------------------------------------------------------------------------------

type PrimGetter =                GLenum -> IO CInt
type Getter a   = (CInt -> a) -> GLenum -> IO a

makeGetter :: PrimGetter -> Getter a
makeGetter g f = fmap f . g

simpleGet, layerGet, deviceGet :: Getter a
simpleGet = makeGetter glutGet
layerGet  = makeGetter glutLayerGet
deviceGet = makeGetter glutDeviceGet
