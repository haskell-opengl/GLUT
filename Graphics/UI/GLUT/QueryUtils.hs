-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.QueryUtils
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module with utilities to query GLUT state.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.QueryUtils (
  Getter, simpleGet, layerGet, deviceGet
) where

import Control.Monad ( liftM )
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

--------------------------------------------------------------------------------

type PrimGetter =                GLenum -> IO CInt
type Getter a   = (CInt -> a) -> GLenum -> IO a

makeGetter :: PrimGetter -> Getter a
makeGetter g f = liftM f . g

simpleGet, layerGet, deviceGet :: Getter a
simpleGet = makeGetter glutGet
layerGet  = makeGetter glutLayerGet
deviceGet = makeGetter glutDeviceGet

foreign import CALLCONV unsafe "glutGet"       glutGet       :: PrimGetter
foreign import CALLCONV unsafe "glutLayerGet"  glutLayerGet  :: PrimGetter
foreign import CALLCONV unsafe "glutDeviceGet" glutDeviceGet :: PrimGetter
