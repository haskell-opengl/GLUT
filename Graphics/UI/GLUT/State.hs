--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.State
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- GLUT maintains a considerable amount of programmer visible state. Some (but
-- not all) of this state may be directly retrieved.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.State (
  -- * GLUT state pertaining to the layers of the  current window
  isOverlayPossible, getLayerInUse, isOverlayEstablished, getTransparentIndex,
  isLayerDamaged
) where

import Control.Monad (liftM)
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( ColorIndex(..) )
import Graphics.UI.GLUT.Constants
import Graphics.UI.GLUT.Overlay ( Layer(..), unmarshalLayer )

--------------------------------------------------------------------------------

isOverlayPossible :: IO Bool
isOverlayPossible = liftM i2b (layerGet glut_OVERLAY_POSSIBLE)

getLayerInUse :: IO Layer
getLayerInUse =
   liftM (unmarshalLayer . fromIntegral) (layerGet glut_LAYER_IN_USE)

isOverlayEstablished :: IO Bool
isOverlayEstablished = liftM i2b (layerGet glut_HAS_OVERLAY)

getTransparentIndex :: IO (Maybe (ColorIndex CInt))
getTransparentIndex = liftM i2c (layerGet glut_TRANSPARENT_INDEX)
   where i2c i = if i < 0 then Nothing else Just (ColorIndex i)

isLayerDamaged :: Layer -> IO Bool
isLayerDamaged Normal  = liftM i2b (layerGet glut_NORMAL_DAMAGED)
isLayerDamaged Overlay = liftM i2b (layerGet glut_OVERLAY_DAMAGED)

foreign import ccall unsafe "glutLayerGet" layerGet :: GLenum -> IO CInt

--------------------------------------------------------------------------------

i2b :: CInt -> Bool
i2b i = i /= 0
