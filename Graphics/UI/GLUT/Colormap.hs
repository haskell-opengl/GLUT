--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Colormap
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- OpenGL supports both RGBA and color index rendering. The RGBA mode is
-- generally preferable to color index because more OpenGL rendering
-- capabilities are available and color index mode requires the loading of
-- colormap entries.
--
-- The GLUT color index state variables are used to read and write entries in a
-- window\'s color index colormap. Every GLUT color index window has its own
-- logical color index colormap. The size of a window\'s colormap can be
-- determined by reading 'numColorMapEntries'.
--
-- GLUT color index windows within a program can attempt to share colormap
-- resources by copying a single color index colormap to multiple windows using
-- 'copyColormap'. If possible GLUT will attempt to share the actual colormap.
-- While copying colormaps using 'copyColormap' can potentially allow sharing of
-- physical colormap resources, logically each window has its own colormap. So
-- changing a copied colormap of a window will force the duplication of the
-- colormap. For this reason, color index programs should generally load a
-- single color index colormap, copy it to all color index windows within the
-- program, and then not modify any colormap cells.
--
-- Use of multiple colormaps is likely to result in colormap installation
-- problems where some windows are displayed with an incorrect colormap due to
-- limitations on colormap resources.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Colormap (
   colorMapEntry,
   copyColormap,
   numColorMapEntries,
   transparentIndex
) where

import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLfloat )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color3(..), Index1(..) )
import Graphics.UI.GLUT.Constants (
   glut_RED, glut_GREEN, glut_BLUE,
   glut_WINDOW_COLORMAP_SIZE, glut_TRANSPARENT_INDEX )
import Graphics.UI.GLUT.QueryUtils ( simpleGet, layerGet )
import Graphics.UI.GLUT.Window ( Window )

--------------------------------------------------------------------------------

-- | Controls the color index colormap entry of the /current window/\'s logical
-- colormap for the /layer in use/. The /layer in use/ of the /current window/
-- should be a color index window. The color index should be zero or greater and
-- less than the total number of colormap entries for the window (see
-- 'numColorMapEntries') and different from an overlay\'s transparent index (see
-- 'transparentIndex').
--
-- If the /layer in use/\'s colormap was copied by reference, setting a colormap
-- entry will force the duplication of the colormap.

colorMapEntry :: Index1 GLint -> StateVar (Color3 GLfloat)
colorMapEntry (Index1 cell) =
   makeStateVar (getColorMapEntry (fromIntegral cell))
                (setColorMapEntry (fromIntegral cell))

setColorMapEntry :: CInt -> Color3 GLfloat -> IO ()
setColorMapEntry cell (Color3 r g b) = glutSetColor cell r g b

foreign import CALLCONV unsafe "glutSetColor" glutSetColor ::
   CInt -> GLfloat -> GLfloat -> GLfloat -> IO ()

getColorMapEntry :: CInt -> IO (Color3 GLfloat)
getColorMapEntry cell = do
   r <- glutGetColor cell glut_RED
   g <- glutGetColor cell glut_GREEN
   b <- glutGetColor cell glut_BLUE
   return $ Color3 r g b

foreign import CALLCONV unsafe "glutGetColor" glutGetColor ::
   CInt -> CInt -> IO GLfloat

--------------------------------------------------------------------------------

-- | Copy (lazily if possible to promote sharing) the logical colormap from a
-- specified window to the /current window/\'s /layer in use/. The copy will be
-- from the normal plane to the normal plane; or from the overlay to the overlay
-- (never across different layers). Once a colormap has been copied, avoid
-- setting cells in the colormap via 'coloMapEntry' since that will force an
-- actual copy of the colormap if it was previously copied by reference.
-- 'copyColormap' should only be called when both the /current window/ and the
-- specified window are color index windows.

foreign import CALLCONV unsafe "glutCopyColormap" copyColormap ::
      Window -> IO ()

--------------------------------------------------------------------------------

-- | Contains the number of entries in the colormap of the /current window/\'s
-- current layer (0 in RGBA mode).

numColorMapEntries :: GettableStateVar GLint
numColorMapEntries =
   makeGettableStateVar $ simpleGet fromIntegral glut_WINDOW_COLORMAP_SIZE

--------------------------------------------------------------------------------

-- | Contains the transparent color index of the overlay of the /current window/
-- or -1 if no overlay is in use.

transparentIndex :: GettableStateVar (Index1 GLint)
transparentIndex =
   makeGettableStateVar $
      layerGet (Index1 . fromIntegral) glut_TRANSPARENT_INDEX
