-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Colormap
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- OpenGL supports both RGBA and color index rendering. The RGBA mode is
-- generally preferable to color index because more OpenGL rendering
-- capabilities are available and color index mode requires the loading
-- of colormap entries.
--
-- The GLUT color index routines are used to write and read entries in a
-- window\'s color index colormap. Every GLUT color index window has its
-- own logical color index colormap. The size of a window\'s colormap can
-- be determined by calling 'Graphics.UI.GLUT.State.getNumColormapEntries'.
--
-- GLUT color index windows within a program can attempt to share
-- colormap resources by copying a single color index colormap to
-- multiple windows using 'copyColormap'. If possible GLUT will attempt
-- to share the actual colormap. While copying colormaps using
-- 'copyColormap' can potentially allow sharing of physical colormap
-- resources, logically each window has its own colormap. So changing a
-- copied colormap of a window will force the duplication of the
-- colormap. For this reason, color index programs should generally load
-- a single color index colormap, copy it to all color index windows
-- within the program, and then not modify any colormap cells.
--
-- Use of multiple colormaps is likely to result in colormap installation
-- problems where some windows are displayed with an incorrect colormap
-- due to limitations on colormap resources.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.Colormap (
   -- * Functions
   setColor,
   getColor,
   copyColormap
) where

import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLfloat )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color3(..), ColorIndex )
import Graphics.UI.GLUT.Constants
import Graphics.UI.GLUT.Window ( Window )

-- | Set the cell color index colormap entry of the /current window/\'s
-- logical colormap for the /layer in use/ with the specified color. The
-- /layer in use/ of the /current window/ should be a color index window.
-- The color index should be zero or greater and less than the total
-- number of colormap entries for the window. If the /layer in use/\'s
-- colormap was copied by reference, a 'setColor' call will force the
-- duplication of the colormap. Do not attempt to set the color of an
-- overlay\'s transparent index.
setColor :: ColorIndex CInt
         -> Color3 GLfloat
         -> IO ()
setColor cell (Color3 r g b) = glutSetColor cell r g b

foreign import ccall unsafe "glutSetColor" glutSetColor ::
   ColorIndex CInt -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | Retrieve the entry for a given color index colormap entry for the
-- /current window/\'s logical colormap. The /current window/ should be a
-- color index window. The index should be zero or greater and less than
-- the total number of colormap entries for the window. For valid color
-- indices, the value returned is 'Just' a color with floating point component
-- values between 0.0 and 1.0 inclusive. 'getColor' will return 'Nothing'
-- if the color index specified is an overlay\'s transparent index, less
-- than zero, or greater or equal to the value returned by @'get'
-- 'WindowColormapSize'@, that is if the color index is transparent or
-- outside the valid range of color indices.
getColor :: ColorIndex CInt
         -> IO (Maybe (Color3 GLfloat))
getColor cell = do
   r <- glutGetColor cell glut_RED
   g <- glutGetColor cell glut_GREEN
   b <- glutGetColor cell glut_BLUE
   return $ if r < 0.0 then Nothing else Just (Color3 r g b)

foreign import ccall unsafe "glutGetColor" glutGetColor ::
   ColorIndex CInt -> CInt -> IO GLfloat

-- | Copy (lazily if possible to promote sharing) the logical colormap
-- from a specified window to the /current window/\'s /layer in use/.
-- The copy will be from the normal plane to the normal plane; or from
-- the overlay to the overlay (never across different layers). Once a
-- colormap has been copied, avoid setting cells in the colormap with
-- 'setColor' since that will force an actual copy of the colormap if it
-- was previously copied by reference. 'copyColormap' should only be
-- called when both the /current window/ and the specified window are
-- color index windows.
foreign import ccall unsafe "glutCopyColormap" copyColormap ::
      Window -> IO ()
