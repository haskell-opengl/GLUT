--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Overlay
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- When  overlay hardware is available, GLUT provides a set of routines for
-- establishing, using, and removing an overlay for GLUT windows. When an
-- overlay is established, a separate OpenGL context is also established. A
-- window\'s overlay OpenGL state is kept distinct from the normal planes\'
-- OpenGL state.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Overlay (
   -- * Overlay creation and destruction
   hasOverlay, overlayPossible,

   -- * Showing and hiding an overlay
   overlayVisible,

   -- * Changing the /layer in use/
   Layer(..), layerInUse,

   -- * Re-displaying
   postOverlayRedisplay
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar,
   SettableStateVar, makeSettableStateVar,
   StateVar, makeStateVar )
import Graphics.UI.GLUT.Constants (
   glut_OVERLAY_POSSIBLE, glut_HAS_OVERLAY, glut_NORMAL, glut_OVERLAY,
   glut_LAYER_IN_USE )
import Graphics.UI.GLUT.QueryUtils ( layerGet )
import Graphics.UI.GLUT.Window ( Window )

--------------------------------------------------------------------------------

-- | Controls the overlay for the /current window/. The requested display mode
-- for the overlay is determined by the /initial display mode/.
-- 'overlayPossible' can be used to determine if an overlay is possible for the
-- /current window/ with the current /initial display mode/. Do not attempt to
-- establish an overlay when one is not possible; GLUT will terminate the
-- program.
--
-- When 'hasOverlay' is set to 'True' when an overlay already exists, the
-- existing overlay is first removed, and then a new overlay is established. The
-- state of the old overlay\'s OpenGL context is discarded. Implicitly, the
-- window\'s /layer in use/ changes to the overlay immediately after the overlay
-- is established.
--
-- The initial display state of an overlay is shown, however the overlay is only
-- actually shown if the overlay\'s window is shown.
--
-- Setting 'hasOverlay' to 'False' is safe even if no overlay is currently
-- established, nothing happens in this case. Implicitly, the window\'s /layer
-- in use/ changes to the normal plane immediately once the overlay is removed.
-- 
-- If the program intends to re-establish the overlay later, it is typically
-- faster and less resource intensive to use 'overlayVisible' to simply change
-- the display status of the overlay.
--
-- /X Implementation Notes:/ GLUT for X uses the @SERVER_OVERLAY_VISUALS@
-- convention to determine if overlay visuals are available. While the
-- convention allows for opaque overlays (no transparency) and overlays with the
-- transparency specified as a bitmask, GLUT overlay management only provides
-- access to transparent pixel overlays.
-- 
-- Until RGBA overlays are better understood, GLUT only supports color index
-- overlays.

hasOverlay :: StateVar Bool
hasOverlay = makeStateVar getHasOverlay setHasOverlay

setHasOverlay :: Bool -> IO ()
setHasOverlay False = glutRemoveOverlay
setHasOverlay True  = glutEstablishOverlay

foreign import CALLCONV safe "glutRemoveOverlay" glutRemoveOverlay :: IO ()

foreign import CALLCONV safe "glutEstablishOverlay" glutEstablishOverlay :: IO ()

getHasOverlay :: IO Bool
getHasOverlay = layerGet (/= 0) glut_HAS_OVERLAY

--------------------------------------------------------------------------------

-- | Contains 'True' if an overlay could be established for the /current window/
-- given the current /initial display mode/. If it contains 'False',
-- 'establishOverlay' will fail with a fatal error if called.

overlayPossible :: GettableStateVar Bool
overlayPossible = makeGettableStateVar $ layerGet (/= 0) glut_OVERLAY_POSSIBLE

--------------------------------------------------------------------------------

-- | Controls the visibility of the overlay of the /current window/.
--
-- The effect of showing or hiding an overlay takes place immediately. Note that
-- 'showOverlay' will not actually display the overlay unless the window is also
-- shown (and even a shown window may be obscured by other windows, thereby
-- obscuring the overlay). It is typically faster and less resource intensive to
-- use the routines below to control the display status of an overlay as opposed
-- to removing and re-establishing the overlay.


overlayVisible :: SettableStateVar Bool
overlayVisible =
   makeSettableStateVar $ \flag ->
      if flag then glutShowOverlay else glutHideOverlay

foreign import CALLCONV safe "glutShowOverlay" glutShowOverlay :: IO ()

foreign import CALLCONV safe "glutHideOverlay" glutHideOverlay :: IO ()

--------------------------------------------------------------------------------

-- | The /layer in use/.
data Layer
   = Normal   -- ^ The normal plane.
   | Overlay  -- ^ The overlay.
   deriving ( Eq, Ord )

marshalLayer :: Layer -> GLenum
marshalLayer l = case l of
   Normal  -> glut_NORMAL
   Overlay -> glut_OVERLAY

unmarshalLayer :: GLenum -> Layer
unmarshalLayer l
   | l == glut_NORMAL  = Normal
   | l == glut_OVERLAY = Overlay
   | otherwise = error "unmarshalLayer"

--------------------------------------------------------------------------------

-- | Controls the per-window /layer in use/ for the /current window/, which can
-- either be the normal plane or the overlay. Selecting the overlay should only
-- be done if an overlay exists, however windows without an overlay may still
-- set the /layer in use/ to 'Normal'. OpenGL commands for the window are
-- directed to the current /layer in use/.

layerInUse :: StateVar Layer
layerInUse =
   makeStateVar getLayerInUse setLayerInUse

setLayerInUse :: Layer -> IO ()
setLayerInUse = glutUseLayer . marshalLayer

foreign import CALLCONV safe "glutUseLayer" glutUseLayer :: GLenum -> IO ()

getLayerInUse :: IO Layer
getLayerInUse = layerGet (unmarshalLayer . fromIntegral) glut_LAYER_IN_USE

--------------------------------------------------------------------------------

-- | Mark the overlay of the given window (or the /current window/, if none is
-- supplied) as needing to be redisplayed. The next iteration through
-- 'Graphics.UI.GLUT.Begin.mainLoop', the window\'s overlay display callback
-- (or simply the display callback if no overlay display callback is registered)
-- will be called to redisplay the window\'s overlay plane. Multiple calls to
-- 'postOverlayRedisplay' before the next display callback opportunity (or
-- overlay display callback opportunity if one is registered) generate only a
-- single redisplay. 'postOverlayRedisplay' may be called within a window\'s
-- display or overlay display callback to re-mark that window for redisplay.
--
-- Logically, overlay damage notification for a window is treated as a
-- 'postOverlayRedisplay' on the damaged window. Unlike damage reported by the
-- window system, 'postOverlayRedisplay' will not set to true the overlay\'s
-- damaged status (see 'Graphics.UI.GLUT.State.damaged').
--
-- Also, see 'Graphics.UI.GLUT.Window.postRedisplay'.

postOverlayRedisplay :: Maybe Window -> IO ()
postOverlayRedisplay =
   maybe glutPostOverlayRedisplay glutPostWindowOverlayRedisplay

foreign import CALLCONV safe "glutPostOverlayRedisplay"
   glutPostOverlayRedisplay :: IO ()

foreign import CALLCONV safe "glutPostWindowOverlayRedisplay"
   glutPostWindowOverlayRedisplay :: Window -> IO ()
