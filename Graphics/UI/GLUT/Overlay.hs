-- #prune
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Overlay
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- When  overlay hardware is available, GLUT provides a set of routines for
-- establishing, using, and removing an overlay for GLUT windows. When an
-- overlay is established, a separate OpenGL context is also established. A
-- window\'s overlay OpenGL state is kept distinct from the normal planes OpenGL
-- state.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Overlay (
   -- * Overlay creation and destruction
   establishOverlay, removeOverlay,

   -- * Changing the /layer in use/
   Layer(..), useLayer,
   unmarshalLayer,   -- used only internally

   -- * Re-displaying
   postOverlayRedisplay, postWindowOverlayRedisplay,

   -- * Showing and hiding an overlay

   -- $ShowingAndHidingAnOverlay
   showOverlay, hideOverlay
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.UI.GLUT.Constants
import Graphics.UI.GLUT.Window ( Window )

--------------------------------------------------------------------------------

-- | Establish an overlay (if possible) for the /current window/. The requested
-- display mode for the overlay is determined by the /initial display mode/.
-- @'layerGet' 'OverlayPossible'@ can be called to determine if an overlay is
-- possible for the /current window/ with the current /initial display mode/. Do
-- not attempt to establish an overlay when one is not possible; GLUT will
-- terminate the program.
--
-- If 'establishOverlay' is called when an overlay already exists, the existing
-- overlay is first removed, and then a new overlay is established. The state of
-- the old overlay\'s OpenGL context is discarded.
--
-- The initial display state of an overlay is shown, however the overlay is only
-- actually shown if the overlay\'s window is shown.
--
-- Implicitly, the window\'s /layer in use/ changes to the overlay immediately
-- after the overlay is established.
--
-- /X Implementation Notes:/ GLUT for X uses the @SERVER_OVERLAY_VISUALS@
-- convention to determine if overlay visuals are available. While the
-- convention allows for opaque overlays (no transparency) and overlays with the
-- transparency specified as a bitmask, GLUT overlay management only provides
-- access to transparent pixel overlays.
--
-- Until RGBA overlays are better understood, GLUT only supports color index
-- overlays.

foreign import ccall safe "glutEstablishOverlay" establishOverlay :: IO ()

-- | Remove the overlay (if one exists). It is safe to call 'removeOverlay' even
-- if no overlay is currently established, it does nothing in this case.
-- Implicitly, the window\'s /layer in use/ changes to the normal plane
-- immediately once the overlay is removed.
--
-- If the program intends to re-establish the overlay later, it is typically
-- faster and less resource intensive to use 'hideOverlay' and 'showOverlay' to
-- simply change the display status of the overlay.

foreign import ccall safe "glutRemoveOverlay" removeOverlay :: IO ()

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

-- | Change the per-window /layer in use/ for the /current window/, selecting
-- either the normal plane or overlay. The overlay should only be specified if
-- an overlay exists, however windows without an overlay may still call
-- @'useLayer' 'Normal'@. OpenGL commands for the window are directed to the
-- current /layer in use/.
--
-- To query the /layer in use/ for a window, call @'layerGet' 'LayerInUse'@.

useLayer :: Layer -> IO ()
useLayer = glutUseLayer . marshalLayer

foreign import ccall safe "glutUseLayer" glutUseLayer :: GLenum -> IO ()

--------------------------------------------------------------------------------

-- | Mark the overlay of the /current window/ as needing to be redisplayed. The
-- next iteration through 'Graphics.UI.GLUT.Begin.mainLoop', the window\'s
-- overlay display callback (or simply the display callback if no overlay
-- display callback is registered) will be called to redisplay the window\'s
-- overlay plane. Multiple calls to 'postOverlayRedisplay' before the next
-- display callback opportunity (or overlay display callback opportunity if one
-- is registered) generate only a single redisplay. 'postOverlayRedisplay' may
-- be called within a window\'s display or overlay display callback to re-mark
-- that window for redisplay.
--
-- Logically, overlay damage notification for a window is treated as a
-- 'postOverlayRedisplay' on the damaged window. Unlike damage reported by the
-- window system, 'postOverlayRedisplay' will not set to true the overlay\'s
-- damaged status (returned by @'layerGet' 'OverlayDamaged'@).
--
-- Also, see 'Graphics.UI.GLUT.Window.postRedisplay'.

foreign import ccall safe "glutPostOverlayRedisplay" postOverlayRedisplay ::
   IO ()

-- | Mark the overlay of the given window as needing to be redisplayed,
-- otherwise the same as 'postOverlayRedisplay'.
--
-- If the window you want to post an overlay redisplay on is not already the
-- /current window/ (and you do not require it to be immediately made current),
-- using 'postWindowOverlayRedisplay' is more efficient than calling
-- 'Graphics.UI.GLUT.Window.setWindow' to the desired window and then calling
-- 'postOverlayRedisplay'.		
--
-- Also, see 'Graphics.UI.GLUT.Window.postWindowRedisplay'.

foreign import ccall safe "glutPostWindowOverlayRedisplay"
   postWindowOverlayRedisplay :: Window -> IO ()

--------------------------------------------------------------------------------

-- $ShowingAndHidingAnOverlay
-- The effect of showing or hiding an overlay takes place immediately. Note that
-- 'showOverlay' will not actually display the overlay unless the window is also
-- shown (and even a shown window may be obscured by other windows, thereby
-- obscuring the overlay). It is typically faster and less resource intensive to
-- use the routines below to control the display status of an overlay as opposed
-- to removing and re-establishing the overlay.

-- | Show the overlay of the /current window/.

foreign import ccall safe "glutShowOverlay" showOverlay :: IO ()

-- | Hide the overlay of the /current window/.

foreign import ccall safe "glutHideOverlay" hideOverlay :: IO ()
