--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Window
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- GLUT supports two types of windows: top-level windows and subwindows. Both
-- types support OpenGL rendering and GLUT callbacks. There is a single
-- identifier space for both types of windows.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Window (
   -- * Window identifiers
   Window,

   -- * Creating and destroying (sub-)windows

   -- $CreatingAndDestroyingSubWindows
   createWindow, createSubWindow, destroyWindow,
   parentWindow, numSubWindows,

   -- * Manipulating the /current window/
   currentWindow, isRealWindow,

   -- * Re-displaying and double buffer management
   postRedisplay, swapBuffers,

   -- * Changing the window geometry

   -- $ChangingTheWindowGeometry
   windowPosition, windowSize, fullScreen,

   -- * Manipulating the stacking order

   -- $ManipulatingTheStackingOrder
   pushWindow, popWindow,

   -- * Managing a window\'s display status
   WindowStatus(..), windowStatus,

   -- * Changing the window\/icon title

   -- $ChangingTheWindowIconTitle
   windowTitle, iconTitle,

   -- * Cursor management
   Cursor(..), cursor, pointerPosition
) where

import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar,
   SettableStateVar, makeSettableStateVar,
   StateVar, makeStateVar )
import Graphics.UI.GLUT.Constants (
   glut_WINDOW_PARENT, glut_WINDOW_NUM_CHILDREN,
   glut_WINDOW_X, glut_WINDOW_Y, glut_WINDOW_WIDTH, glut_WINDOW_HEIGHT,
   glut_CURSOR_RIGHT_ARROW, glut_CURSOR_LEFT_ARROW, glut_CURSOR_INFO,
   glut_CURSOR_DESTROY, glut_CURSOR_HELP, glut_CURSOR_CYCLE, glut_CURSOR_SPRAY,
   glut_CURSOR_WAIT, glut_CURSOR_TEXT, glut_CURSOR_CROSSHAIR,
   glut_CURSOR_UP_DOWN, glut_CURSOR_LEFT_RIGHT, glut_CURSOR_TOP_SIDE,
   glut_CURSOR_BOTTOM_SIDE, glut_CURSOR_LEFT_SIDE, glut_CURSOR_RIGHT_SIDE,
   glut_CURSOR_TOP_LEFT_CORNER, glut_CURSOR_TOP_RIGHT_CORNER,
   glut_CURSOR_BOTTOM_RIGHT_CORNER, glut_CURSOR_BOTTOM_LEFT_CORNER,
   glut_CURSOR_INHERIT, glut_CURSOR_NONE, glut_CURSOR_FULL_CROSSHAIR,
   glut_WINDOW_CURSOR )
import Graphics.UI.GLUT.QueryUtils ( simpleGet )
import Graphics.UI.GLUT.Types ( Window, makeWindow )

--------------------------------------------------------------------------------

-- $CreatingAndDestroyingSubWindows
-- Each created window has a unique associated OpenGL context. State changes to
-- a window\'s associated OpenGL context can be done immediately after the
-- window is created.
--
-- The /display state/ of a window is initially for the window to be shown. But
-- the window\'s /display state/ is not actually acted upon until
-- 'Graphics.UI.GLUT.Begin.mainLoop' is entered. This means until
-- 'Graphics.UI.GLUT.Begin.mainLoop' is called, rendering to a created window is
-- ineffective because the window can not yet be displayed.
--
-- The value returned by 'createWindow' and 'createSubWindow' is a unique
-- identifier for the window, which can be used when calling 'setWindow'.

-- | Create a top-level window. The given name will be provided to the window
-- system as the window\'s name. The intent is that the window system will label
-- the window with the name.Implicitly, the /current window/ is set to the newly
-- created window.
--
-- /X Implementation Notes:/ The proper X Inter-Client Communication Conventions
-- Manual (ICCCM) top-level properties are established. The @WM_COMMAND@
-- property that lists the command line used to invoke the GLUT program is only
-- established for the first window created.

createWindow
   :: String    -- ^ The window name
   -> IO Window -- ^ The identifier for the newly created window
createWindow name = withCString name glutCreateWindow

foreign import CALLCONV unsafe "glutCreateWindow" glutCreateWindow ::
      CString -> IO Window

--------------------------------------------------------------------------------

-- | Create a subwindow of the identified window with the given relative
-- position and size. Implicitly, the /current window/ is set to the
-- newly created subwindow. Subwindows can be nested arbitrarily deep.

createSubWindow
   :: Window    -- ^ Identifier of the subwindow\'s parent window.
   -> Position  -- ^ Window position in pixels relative to parent window\'s
                --   origin.
   -> Size      -- ^ Window size in pixels
   -> IO Window -- ^ The identifier for the newly created subwindow
createSubWindow win (Position x y) (Size w h) =
   glutCreateSubWindow win
                       (fromIntegral x) (fromIntegral y)
                       (fromIntegral w) (fromIntegral h)

foreign import CALLCONV unsafe "glutCreateSubWindow" glutCreateSubWindow ::
      Window -> CInt -> CInt -> CInt -> CInt -> IO Window

--------------------------------------------------------------------------------

-- | Contains the /current window\'s/ parent. If the /current window/ is a
-- top-level window, a pseudo window is returned, see 'isRealWindow'.

parentWindow :: GettableStateVar Window
parentWindow =
   makeGettableStateVar $
      simpleGet makeWindow glut_WINDOW_PARENT

--------------------------------------------------------------------------------

-- | Contains the number of subwindows the /current window/ has, not counting
-- children of children.

numSubWindows :: GettableStateVar Int
numSubWindows =
   makeGettableStateVar $
      simpleGet fromIntegral glut_WINDOW_NUM_CHILDREN

--------------------------------------------------------------------------------

-- | Destroy the specified window and the window\'s associated OpenGL context,
-- logical colormap (if the window is color index), and overlay and related
-- state (if an overlay has been established). Any subwindows of the destroyed
-- window are also destroyed by 'destroyWindow'. If the specified window was the
-- /current window/, the /current window/ becomes invalid ('getWindow' will
-- return 'Nothing').

foreign import CALLCONV unsafe "glutDestroyWindow" destroyWindow ::
   Window -> IO ()

--------------------------------------------------------------------------------

-- | Controls the /current window/. It does /not/ affect the /layer in use/ for
-- the window; this is done using 'Graphics.UI.GLUT.Overlay.layerInUse'. If no
-- windows exist or the previously /current window/ was destroyed, a pseudo
-- window is returned, see 'isRealWindow'.

currentWindow :: StateVar Window
currentWindow = makeStateVar glutGetWindow glutSetWindow

foreign import CALLCONV unsafe "glutSetWindow" glutSetWindow :: Window -> IO ()

foreign import CALLCONV unsafe "glutGetWindow" glutGetWindow :: IO Window

-- | Returns 'True' if the given window identifier refers to a real window, not
-- a pseudo one.

isRealWindow :: Window -> Bool
isRealWindow = (/= makeWindow 0)

--------------------------------------------------------------------------------

-- | Mark the normal plane of given window (or the /current window/, if none
-- is supplied) as needing to be redisplayed. The next iteration through
-- 'Graphics.UI.GLUT.Begin.mainLoop', the window\'s display callback will be
-- called to redisplay the window\'s normal plane. Multiple calls to
-- 'postRedisplay' before the next display callback opportunity generates only a
-- single redisplay callback. 'postRedisplay' may be called within a window\'s
-- display or overlay display callback to re-mark that window for redisplay.
--
-- Logically, normal plane damage notification for a window is treated as a
-- 'postRedisplay' on the damaged window. Unlike damage reported by the window
-- system, 'postRedisplay' will /not/ set to true the normal plane\'s damaged
-- status (see 'Graphics.UI.GLUT.State.damaged').
--
-- Also, see 'Graphics.UI.GLUT.Overlay.postOverlayRedisplay'.

postRedisplay :: Maybe Window -> IO ()
postRedisplay = maybe glutPostRedisplay glutPostWindowRedisplay

foreign import CALLCONV unsafe "glutPostRedisplay" glutPostRedisplay :: IO ()

-- | Mark the normal plane of the given window as needing to be redisplayed,
-- otherwise the same as 'postRedisplay'.
--
-- The advantage of this routine is that it saves the cost of a 'setWindow' call
-- (entailing an expensive OpenGL context switch), which is particularly useful
-- when multiple windows need redisplays posted at the same time. 

foreign import CALLCONV unsafe "glutPostWindowRedisplay"
   glutPostWindowRedisplay :: Window -> IO ()

--------------------------------------------------------------------------------

-- | Perform a buffer swap on the /layer in use/ for the /current window/.
-- Specifically, 'swapBuffers' promotes the contents of the back buffer of the
-- /layer in use/ of the /current window/ to become the contents of the front
-- buffer. The contents of the back buffer then become undefined. The update
-- typically takes place during the vertical retrace of the monitor, rather than
-- immediately after 'swapBuffers' is called.
--
-- An implicit 'Graphics.Rendering.OpenGL.GL.FlushFinish.flush' is done by
-- 'swapBuffers' before it returns. Subsequent OpenGL commands can be issued
-- immediately after calling 'swapBuffers', but are not executed until the
-- buffer exchange is completed.
--
-- If the /layer in use/ is not double buffered, 'swapBuffers' has no effect.

foreign import CALLCONV unsafe "glutSwapBuffers" swapBuffers :: IO ()

--------------------------------------------------------------------------------

-- $ChangingTheWindowGeometry
-- Note that the requests by 'windowPosition', 'windowSize', and 'fullScreen'
-- are not processed immediately. A request is executed after returning to the
-- main event loop. This allows multiple requests to the same window to be
-- coalesced.
--
-- 'windowPosition' and 'windowSize' requests on a window will disable the full
-- screen status of the window.

--------------------------------------------------------------------------------

-- | Controls the position of the /current window/. For top-level windows,
-- parameters of 'Position' are pixel offsets from the screen origin. For
-- subwindows, the parameters are pixel offsets from the window\'s parent window
-- origin.
--
-- In the case of top-level windows, setting 'windowPosition' is considered only
-- a request for positioning the window. The window system is free to apply its
-- own policies to top-level window placement. The intent is that top-level
-- windows should be repositioned according 'windowPosition'\'s value.

windowPosition :: StateVar Position
windowPosition = makeStateVar getWindowPosition setWindowPosition

setWindowPosition :: Position -> IO ()
setWindowPosition (Position x y) =
   glutPositionWindow (fromIntegral x) (fromIntegral y)

foreign import CALLCONV unsafe "glutPositionWindow" glutPositionWindow ::
   CInt -> CInt -> IO ()

getWindowPosition :: IO Position
getWindowPosition = do
   x <- simpleGet fromIntegral glut_WINDOW_X
   y <- simpleGet fromIntegral glut_WINDOW_Y
   return $ Position x y

--------------------------------------------------------------------------------

-- | Controls the size of the /current window/. The parameters of 'Size' are
-- size extents in pixels. The width and height must be positive values.
--
-- In the case of top-level windows, setting 'windowSize' is considered only a
-- request for sizing the window. The window system is free to apply its own
-- policies to top-level window sizing. The intent is that top-level windows
-- should be reshaped according 'windowSize'\'s value. Whether a reshape
-- actually takes effect and, if so, the reshaped dimensions are reported to the
-- program by a reshape callback.

windowSize :: StateVar Size
windowSize = makeStateVar getWindowSize setWindowSize

setWindowSize :: Size -> IO ()
setWindowSize (Size w h) =
   glutReshapeWindow (fromIntegral w) (fromIntegral h)

foreign import CALLCONV unsafe "glutReshapeWindow" glutReshapeWindow ::
   CInt -> CInt -> IO ()

getWindowSize :: IO Size
getWindowSize = do
   w <- simpleGet fromIntegral glut_WINDOW_WIDTH
   h <- simpleGet fromIntegral glut_WINDOW_HEIGHT
   return $ Size w h

--------------------------------------------------------------------------------

-- | Request that the /current window/ be made full screen. The exact semantics
-- of what full screen means may vary by window system. The intent is to make
-- the window as large as possible and disable any window decorations or borders
-- added the window system. The window width and height are not guaranteed to be
-- the same as the screen width and height, but that is the intent of making a
-- window full screen.
--
-- 'fullScreen' is defined to work only on top-level windows.
--
-- /X Implementation Notes:/ In the X implementation of GLUT, full screen is
-- implemented by sizing and positioning the window to cover the entire screen
-- and posting the @_MOTIF_WM_HINTS@ property on the window requesting
-- absolutely no decorations. Non-Motif window managers may not respond to
-- @_MOTIF_WM_HINTS@.

foreign import CALLCONV unsafe "glutFullScreen" fullScreen :: IO ()

--------------------------------------------------------------------------------

-- $ManipulatingTheStackingOrder
-- 'pushWindow' and 'popWindow' work on both top-level windows and subwindows.
-- The effect of pushing and popping windows does not take place immediately.
-- Instead the push or pop is saved for execution upon return to the GLUT event
-- loop. Subsequent pop or push requests on a window replace the previously
-- saved request for that window. The effect of pushing and popping top-level
-- windows is subject to the window system\'s policy for restacking windows.

-- | Change the stacking order of the /current window/ relative to its siblings
-- (lowering it).

foreign import CALLCONV unsafe "glutPushWindow" pushWindow :: IO ()

-- | Change the stacking order of the /current window/ relative to its siblings,
-- bringing the /current window/ closer to the top.

foreign import CALLCONV unsafe "glutPopWindow" popWindow :: IO ()

--------------------------------------------------------------------------------

-- | The display status of a window.

data WindowStatus
   = Shown
   | Hidden
   | Iconified
   deriving ( Eq, Ord, Show )

-- | Controls the display status of the /current window/.
--
-- Note that the effect of showing, hiding, and iconifying windows does not take
-- place immediately. Instead the requests are saved for execution upon return
-- to the GLUT event loop. Subsequent show, hide, or iconification requests on a
-- window replace the previously saved request for that window. The effect of
-- hiding, showing, or iconifying top-level windows is subject to the window
-- system\'s policy for displaying windows. Subwindows can\'t be iconified.

windowStatus :: SettableStateVar WindowStatus
windowStatus = makeSettableStateVar setStatus
   where setStatus Shown     = glutShowWindow
         setStatus Hidden    = glutHideWindow
         setStatus Iconified = glutIconifyWindow

foreign import CALLCONV unsafe "glutShowWindow" glutShowWindow :: IO ()

foreign import CALLCONV unsafe "glutHideWindow" glutHideWindow :: IO ()

foreign import CALLCONV unsafe "glutIconifyWindow" glutIconifyWindow :: IO ()

--------------------------------------------------------------------------------

-- $ChangingTheWindowIconTitle
-- 'windowTitle' and 'iconTitle' should be set only when the /current
-- window/ is a top-level window. Upon creation of a top-level window, the
-- window and icon names are determined by the name given to 'createWindow'.
-- Once created, setting 'windowTitle' and 'iconTitle' can change the window and
-- icon names respectively of top-level windows. Each call requests the window
-- system change the title appropriately. Requests are not buffered or
-- coalesced. The policy by which the window and icon name are displayed is
-- window system dependent.

-- | Controls the window title of the /current top-level window/.

windowTitle :: SettableStateVar String
windowTitle =
   makeSettableStateVar $ \name ->
      withCString name glutSetWindowTitle

foreign import CALLCONV unsafe "glutSetWindowTitle" glutSetWindowTitle ::
      CString -> IO ()

-- | Controls the icon title of the /current top-level window/.

iconTitle :: SettableStateVar String
iconTitle =
   makeSettableStateVar $ \name ->
      withCString name glutSetIconTitle

foreign import CALLCONV unsafe "glutSetIconTitle" glutSetIconTitle ::
      CString -> IO ()

--------------------------------------------------------------------------------

-- | The different cursor images GLUT supports.

data Cursor
   = RightArrow        -- ^ Arrow pointing up and to the right.
   | LeftArrow         -- ^ Arrow pointing up and to the left.
   | Info              -- ^ Pointing hand.
   | Destroy           -- ^ Skull & cross bones.
   | Help              -- ^ Question mark.
   | Cycle             -- ^ Arrows rotating in a circle.
   | Spray             -- ^ Spray can.
   | Wait              -- ^ Wrist watch.
   | Text              -- ^ Insertion point cursor for text.
   | Crosshair         -- ^ Simple cross-hair.
   | UpDown            -- ^ Bi-directional pointing up & down.
   | LeftRight         -- ^ Bi-directional pointing left & right.
   | TopSide           -- ^ Arrow pointing to top side.
   | BottomSide        -- ^ Arrow pointing to bottom side.
   | LeftSide          -- ^ Arrow pointing to left side.
   | RightSide         -- ^ Arrow pointing to right side.
   | TopLeftCorner     -- ^ Arrow pointing to top-left corner.
   | TopRightCorner    -- ^ Arrow pointing to top-right corner.
   | BottomRightCorner -- ^ Arrow pointing to bottom-left corner.
   | BottomLeftCorner  -- ^ Arrow pointing to bottom-right corner.
   | Inherit           -- ^ Use parent\'s cursor.
   | None              -- ^ Invisible cursor.
   | FullCrosshair     -- ^ Full-screen cross-hair cursor (if possible, otherwise 'Crosshair').
   deriving ( Eq, Ord, Show )

marshalCursor :: Cursor -> CInt
marshalCursor c = case c of
   RightArrow        -> glut_CURSOR_RIGHT_ARROW
   LeftArrow         -> glut_CURSOR_LEFT_ARROW
   Info              -> glut_CURSOR_INFO
   Destroy           -> glut_CURSOR_DESTROY
   Help              -> glut_CURSOR_HELP
   Cycle             -> glut_CURSOR_CYCLE
   Spray             -> glut_CURSOR_SPRAY
   Wait              -> glut_CURSOR_WAIT
   Text              -> glut_CURSOR_TEXT
   Crosshair         -> glut_CURSOR_CROSSHAIR
   UpDown            -> glut_CURSOR_UP_DOWN
   LeftRight         -> glut_CURSOR_LEFT_RIGHT
   TopSide           -> glut_CURSOR_TOP_SIDE
   BottomSide        -> glut_CURSOR_BOTTOM_SIDE
   LeftSide          -> glut_CURSOR_LEFT_SIDE
   RightSide         -> glut_CURSOR_RIGHT_SIDE
   TopLeftCorner     -> glut_CURSOR_TOP_LEFT_CORNER
   TopRightCorner    -> glut_CURSOR_TOP_RIGHT_CORNER
   BottomRightCorner -> glut_CURSOR_BOTTOM_RIGHT_CORNER
   BottomLeftCorner  -> glut_CURSOR_BOTTOM_LEFT_CORNER
   Inherit           -> glut_CURSOR_INHERIT
   None              -> glut_CURSOR_NONE
   FullCrosshair     -> glut_CURSOR_FULL_CROSSHAIR

unmarshalCursor :: CInt -> Cursor
unmarshalCursor c
   | c == glut_CURSOR_RIGHT_ARROW         = RightArrow
   | c == glut_CURSOR_LEFT_ARROW          = LeftArrow
   | c == glut_CURSOR_INFO                = Info
   | c == glut_CURSOR_DESTROY             = Destroy
   | c == glut_CURSOR_HELP                = Help
   | c == glut_CURSOR_CYCLE               = Cycle
   | c == glut_CURSOR_SPRAY               = Spray
   | c == glut_CURSOR_WAIT                = Wait
   | c == glut_CURSOR_TEXT                = Text
   | c == glut_CURSOR_CROSSHAIR           = Crosshair
   | c == glut_CURSOR_UP_DOWN             = UpDown
   | c == glut_CURSOR_LEFT_RIGHT          = LeftRight
   | c == glut_CURSOR_TOP_SIDE            = TopSide
   | c == glut_CURSOR_BOTTOM_SIDE         = BottomSide
   | c == glut_CURSOR_LEFT_SIDE           = LeftSide
   | c == glut_CURSOR_RIGHT_SIDE          = RightSide
   | c == glut_CURSOR_TOP_LEFT_CORNER     = TopLeftCorner
   | c == glut_CURSOR_TOP_RIGHT_CORNER    = TopRightCorner
   | c == glut_CURSOR_BOTTOM_RIGHT_CORNER = BottomRightCorner
   | c == glut_CURSOR_BOTTOM_LEFT_CORNER  = BottomLeftCorner
   | c == glut_CURSOR_INHERIT             = Inherit
   | c == glut_CURSOR_NONE                = None
   | c == glut_CURSOR_FULL_CROSSHAIR      = FullCrosshair
   | otherwise = error "unmarshalCursor"

--------------------------------------------------------------------------------

-- | Change the cursor image of the /current window/. Each call requests the
-- window system change the cursor appropriately. The cursor image when a window
-- is created is 'Inherit'. The exact cursor images used are implementation
-- dependent. The intent is for the image to convey the meaning of the cursor
-- name. For a top-level window, 'Inherit' uses the default window system
-- cursor.
--
-- /X Implementation Notes:/ GLUT for X uses SGI\'s @_SGI_CROSSHAIR_CURSOR@
-- convention to access a full-screen cross-hair cursor if possible.

cursor :: StateVar Cursor
cursor = makeStateVar getCursor setCursor

setCursor :: Cursor -> IO ()
setCursor = glutSetCursor . marshalCursor

foreign import CALLCONV unsafe "glutSetCursor" glutSetCursor :: CInt -> IO ()

getCursor :: IO Cursor
getCursor = simpleGet unmarshalCursor glut_WINDOW_CURSOR

--------------------------------------------------------------------------------

-- | Setting 'pointerPosition' warps the window system\'s pointer to a new
-- location relative to the origin of the /current window/ by the specified
-- pixel offset, which may be negative. The warp is done immediately.
--
-- If the pointer would be warped outside the screen\'s frame buffer region, the
-- location will be clamped to the nearest screen edge. The window system is
-- allowed to further constrain the pointer\'s location in window system
-- dependent ways.
--
-- Good advice from Xlib\'s @XWarpPointer@ man page: \"There is seldom any
-- reason for calling this function. The pointer should normally be left to the
-- user.\"

pointerPosition :: SettableStateVar Position
pointerPosition =
   makeSettableStateVar $ \(Position x y) ->
      glutWarpPointer (fromIntegral x) (fromIntegral y)

foreign import CALLCONV unsafe "glutWarpPointer" glutWarpPointer ::
   CInt -> CInt -> IO ()
