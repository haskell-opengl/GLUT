-- #prune
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
  -- * Type synonyms
  SubWindowCount, SampleCount, BufferDepth, ButtonCount, ButtonIndex, DialCount,
  DialIndex, AxisCount, PollRate,

  -- * State of the /current window/
  getWindowPosition, getWindowSize,
  getParent, getSubWindowCount,
  getCursor,
  isRGBA, getRGBABufferDepths,
  getColorBufferDepth, getColormapEntryCount,
  isDoubleBuffered, isStereo,
  getAccumBufferDepths, getDepthBufferDepth, getStencilBufferDepth,
  getSampleCount, getFormatID,

  -- * Miscellaneous
  getElapsedTime, getMenuItemCount,

  -- * GLUT state pertaining to the layers of the /current window/
  isOverlayPossible, getLayerInUse, isOverlayEstablished, getTransparentIndex,
  isNormalDamaged, isOverlayDamaged,

  -- * Device information

  -- $DeviceInformation

  -- ** Screen information
  ScreenInfo(..), getScreenInfo,

  -- ** Mouse information
  MouseInfo(..), getMouseInfo,

  -- ** Spaceball information
  SpaceballInfo(..), getSpaceballInfo,

  -- ** Dial and button box information
  DialAndButtonBoxInfo(..), getDialAndButtonBoxInfo,

  -- ** Tablet information
  TabletInfo(..), getTabletInfo,

  -- ** Joystick information
  JoystickInfo(..), getJoystickInfo
) where

import Control.Monad ( liftM )
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Index1(..) )
import Graphics.UI.GLUT.Constants (
   glut_WINDOW_X, glut_WINDOW_Y, glut_WINDOW_WIDTH, glut_WINDOW_HEIGHT,
   glut_WINDOW_PARENT, glut_WINDOW_NUM_CHILDREN,
   glut_WINDOW_CURSOR, glut_WINDOW_RGBA,
   glut_WINDOW_RED_SIZE, glut_WINDOW_GREEN_SIZE, glut_WINDOW_BLUE_SIZE,
   glut_WINDOW_ALPHA_SIZE,
   glut_WINDOW_BUFFER_SIZE, glut_WINDOW_COLORMAP_SIZE,
   glut_WINDOW_DOUBLEBUFFER, glut_WINDOW_STEREO,
   glut_WINDOW_ACCUM_RED_SIZE, glut_WINDOW_ACCUM_GREEN_SIZE,
   glut_WINDOW_ACCUM_BLUE_SIZE, glut_WINDOW_ACCUM_ALPHA_SIZE,
   glut_WINDOW_DEPTH_SIZE, glut_WINDOW_STENCIL_SIZE, glut_WINDOW_NUM_SAMPLES,
   glut_WINDOW_FORMAT_ID, glut_ELAPSED_TIME, glut_MENU_NUM_ITEMS,
   glut_OVERLAY_POSSIBLE, glut_LAYER_IN_USE, glut_HAS_OVERLAY,
   glut_TRANSPARENT_INDEX,
   glut_NORMAL_DAMAGED, glut_OVERLAY_DAMAGED,
   glut_SCREEN_WIDTH, glut_SCREEN_HEIGHT,
   glut_SCREEN_WIDTH_MM, glut_SCREEN_HEIGHT_MM,
   glut_HAS_MOUSE, glut_NUM_MOUSE_BUTTONS,
   glut_HAS_SPACEBALL, glut_NUM_SPACEBALL_BUTTONS,
   glut_HAS_DIAL_AND_BUTTON_BOX, glut_NUM_DIALS, glut_NUM_BUTTON_BOX_BUTTONS,
   glut_HAS_TABLET, glut_NUM_TABLET_BUTTONS,
   glut_HAS_JOYSTICK, glut_JOYSTICK_BUTTONS, glut_JOYSTICK_POLL_RATE,
   glut_JOYSTICK_AXES,
   glut_CURSOR_RIGHT_ARROW, glut_CURSOR_LEFT_ARROW, glut_CURSOR_INFO,
   glut_CURSOR_DESTROY, glut_CURSOR_HELP, glut_CURSOR_CYCLE, glut_CURSOR_SPRAY,
   glut_CURSOR_WAIT, glut_CURSOR_TEXT, glut_CURSOR_CROSSHAIR,
   glut_CURSOR_UP_DOWN, glut_CURSOR_LEFT_RIGHT, glut_CURSOR_TOP_SIDE,
   glut_CURSOR_BOTTOM_SIDE, glut_CURSOR_LEFT_SIDE, glut_CURSOR_RIGHT_SIDE,
   glut_CURSOR_TOP_LEFT_CORNER, glut_CURSOR_TOP_RIGHT_CORNER,
   glut_CURSOR_BOTTOM_RIGHT_CORNER, glut_CURSOR_BOTTOM_LEFT_CORNER,
   glut_CURSOR_INHERIT, glut_CURSOR_NONE, glut_CURSOR_FULL_CROSSHAIR,
   glut_NORMAL, glut_OVERLAY )
import Graphics.UI.GLUT.Overlay ( Layer(..) )
import Graphics.UI.GLUT.QueryUtils ( simpleGet, layerGet, deviceGet )
import Graphics.UI.GLUT.Window ( Window, makeWindow, Cursor(..) )

--------------------------------------------------------------------------------

-- | Number of children of a window

type SubWindowCount = CInt

-- | Number of samples for multisampling

type SampleCount = CInt

-- | Bit depth of a buffer

type BufferDepth = CInt

-- | Number of buttons of an input device

type ButtonCount = CInt

-- | The index of a specific buttons of an input device

type ButtonIndex = CInt

-- | Number of dials of a dial and button box

type DialCount = CInt

-- | The index of a specific dial of a dial and button box

type DialIndex = CInt

-- | Number of axes of a joystick

type AxisCount  = CInt

-- | The a rate at which a joystick is polled (in milliseconds)

type PollRate = CInt

--------------------------------------------------------------------------------

-- | Return the location of the /current window,/ measured in pixels relative to
-- the screen origin.

getWindowPosition :: IO Position
getWindowPosition = do
   x <- simpleGet fromIntegral glut_WINDOW_X
   y <- simpleGet fromIntegral glut_WINDOW_Y
   return $ Position x y

-- | Return the size of the /current window,/ measured in pixels.

getWindowSize :: IO Size
getWindowSize = do
   w <- simpleGet fromIntegral glut_WINDOW_WIDTH
   h <- simpleGet fromIntegral glut_WINDOW_HEIGHT
   return $ Size w h

-- | Return 'Just' the /current window\'s/ parent or 'Nothing' if the /current
-- window/ is a top-level window.

getParent :: IO (Maybe Window)
getParent = do
   w <- simpleGet makeWindow glut_WINDOW_PARENT
   return $ if w == makeWindow 0 then Nothing else Just w

-- | Return the number of subwindows the /current window/ has, not counting
-- children of children.

getSubWindowCount :: IO SubWindowCount
getSubWindowCount = simpleGet id glut_WINDOW_NUM_CHILDREN

-- | Return the current cursor for the /current window./

getCursor :: IO Cursor
getCursor = simpleGet unmarshalCursor glut_WINDOW_CURSOR

-- | Test whether the current layer of the /current window/ is in RGBA mode.
-- 'False' means color index mode.

isRGBA :: IO Bool
isRGBA = simpleGet i2b glut_WINDOW_RGBA

-- | Return the number of red, green, blue, and alpha bits in the color buffer
-- of the /current window\'s/ current layer (0 in color index mode).

getRGBABufferDepths :: IO (BufferDepth, BufferDepth, BufferDepth, BufferDepth)
getRGBABufferDepths = do
   r <- simpleGet id glut_WINDOW_RED_SIZE
   g <- simpleGet id glut_WINDOW_GREEN_SIZE
   b <- simpleGet id glut_WINDOW_BLUE_SIZE
   a <- simpleGet id glut_WINDOW_ALPHA_SIZE
   return (r, g, b, a)

-- | Return the total number of bits in the color buffer of the /current
-- window\'s/ current layer. For an RGBA layer, this is the sum of the red,
-- green, blue, and alpha bits. For an color index layer, this is the number
-- of bits of the color indexes.

getColorBufferDepth :: IO BufferDepth
getColorBufferDepth = simpleGet id glut_WINDOW_BUFFER_SIZE

-- | Return the number of entries in the colormap of the /current window\'s/
-- current layer (0 in RGBA mode).

getColormapEntryCount :: IO CInt
getColormapEntryCount = simpleGet id glut_WINDOW_COLORMAP_SIZE

-- | Test whether the current layer of the /current window/ is double buffered.

isDoubleBuffered :: IO Bool
isDoubleBuffered = simpleGet i2b glut_WINDOW_DOUBLEBUFFER

-- | Test whether the current layer of the /current window/ is stereo.

isStereo :: IO Bool
isStereo = simpleGet i2b glut_WINDOW_STEREO

-- | Return the number of red, green, blue, and alpha bits in the accumulation
-- buffer of the /current window\'s/ current layer (0 in color index mode).

getAccumBufferDepths :: IO (BufferDepth, BufferDepth, BufferDepth, BufferDepth)
getAccumBufferDepths = do
   r <- simpleGet id glut_WINDOW_ACCUM_RED_SIZE
   g <- simpleGet id glut_WINDOW_ACCUM_GREEN_SIZE
   b <- simpleGet id glut_WINDOW_ACCUM_BLUE_SIZE
   a <- simpleGet id glut_WINDOW_ACCUM_ALPHA_SIZE
   return (r, g, b, a)

-- | Return the number of bits in the depth buffer of the /current window\'s/
-- current layer.

getDepthBufferDepth :: IO BufferDepth
getDepthBufferDepth = simpleGet id glut_WINDOW_DEPTH_SIZE

-- | Return the number of bits in the stencil buffer of the /current window\'s/
-- current layer.

getStencilBufferDepth :: IO BufferDepth
getStencilBufferDepth = simpleGet id glut_WINDOW_STENCIL_SIZE

-- | Return the number of samples for multisampling for the /current window./

getSampleCount :: IO SampleCount
getSampleCount = simpleGet id glut_WINDOW_NUM_SAMPLES

-- | Return the window system dependent format ID for the current layer of the
-- /current window/. On X11 GLUT implementations, this is the X visual ID. On
-- Win32 GLUT implementations, this is the Win32 Pixel Format Descriptor number.
-- This value is returned for debugging, benchmarking, and testing ease.

getFormatID :: IO CInt
getFormatID = simpleGet id glut_WINDOW_FORMAT_ID

--------------------------------------------------------------------------------

-- | Return the number of milliseconds since
-- 'Graphics.UI.GLUT.Initialization.initialize' was called (or first call to
-- 'getElapsedTime').

getElapsedTime :: IO CInt
getElapsedTime = simpleGet id glut_ELAPSED_TIME

-- | Return the number of menu items in the /current menu./

getMenuItemCount :: IO CInt
getMenuItemCount = simpleGet id glut_MENU_NUM_ITEMS

--------------------------------------------------------------------------------

-- | Test whether an overlay could be established for the /current window/ given
-- the current /initial display mode/. If 'False',
-- 'Graphics.UI.GLUT.Overlay.establishOverlay' will fail with a fatal error if
-- called.

isOverlayPossible :: IO Bool
isOverlayPossible = layerGet i2b glut_OVERLAY_POSSIBLE

-- | Return the /layer in use/ of the /current window/ (normal plane or
-- overlay).

getLayerInUse :: IO Layer
getLayerInUse = layerGet i2l glut_LAYER_IN_USE

-- | Test if the /current window/ has an overlay established.

isOverlayEstablished :: IO Bool
isOverlayEstablished = layerGet i2b glut_HAS_OVERLAY

-- | Return 'Just' the transparent color index of the overlay of the /current
-- window/; 'Nothing' is returned if no overlay is in use.

getTransparentIndex :: IO (Maybe (Index1 CInt))
getTransparentIndex = layerGet i2c glut_TRANSPARENT_INDEX

-- | Test if the normal plane of the /current window/ has been damaged (by
-- window system activity) since the last display callback was triggered.
-- Calling 'Graphics.UI.GLUT.Window.postRedisplay' will not set this 'True'.

isNormalDamaged :: IO Bool
isNormalDamaged  = layerGet i2b glut_NORMAL_DAMAGED

-- | Test if the overlay plane of the /current window/ has been damaged (by
-- window system activity) since the last display callback was triggered.
-- Calling 'Graphics.UI.GLUT.Window.postRedisplay' or
-- 'Graphics.UI.GLUT.Overlay.postOverlayRedisplay' will not set this 'True'.
-- Return 'Nothing' if no overlay is in use.

isOverlayDamaged :: IO (Maybe Bool)
isOverlayDamaged = layerGet i2mb glut_OVERLAY_DAMAGED

--------------------------------------------------------------------------------

-- $DeviceInformation
-- If a device is not available, the following routines return 'Nothing',
-- otherwise they return 'Just' the specific device information.

--------------------------------------------------------------------------------
-- Although technically this belongs to "Simple GLUT state", it is more
-- appropriate here...

-- | The size of the screen in pixels and millimeters

data ScreenInfo = ScreenInfo Size Size

-- | /Note:/ A screen is always assumed, so there is no 'Maybe' here.

getScreenInfo :: IO ScreenInfo
getScreenInfo = do
  wpx <- simpleGet fromIntegral glut_SCREEN_WIDTH
  hpx <- simpleGet fromIntegral glut_SCREEN_HEIGHT
  wmm <- simpleGet fromIntegral glut_SCREEN_WIDTH_MM
  hmm <- simpleGet fromIntegral glut_SCREEN_HEIGHT_MM
  return $ ScreenInfo (Size wpx hpx) (Size wmm hmm)

--------------------------------------------------------------------------------

getDeviceInfo :: GLenum -> IO a -> IO (Maybe a)
getDeviceInfo dev act = do
   hasDevice <- deviceGet i2b dev
   if hasDevice then liftM Just act else return Nothing

--------------------------------------------------------------------------------

-- | The number of buttons of a mouse

newtype MouseInfo = MouseInfo ButtonCount
   deriving ( Eq, Ord )

-- | Return 'Just' the number of buttons of an attached mouse or 'Nothing' if
-- there is none.

getMouseInfo :: IO (Maybe MouseInfo)
getMouseInfo = getDeviceInfo glut_HAS_MOUSE $
   deviceGet MouseInfo glut_NUM_MOUSE_BUTTONS

--------------------------------------------------------------------------------

-- | The number of buttons of a Spaceball

newtype SpaceballInfo = SpaceballInfo ButtonCount
   deriving ( Eq, Ord )

-- | Return 'Just' the number of buttons of the attached Spaceball or 'Nothing'
-- if there is none.

getSpaceballInfo :: IO (Maybe SpaceballInfo)
getSpaceballInfo = getDeviceInfo glut_HAS_SPACEBALL $
   deviceGet SpaceballInfo glut_NUM_SPACEBALL_BUTTONS

--------------------------------------------------------------------------------

-- | The number of dials and buttons of a dial & button box

data DialAndButtonBoxInfo = DialAndButtonBoxInfo DialCount ButtonCount
   deriving ( Eq, Ord )

-- | Return 'Just' the number of dials and buttons of an attached dial & button
--  box or 'Nothing' if there is none.

getDialAndButtonBoxInfo :: IO (Maybe DialAndButtonBoxInfo)
getDialAndButtonBoxInfo = getDeviceInfo glut_HAS_DIAL_AND_BUTTON_BOX $ do
   d <- deviceGet id glut_NUM_DIALS
   b <- deviceGet id glut_NUM_BUTTON_BOX_BUTTONS
   return $ DialAndButtonBoxInfo d b

--------------------------------------------------------------------------------

-- | The number of buttons of a tablet

newtype TabletInfo = TabletInfo ButtonCount
   deriving ( Eq, Ord )

-- | Return 'Just' the number of buttons of an attached tablet or 'Nothing' if
-- there is none.

getTabletInfo :: IO (Maybe TabletInfo)
getTabletInfo = getDeviceInfo glut_HAS_TABLET $
   deviceGet TabletInfo glut_NUM_TABLET_BUTTONS

--------------------------------------------------------------------------------

-- | Information about a joystick

data JoystickInfo = JoystickInfo ButtonCount PollRate AxisCount
   deriving ( Eq, Ord )

-- | Return 'Just' some information about an attached joystick or 'Nothing' if
-- there is none.

getJoystickInfo :: IO (Maybe JoystickInfo)
getJoystickInfo = getDeviceInfo glut_HAS_JOYSTICK $ do
   b <- deviceGet id glut_JOYSTICK_BUTTONS
   r <- deviceGet id glut_JOYSTICK_POLL_RATE
   a <- deviceGet id glut_JOYSTICK_AXES
   return $ JoystickInfo b r a

--------------------------------------------------------------------------------
-- Convenience unmarshalers

i2b :: CInt -> Bool
i2b = (/= 0)

i2l :: CInt -> Layer
i2l = unmarshalLayer . fromIntegral

i2c :: CInt -> Maybe (Index1 CInt)
i2c i = if i < 0 then Nothing else Just (Index1 i)

i2mb :: CInt -> Maybe Bool
i2mb i = if i < 0 then Nothing else Just (i /= 0)

--------------------------------------------------------------------------------

-- marshaler is in Graphics.UI.GLUT.Window

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

-- marshaler is in Graphics.UI.GLUT.Overlay

unmarshalLayer :: GLenum -> Layer
unmarshalLayer l
   | l == glut_NORMAL  = Normal
   | l == glut_OVERLAY = Overlay
   | otherwise = error "unmarshalLayer"
