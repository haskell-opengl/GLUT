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
  NumChildren, NumSamples, NumBits, NumButtons, NumDials, NumAxes, PollRate,

  -- * Initial window parameters
  getInitWindowPosition, getInitWindowSize, getInitDisplayMode,
  isDisplayModePossible,

  -- * State of the /current window/
  getWindowPosition, getWindowSize,
  getParent, getNumChildren,
  getCursor,
  isRGBA, getNumColorBits,
  getNumBufferBits, getNumColormapEntries,
  isDoubleBuffered, isStereo,
  getNumAccumBits, getNumDepthBits, getNumStencilBits,
  getNumSamples, getFormatID,

  -- * Miscellaneous
  getElapsedTime, getNumMenuItems,

  -- * GLUT state pertaining to the layers of the  /current window/
  isOverlayPossible, getLayerInUse, isOverlayEstablished, getTransparentIndex,
  isNormalDamaged, isOverlayDamaged,

  -- * Device information

  -- $DeviceInformation

  -- ** Screen information
  ScreenInfo(..), getScreenInfo,

  -- ** Keyboard information
  KeyRepeat(..), KeyboardInfo(..), getKeyboardInfo,

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
import Data.Bits ( Bits((.&.)) )
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( ColorIndex(..) )
import Graphics.UI.GLUT.Constants
import Graphics.UI.GLUT.Overlay ( Layer(..) )
import Graphics.UI.GLUT.Initialization ( WindowPosition(..), WindowSize(..),
                                         DisplayMode(..), marshalDisplayMode )
import Graphics.UI.GLUT.Window ( Window(..), Cursor(..) )

--------------------------------------------------------------------------------

-- | Number of children of a window

type NumChildren = CInt

-- | Number of samples for multisampling

type NumSamples = CInt

-- | Bit depth of a buffer

type NumBits = CInt

-- | Number of buttons of an input device

type NumButtons = CInt

-- | Number of dials of a dial and button box

type NumDials = CInt

-- | Number of axes of a joystick

type NumAxes  = CInt

-- | The a rate at which a joystick is polled

type PollRate = CInt

--------------------------------------------------------------------------------

-- | Return the /initial window position./
getInitWindowPosition :: IO WindowPosition
getInitWindowPosition = do
   x <- get id glut_INIT_WINDOW_X
   y <- get id glut_INIT_WINDOW_Y
   return $ WindowPosition x y

-- | Return the /initial window size./

getInitWindowSize :: IO WindowSize
getInitWindowSize = do
   w <- get id glut_INIT_WINDOW_WIDTH
   h <- get id glut_INIT_WINDOW_HEIGHT
   return $ WindowSize w h

-- | Return the /initial display mode./

getInitDisplayMode :: IO [DisplayMode]
getInitDisplayMode =
   get i2dms glut_INIT_DISPLAY_MODE

-- | Test whether the /current display mode/ is supported or not.

isDisplayModePossible :: IO Bool
isDisplayModePossible = get i2b glut_DISPLAY_MODE_POSSIBLE

--------------------------------------------------------------------------------

-- | Return the location of the /current window,/ measured in pixels relative to
-- the screen origin.

getWindowPosition :: IO WindowPosition
getWindowPosition = do
   x <- get id glut_WINDOW_X
   y <- get id glut_WINDOW_Y
   return $ WindowPosition x y

-- | Return the size of the /current window,/ measured in pixels.

getWindowSize :: IO WindowSize
getWindowSize = do
   w <- get id glut_WINDOW_WIDTH
   h <- get id glut_WINDOW_HEIGHT
   return $ WindowSize w h

-- | Return 'Just' the /current window\'s/ parent or 'Nothing' if the /current
-- window/ is a top-level window.

getParent :: IO (Maybe Window)
getParent = do
   w <- get Window glut_WINDOW_PARENT
   return $ if w == Window 0 then Nothing else Just w

-- | Return the number of subwindows the /current window/ has, not counting
-- children of children.

getNumChildren :: IO NumChildren
getNumChildren = get id glut_WINDOW_NUM_CHILDREN

-- | Return the current cursor for the /current window./

getCursor :: IO Cursor
getCursor = get unmarshalCursor glut_WINDOW_CURSOR

-- | Test whether the current layer of the /current window/ is in RGBA mode.
-- 'False' means color index mode.

isRGBA :: IO Bool
isRGBA = get i2b glut_WINDOW_RGBA

-- | Return the number of red, green, blue, and alpha bits in the color buffer
-- of the /current window\'s/ current layer (0 in color index mode).

getNumColorBits :: IO (NumBits, NumBits, NumBits, NumBits)
getNumColorBits = do
   r <- get id glut_WINDOW_RED_SIZE
   g <- get id glut_WINDOW_GREEN_SIZE
   b <- get id glut_WINDOW_BLUE_SIZE
   a <- get id glut_WINDOW_ALPHA_SIZE
   return (r, g, b, a)

-- | Return the total number of bits in the color buffer of the /current
-- window\'s/ current layer. For an RGBA layer, this is the sum of the red,
-- green, blue, and alpha bits. For an color index layer, this is the number
-- of bits of the color indexes.

getNumBufferBits :: IO NumBits
getNumBufferBits = get id glut_WINDOW_BUFFER_SIZE

-- | Return the number of entries in the colormap of the /current window\'s/
-- current layer (0 in RGBA mode).

getNumColormapEntries :: IO (ColorIndex CInt)
getNumColormapEntries = get ColorIndex glut_WINDOW_COLORMAP_SIZE

-- | Test whether the current layer of the /current window/ is double buffered.

isDoubleBuffered :: IO Bool
isDoubleBuffered = get i2b glut_WINDOW_DOUBLEBUFFER

-- | Test whether the current layer of the /current window/ is stereo.

isStereo :: IO Bool
isStereo = get i2b glut_WINDOW_STEREO

-- | Return the number of red, green, blue, and alpha bits in the accumulation
-- buffer of the /current window\'s/ current layer (0 in color index mode).

getNumAccumBits :: IO (NumBits, NumBits, NumBits, NumBits)
getNumAccumBits = do
   r <- get id glut_WINDOW_ACCUM_RED_SIZE
   g <- get id glut_WINDOW_ACCUM_GREEN_SIZE
   b <- get id glut_WINDOW_ACCUM_BLUE_SIZE
   a <- get id glut_WINDOW_ACCUM_ALPHA_SIZE
   return (r, g, b, a)

-- | Return the number of bits in the depth buffer of the /current window\'s/
-- current layer.

getNumDepthBits :: IO NumBits
getNumDepthBits = get id glut_WINDOW_DEPTH_SIZE

-- | Return the number of bits in the stencil buffer of the /current window\'s/
-- current layer.

getNumStencilBits :: IO NumBits
getNumStencilBits = get id glut_WINDOW_STENCIL_SIZE

-- | Return the number of samples for multisampling for the /current window./

getNumSamples :: IO NumSamples
getNumSamples = get id glut_WINDOW_NUM_SAMPLES

-- | Return the window system dependent format ID for the current layer of the
-- /current window/. On X11 GLUT implementations, this is the X visual ID. On
-- Win32 GLUT implementations, this is the Win32 Pixel Format Descriptor number.
-- This value is returned for debugging, benchmarking, and testing ease.

getFormatID :: IO CInt
getFormatID = get id glut_WINDOW_FORMAT_ID

--------------------------------------------------------------------------------

-- | Return the number of milliseconds since
-- 'Graphics.UI.GLUT.Initialization.init' was called (or first call to
-- 'getElapsedTime').

getElapsedTime :: IO CInt
getElapsedTime = get id glut_ELAPSED_TIME

-- | Return the number of menu items in the /current menu./

getNumMenuItems :: IO CInt
getNumMenuItems = get id glut_MENU_NUM_ITEMS

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

getTransparentIndex :: IO (Maybe (ColorIndex CInt))
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

data ScreenInfo = ScreenInfo WindowSize WindowSize

-- | /Note:/ A screen is always assumed, so there is no 'Maybe' here.

getScreenInfo :: IO ScreenInfo
getScreenInfo = do
  wpx <- get id glut_SCREEN_WIDTH
  hpx <- get id glut_SCREEN_HEIGHT
  wmm <- get id glut_SCREEN_WIDTH_MM
  hmm <- get id glut_SCREEN_HEIGHT_MM
  return $ ScreenInfo (WindowSize wpx hpx) (WindowSize wmm hmm)

--------------------------------------------------------------------------------

data KeyRepeat
   = KeyRepeatOn
   | KeyRepeatOff
   | KeyRepeatDefault
   deriving ( Eq, Ord )

unmarshalKeyRepeat :: CInt -> KeyRepeat
unmarshalKeyRepeat r
   | r == glut_KEY_REPEAT_ON      = KeyRepeatOn
   | r == glut_KEY_REPEAT_OFF     = KeyRepeatOff
   | r == glut_KEY_REPEAT_DEFAULT = KeyRepeatDefault
   | otherwise = error "unmarshalKeyRepeat"

data KeyboardInfo = KeyboardInfo KeyRepeat Bool
   deriving ( Eq, Ord )

getKeyboardInfo :: IO (Maybe KeyboardInfo)
getKeyboardInfo = getDeviceInfo glut_HAS_KEYBOARD $ do
   r <- deviceGet unmarshalKeyRepeat glut_DEVICE_KEY_REPEAT
   i <- deviceGet i2b glut_DEVICE_IGNORE_KEY_REPEAT
   return $ KeyboardInfo r i

getDeviceInfo :: GLenum -> IO a -> IO (Maybe a)
getDeviceInfo dev act = do
   hasDevice <- deviceGet i2b dev
   if hasDevice then liftM Just act else return Nothing

--------------------------------------------------------------------------------

newtype MouseInfo = MouseInfo NumButtons
   deriving ( Eq, Ord )

getMouseInfo :: IO (Maybe MouseInfo)
getMouseInfo = getDeviceInfo glut_HAS_MOUSE $
   deviceGet MouseInfo glut_NUM_MOUSE_BUTTONS

--------------------------------------------------------------------------------

newtype SpaceballInfo = SpaceballInfo NumButtons
   deriving ( Eq, Ord )

getSpaceballInfo :: IO (Maybe SpaceballInfo)
getSpaceballInfo = getDeviceInfo glut_HAS_SPACEBALL $
   deviceGet SpaceballInfo glut_NUM_SPACEBALL_BUTTONS

--------------------------------------------------------------------------------

data DialAndButtonBoxInfo = DialAndButtonBoxInfo NumButtons NumDials
   deriving ( Eq, Ord )

getDialAndButtonBoxInfo :: IO (Maybe DialAndButtonBoxInfo)
getDialAndButtonBoxInfo = getDeviceInfo glut_HAS_DIAL_AND_BUTTON_BOX $ do
   b <- deviceGet id glut_NUM_BUTTON_BOX_BUTTONS
   d <- deviceGet id glut_NUM_DIALS
   return $ DialAndButtonBoxInfo b d

--------------------------------------------------------------------------------

newtype TabletInfo = TabletInfo NumButtons
   deriving ( Eq, Ord )

getTabletInfo :: IO (Maybe TabletInfo)
getTabletInfo = getDeviceInfo glut_HAS_TABLET $
   deviceGet TabletInfo glut_NUM_TABLET_BUTTONS

--------------------------------------------------------------------------------

data JoystickInfo = JoystickInfo NumButtons PollRate NumAxes
   deriving ( Eq, Ord )

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

i2c :: CInt -> Maybe (ColorIndex CInt)
i2c i = if i < 0 then Nothing else Just (ColorIndex i)

i2mb :: CInt -> Maybe Bool
i2mb i = if i < 0 then Nothing else Just (i /= 0)

i2dms :: CInt -> [DisplayMode]
i2dms = fromBitfield marshalDisplayMode . fromIntegral

fromBitfield :: (Enum a, Bounded a, Bits b) => (a -> b) -> b -> [a]
fromBitfield marshal bitfield =
   [ c | c <- [ minBound .. maxBound ],  (bitfield .&. marshal c) /= 0 ]

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

--------------------------------------------------------------------------------

-- Convenience wrappers for primitive getters

type PrimGetter =                GLenum -> IO CInt
type Getter a   = (CInt -> a) -> GLenum -> IO a

makeGetter :: PrimGetter -> Getter a
makeGetter g f = liftM f . g

get, layerGet, deviceGet :: Getter a
get       = makeGetter glutGet
layerGet  = makeGetter glutLayerGet
deviceGet = makeGetter glutDeviceGet

foreign import ccall unsafe "glutGet"       glutGet       :: PrimGetter
foreign import ccall unsafe "glutLayerGet"  glutLayerGet  :: PrimGetter
foreign import ccall unsafe "glutDeviceGet" glutDeviceGet :: PrimGetter
