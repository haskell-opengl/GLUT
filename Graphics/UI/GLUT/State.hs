--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.State
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- GLUT maintains a considerable amount of programmer visible state. Some (but
-- not all) of this state may be directly retrieved.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.State (
  -- * State of the /current window/
  isRGBA,
  BufferDepth, getRGBABufferDepths, getColorBufferDepth,
  isDoubleBuffered, isStereo,
  getAccumBufferDepths, getDepthBufferDepth, getStencilBufferDepth,
  SampleCount, getSampleCount, getFormatID,

  -- * Timing
  elapsedTime,

  -- * GLUT state pertaining to the layers of the /current window/
  isNormalDamaged, isOverlayDamaged,

  -- * Device information

  -- $DeviceInformation
  screenSize, screenSizeMM,
  hasKeyboard,
  ButtonCount, numMouseButtons,
  numSpaceballButtons,
  DialCount, numDialsAndButtons,
  numTabletButtons,
  AxisCount, PollRate, joystickInfo
) where

import Control.Monad ( liftM )
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Graphics.UI.GLUT.Constants (
   glut_WINDOW_RGBA,
   glut_WINDOW_RED_SIZE, glut_WINDOW_GREEN_SIZE, glut_WINDOW_BLUE_SIZE,
   glut_WINDOW_ALPHA_SIZE, glut_WINDOW_BUFFER_SIZE,
   glut_WINDOW_DOUBLEBUFFER, glut_WINDOW_STEREO,
   glut_WINDOW_ACCUM_RED_SIZE, glut_WINDOW_ACCUM_GREEN_SIZE,
   glut_WINDOW_ACCUM_BLUE_SIZE, glut_WINDOW_ACCUM_ALPHA_SIZE,
   glut_WINDOW_DEPTH_SIZE, glut_WINDOW_STENCIL_SIZE, glut_WINDOW_NUM_SAMPLES,
   glut_WINDOW_FORMAT_ID, glut_ELAPSED_TIME,
   glut_NORMAL_DAMAGED, glut_OVERLAY_DAMAGED,
   glut_SCREEN_WIDTH, glut_SCREEN_HEIGHT,
   glut_SCREEN_WIDTH_MM, glut_SCREEN_HEIGHT_MM,
   glut_HAS_KEYBOARD,
   glut_HAS_MOUSE, glut_NUM_MOUSE_BUTTONS,
   glut_HAS_SPACEBALL, glut_NUM_SPACEBALL_BUTTONS,
   glut_HAS_DIAL_AND_BUTTON_BOX, glut_NUM_DIALS, glut_NUM_BUTTON_BOX_BUTTONS,
   glut_HAS_TABLET, glut_NUM_TABLET_BUTTONS,
   glut_HAS_JOYSTICK, glut_JOYSTICK_BUTTONS, glut_JOYSTICK_POLL_RATE,
   glut_JOYSTICK_AXES )
import Graphics.UI.GLUT.QueryUtils ( simpleGet, layerGet, deviceGet )

--------------------------------------------------------------------------------

-- | Test whether the current layer of the /current window/ is in RGBA mode.
-- 'False' means color index mode.

isRGBA :: IO Bool
isRGBA = simpleGet i2b glut_WINDOW_RGBA

-- | Bit depth of a buffer

type BufferDepth = CInt

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

-- | Number of samples for multisampling

type SampleCount = CInt

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

-- | Contains the number of milliseconds since
-- 'Graphics.UI.GLUT.Initialization.initialize' was called.

elapsedTime :: GettableStateVar Int
elapsedTime = makeGettableStateVar $ simpleGet fromIntegral glut_ELAPSED_TIME

--------------------------------------------------------------------------------

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
-- If a device is not available, the following state variables contain
-- 'Nothing', otherwise they return 'Just' the specific device information.
-- Only a screen is always assumed.

--------------------------------------------------------------------------------

-- | The size of the screen in pixels.

screenSize :: GettableStateVar Size
screenSize =
   makeGettableStateVar $ do
      wpx <- simpleGet fromIntegral glut_SCREEN_WIDTH
      hpx <- simpleGet fromIntegral glut_SCREEN_HEIGHT
      return $ Size wpx hpx

-- | The size of the screen in millimeters.

screenSizeMM :: GettableStateVar Size
screenSizeMM =
   makeGettableStateVar $ do
      wmm <- simpleGet fromIntegral glut_SCREEN_WIDTH_MM
      hmm <- simpleGet fromIntegral glut_SCREEN_HEIGHT_MM
      return $ Size wmm hmm

--------------------------------------------------------------------------------

-- | Contains 'True' if a keyboard is present, 'False' otherwise.

hasKeyboard :: GettableStateVar Bool
hasKeyboard = makeGettableStateVar $ deviceGet i2b glut_HAS_KEYBOARD

--------------------------------------------------------------------------------

-- | Number of buttons of an input device

type ButtonCount = CInt

-- | Contains 'Just' the number of buttons of an attached mouse or 'Nothing' if
-- there is none.

numMouseButtons :: GettableStateVar (Maybe ButtonCount)
numMouseButtons =
   getDeviceInfo glut_HAS_MOUSE $
      deviceGet id glut_NUM_MOUSE_BUTTONS

--------------------------------------------------------------------------------

-- | Contains 'Just' the number of buttons of the attached Spaceball or 'Nothing'
-- if there is none.

numSpaceballButtons :: GettableStateVar (Maybe ButtonCount)
numSpaceballButtons =
   getDeviceInfo glut_HAS_SPACEBALL $
      deviceGet id glut_NUM_SPACEBALL_BUTTONS

--------------------------------------------------------------------------------

-- | Number of dials of a dial and button box

type DialCount = CInt

-- | Contains 'Just' the number of dials and buttons of an attached dial &
-- button box or 'Nothing' if there is none.

numDialsAndButtons :: GettableStateVar (Maybe (DialCount, ButtonCount))
numDialsAndButtons =
   getDeviceInfo glut_HAS_DIAL_AND_BUTTON_BOX $ do
      d <- deviceGet id glut_NUM_DIALS
      b <- deviceGet id glut_NUM_BUTTON_BOX_BUTTONS
      return (d, b)

--------------------------------------------------------------------------------

-- | Contains 'Just' the number of buttons of an attached tablet or 'Nothing' if
-- there is none.

numTabletButtons :: GettableStateVar (Maybe ButtonCount)
numTabletButtons =
   getDeviceInfo glut_HAS_TABLET $
      deviceGet id glut_NUM_TABLET_BUTTONS

--------------------------------------------------------------------------------

-- | Number of axes of a joystick

type AxisCount  = CInt

-- | The a rate at which a joystick is polled (in milliseconds)

type PollRate = CInt

-- | Contains 'Just' the number of buttons of an attached joystick, the number
-- of joystick axes, and the rate at which the joystick is polled. Contains
-- 'Nothing' if there is no joystick attached.

joystickInfo :: GettableStateVar (Maybe (ButtonCount, PollRate, AxisCount))
joystickInfo =
   getDeviceInfo glut_HAS_JOYSTICK $ do
      b <- deviceGet id glut_JOYSTICK_BUTTONS
      a <- deviceGet id glut_JOYSTICK_AXES
      r <- deviceGet id glut_JOYSTICK_POLL_RATE
      return (b, a, r)

--------------------------------------------------------------------------------
-- Convenience unmarshalers

i2b :: CInt -> Bool
i2b = (/= 0)

i2mb :: CInt -> Maybe Bool
i2mb i = if i < 0 then Nothing else Just (i /= 0)

--------------------------------------------------------------------------------

getDeviceInfo :: GLenum -> IO a -> GettableStateVar (Maybe a)
getDeviceInfo dev act =
   makeGettableStateVar $ do
      hasDevice <- deviceGet i2b dev
      if hasDevice then liftM Just act else return Nothing
