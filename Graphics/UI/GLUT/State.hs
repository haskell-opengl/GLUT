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
  rgba,
  BufferDepth, rgbaBufferDepths, colorBufferDepth,
  doubleBuffered, stereo,
  accumBufferDepths, depthBufferDepth, stencilBufferDepth,
  SampleCount, sampleCount, formatID,

  -- * Timing
  elapsedTime,

  -- * GLUT state pertaining to the layers of the /current window/
  damaged,

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
import Graphics.UI.GLUT.Overlay ( Layer(..) )
import Graphics.UI.GLUT.QueryUtils ( simpleGet, layerGet, deviceGet )

--------------------------------------------------------------------------------

-- | Contains 'True' when the current layer of the /current window/ is in RGBA
-- mode, 'False' means color index mode.

rgba :: GettableStateVar Bool
rgba = makeGettableStateVar$ simpleGet i2b glut_WINDOW_RGBA

-- | Bit depth of a buffer

type BufferDepth = Int

-- | Contains the number of red, green, blue, and alpha bits in the color buffer
-- of the /current window\'s/ current layer (0 in color index mode).

rgbaBufferDepths ::
   GettableStateVar (BufferDepth, BufferDepth, BufferDepth, BufferDepth)
rgbaBufferDepths = makeGettableStateVar $ do
   r <- simpleGet fromIntegral glut_WINDOW_RED_SIZE
   g <- simpleGet fromIntegral glut_WINDOW_GREEN_SIZE
   b <- simpleGet fromIntegral glut_WINDOW_BLUE_SIZE
   a <- simpleGet fromIntegral glut_WINDOW_ALPHA_SIZE
   return (r, g, b, a)

-- | Contains the total number of bits in the color buffer of the /current
-- window\'s/ current layer. For an RGBA layer, this is the sum of the red,
-- green, blue, and alpha bits. For an color index layer, this is the number
-- of bits of the color indexes.

colorBufferDepth :: GettableStateVar BufferDepth
colorBufferDepth =
   makeGettableStateVar $ simpleGet fromIntegral glut_WINDOW_BUFFER_SIZE

-- | Contains 'True' when the current layer of the /current window/ is double
-- buffered, 'False' otherwise.

doubleBuffered :: GettableStateVar Bool
doubleBuffered = makeGettableStateVar $ simpleGet i2b glut_WINDOW_DOUBLEBUFFER

-- | Contains 'True' when the current layer of the /current window/ is stereo,
-- 'False' otherwise.

stereo :: GettableStateVar Bool
stereo = makeGettableStateVar $ simpleGet i2b glut_WINDOW_STEREO

-- | Contains the number of red, green, blue, and alpha bits in the accumulation
-- buffer of the /current window\'s/ current layer (0 in color index mode).

accumBufferDepths ::
   GettableStateVar (BufferDepth, BufferDepth, BufferDepth, BufferDepth)
accumBufferDepths = makeGettableStateVar $ do
   r <- simpleGet fromIntegral glut_WINDOW_ACCUM_RED_SIZE
   g <- simpleGet fromIntegral glut_WINDOW_ACCUM_GREEN_SIZE
   b <- simpleGet fromIntegral glut_WINDOW_ACCUM_BLUE_SIZE
   a <- simpleGet fromIntegral glut_WINDOW_ACCUM_ALPHA_SIZE
   return (r, g, b, a)

-- | Contains the number of bits in the depth buffer of the /current window\'s/
-- current layer.

depthBufferDepth :: GettableStateVar BufferDepth
depthBufferDepth =
   makeGettableStateVar $ simpleGet fromIntegral glut_WINDOW_DEPTH_SIZE

-- | Contains the number of bits in the stencil buffer of the /current
-- window\'s/ current layer.

stencilBufferDepth :: GettableStateVar BufferDepth
stencilBufferDepth =
   makeGettableStateVar $ simpleGet fromIntegral glut_WINDOW_STENCIL_SIZE

-- | Number of samples for multisampling

type SampleCount = Int

-- | Contains the number of samples for multisampling for the /current window./

sampleCount :: GettableStateVar SampleCount
sampleCount =
   makeGettableStateVar $ simpleGet fromIntegral glut_WINDOW_NUM_SAMPLES

-- | Contains the window system dependent format ID for the current layer of the
-- /current window/. On X11 GLUT implementations, this is the X visual ID. On
-- Win32 GLUT implementations, this is the Win32 Pixel Format Descriptor number.
-- This value is returned for debugging, benchmarking, and testing ease.

formatID :: GettableStateVar Int
formatID = makeGettableStateVar $ simpleGet fromIntegral glut_WINDOW_FORMAT_ID

--------------------------------------------------------------------------------

-- | Contains the number of milliseconds since
-- 'Graphics.UI.GLUT.Initialization.initialize' was called.

elapsedTime :: GettableStateVar Int
elapsedTime = makeGettableStateVar $ simpleGet fromIntegral glut_ELAPSED_TIME

--------------------------------------------------------------------------------

-- | Contains 'True' if the given plane of the /current window/ has been
-- damaged (by window system activity) since the last display callback was
-- triggered. Calling 'Graphics.UI.GLUT.Window.postRedisplay' or
-- 'Graphics.UI.GLUT.Overlay.postOverlayRedisplay' will not set this 'True'.

damaged :: Layer -> GettableStateVar Bool
damaged l = makeGettableStateVar $ layerGet isDamaged (marshalDamagedLayer l)
   where isDamaged d = d /= 0 && d /= -1
         marshalDamagedLayer Normal  = glut_NORMAL_DAMAGED
         marshalDamagedLayer Overlay = glut_OVERLAY_DAMAGED

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

type ButtonCount = Int

-- | Contains 'Just' the number of buttons of an attached mouse or 'Nothing' if
-- there is none.

numMouseButtons :: GettableStateVar (Maybe ButtonCount)
numMouseButtons =
   getDeviceInfo glut_HAS_MOUSE $
      deviceGet fromIntegral glut_NUM_MOUSE_BUTTONS

--------------------------------------------------------------------------------

-- | Contains 'Just' the number of buttons of the attached Spaceball or 'Nothing'
-- if there is none.

numSpaceballButtons :: GettableStateVar (Maybe ButtonCount)
numSpaceballButtons =
   getDeviceInfo glut_HAS_SPACEBALL $
      deviceGet fromIntegral glut_NUM_SPACEBALL_BUTTONS

--------------------------------------------------------------------------------

-- | Number of dials of a dial and button box

type DialCount = Int

-- | Contains 'Just' the number of dials and buttons of an attached dial &
-- button box or 'Nothing' if there is none.

numDialsAndButtons :: GettableStateVar (Maybe (DialCount, ButtonCount))
numDialsAndButtons =
   getDeviceInfo glut_HAS_DIAL_AND_BUTTON_BOX $ do
      d <- deviceGet fromIntegral glut_NUM_DIALS
      b <- deviceGet fromIntegral glut_NUM_BUTTON_BOX_BUTTONS
      return (d, b)

--------------------------------------------------------------------------------

-- | Contains 'Just' the number of buttons of an attached tablet or 'Nothing' if
-- there is none.

numTabletButtons :: GettableStateVar (Maybe ButtonCount)
numTabletButtons =
   getDeviceInfo glut_HAS_TABLET $
      deviceGet fromIntegral glut_NUM_TABLET_BUTTONS

--------------------------------------------------------------------------------

-- | Number of axes of a joystick

type AxisCount = Int

-- | The a rate at which a joystick is polled (in milliseconds)

type PollRate = Int

-- | Contains 'Just' the number of buttons of an attached joystick, the number
-- of joystick axes, and the rate at which the joystick is polled. Contains
-- 'Nothing' if there is no joystick attached.

joystickInfo :: GettableStateVar (Maybe (ButtonCount, PollRate, AxisCount))
joystickInfo =
   getDeviceInfo glut_HAS_JOYSTICK $ do
      b <- deviceGet fromIntegral glut_JOYSTICK_BUTTONS
      a <- deviceGet fromIntegral glut_JOYSTICK_AXES
      r <- deviceGet fromIntegral glut_JOYSTICK_POLL_RATE
      return (b, a, r)

--------------------------------------------------------------------------------
-- Convenience unmarshalers

i2b :: CInt -> Bool
i2b = (/= 0)

--------------------------------------------------------------------------------

getDeviceInfo :: GLenum -> IO a -> GettableStateVar (Maybe a)
getDeviceInfo dev act =
   makeGettableStateVar $ do
      hasDevice <- deviceGet i2b dev
      if hasDevice then liftM Just act else return Nothing
