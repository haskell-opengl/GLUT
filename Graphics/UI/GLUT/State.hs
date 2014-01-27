--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.State
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- GLUT maintains a considerable amount of programmer visible state. Some (but
-- not all) of this state may be directly retrieved.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.State (
   -- * State of all windows
   windowBorderWidth, windowHeaderHeight, skipStaleMotionEvents,

   -- * State of the /current window/

   -- ** Framebuffer state
   rgba,
   BufferDepth, rgbaBufferDepths, colorBufferDepth,
   doubleBuffered, stereo,
   accumBufferDepths, depthBufferDepth, stencilBufferDepth,
   SampleCount, sampleCount, formatID,

   -- ** Full screen state
   fullScreenMode,

   -- ** Object rendering state
   geometryVisualizeNormals,

   -- ** Vertex attribute state
   vertexAttribCoord3, vertexAttribNormal, vertexAttribTexCoord2,

   -- ** Layer state
   damaged,

   -- * Timing
   elapsedTime,

   -- * Device information

   -- $DeviceInformation
   screenSize, screenSizeMM,
   hasKeyboard,
   ButtonCount, numMouseButtons,
   numSpaceballButtons,
   DialCount, numDialsAndButtons,
   numTabletButtons,
   AxisCount, PollRate, joystickInfo,
   supportedNumAuxBuffers, supportedSamplesPerPixel,

   -- * GLUT information
   glutVersion, initState
) where

import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT.Overlay
import Graphics.UI.GLUT.QueryUtils
import Graphics.UI.GLUT.Raw
import Graphics.UI.GLUT.Window

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

-- | (/freeglut only/) Contains 'True' if the /current window/ is in full screen
-- mode, 'False' otherwise.

fullScreenMode :: StateVar Bool
fullScreenMode = makeStateVar getFullScreenMode setFullScreenMode

getFullScreenMode :: IO Bool
getFullScreenMode = simpleGet i2b glut_FULL_SCREEN

setFullScreenMode :: Bool -> IO ()
setFullScreenMode newMode = do
   oldMode <- getFullScreenMode
   unless (newMode == oldMode) fullScreenToggle

--------------------------------------------------------------------------------

-- | (/freeglut only/) Controls if vectors representing the normals should be
-- drawn, too, when objects are drawn.

geometryVisualizeNormals :: StateVar Bool
geometryVisualizeNormals =
   makeStateVar
      (simpleGet i2b glut_GEOMETRY_VISUALIZE_NORMALS)
      (glutSetOption glut_GEOMETRY_VISUALIZE_NORMALS . b2i)


--------------------------------------------------------------------------------

-- | (/freeglut only/) If 'vertexAttribCoord3' and 'vertexAttribNormal' both
-- contain 'Nothing', the fixed function pipeline is used to draw
-- objects. Otherwise VBOs are used and the coordinates are passed via 'Just'
-- this attribute location (for a vec3).

vertexAttribCoord3 :: SettableStateVar (Maybe AttribLocation)
vertexAttribCoord3 = setVertexAttribWith glutSetVertexAttribCoord3

setVertexAttribWith :: (GLint -> IO ()) -> SettableStateVar (Maybe AttribLocation)
setVertexAttribWith f = makeSettableStateVar $ f . getLocation
   where getLocation = maybe (-1) (\(AttribLocation l) -> fromIntegral l)

-- | (/freeglut only/) If 'vertexAttribCoord3' and 'vertexAttribNormal' both
-- contain 'Nothing', the fixed function pipeline is used to draw
-- objects. Otherwise VBOs are used and the normals are passed via 'Just' this
-- attribute location (for a vec3).

vertexAttribNormal :: SettableStateVar (Maybe AttribLocation)
vertexAttribNormal = setVertexAttribWith glutSetVertexAttribNormal

-- | (/freeglut only/) If VBOs are used to draw objects (controlled via
-- 'vertexAttribCoord3' and 'vertexAttribNormal'), the texture coordinates are
-- passed via 'Just' this attribute location (for a vec2).

vertexAttribTexCoord2 :: SettableStateVar (Maybe AttribLocation)
vertexAttribTexCoord2 = setVertexAttribWith glutSetVertexAttribTexCoord2

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
         marshalDamagedLayer x = case x of
            Normal -> glut_NORMAL_DAMAGED
            Overlay -> glut_OVERLAY_DAMAGED

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

-----------------------------------------------------------------------------

-- | (/freeglut only/) Contains a list of the number of auxiliary buffers
-- supported, in increasing order.

supportedNumAuxBuffers :: GettableStateVar [Int]
supportedNumAuxBuffers = getModeValues glut_AUX

-- | (/freeglut only/) Contains a list of the number of samples per pixel
-- supported for multisampling, in increasing order.

supportedSamplesPerPixel :: GettableStateVar [SampleCount]
supportedSamplesPerPixel = getModeValues (fromIntegral glut_MULTISAMPLE)

getModeValues :: Integral a => GLenum -> GettableStateVar [a]
getModeValues what = makeGettableStateVar $
   alloca $ \sizeBuffer -> do
      valuesBuffer <- glutGetModeValues what sizeBuffer
      size <- peek sizeBuffer
      fmap (map fromIntegral) $ peekArray (fromIntegral size) valuesBuffer

--------------------------------------------------------------------------------
-- Convenience (un-)marshalers

i2b :: CInt -> Bool
i2b = (/= 0)

b2i :: Bool ->  CInt
b2i False = 0
b2i True = 1

--------------------------------------------------------------------------------

getDeviceInfo :: GLenum -> IO a -> GettableStateVar (Maybe a)
getDeviceInfo dev act =
   makeGettableStateVar $ do
      hasDevice <- deviceGet i2b dev
      if hasDevice then fmap Just act else return Nothing

-----------------------------------------------------------------------------

-- | Contains version of GLUT in the form of
-- @/flavour/ /major/./minor/./patchlevel/@, where @/flavour/@ is one of
-- @GLUT@, @freeglut@ or @OpenGLUT@.

glutVersion :: GettableStateVar String
glutVersion = makeGettableStateVar $ do
   let isGLUT = isUnknown "glutSetOption"
       isFreeglut = isUnknown "glutSetWindowStayOnTop"
       isUnknown = fmap (== nullFunPtr) . getAPIEntryInternal
       showVersionPart x = shows (x `mod` 100)
       showVersion v = showVersionPart (v `div` 10000) . showChar '.' .
                       showVersionPart (v `div`   100) . showChar '.' .
                       showVersionPart  v
   g <- isGLUT
   if g
      then return "GLUT 3.7"   -- ToDo: just guessing
      else do f <- isFreeglut
              v <- simpleGet id glut_VERSION
              let prefix = if f then "freeglut" else "OpenGLUT"
              return $ showString prefix . showChar ' ' . showVersion v $ ""

-----------------------------------------------------------------------------

-- | (/freeglut only/) Contains the thickness of the sizing border around the
-- perimeter of a window that can be resized, in pixels.

windowBorderWidth :: GettableStateVar Int
windowBorderWidth =
   makeGettableStateVar (simpleGet fromIntegral glut_WINDOW_BORDER_WIDTH)

-----------------------------------------------------------------------------

-- | (/freeglut only/) Contains the height of the header\/caption area of a
-- window in pixels.

windowHeaderHeight :: GettableStateVar Int
windowHeaderHeight =
   makeGettableStateVar (simpleGet fromIntegral glut_WINDOW_HEADER_HEIGHT)

-----------------------------------------------------------------------------

-- | (/freeglut on X11 only/) Controls if all but the last motion event should
-- be discarded.

skipStaleMotionEvents :: StateVar Bool
skipStaleMotionEvents =
   makeStateVar
      (simpleGet i2b glut_SKIP_STALE_MOTION_EVENTS)
      (glutSetOption glut_SKIP_STALE_MOTION_EVENTS . b2i)

-----------------------------------------------------------------------------

-- | (/freeglut only/) Contains 'True' if GLUT has been initialized 
-- with 'Graphics.UI.GLUT.Initialization.initialize' or
-- 'Graphics.UI.GLUT.Initialization.getArgsAndInitialize' has and not yet
-- been de-initialized with 'Graphics.UI.GLUT.Initialization.exit'. Contains
-- 'False' otherwise.

initState :: GettableStateVar Bool
initState = makeGettableStateVar$ simpleGet i2b glut_INIT_STATE
