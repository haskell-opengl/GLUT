--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Callbacks.Window
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Callbacks.Window (
   -- * Redisplay callbacks
   DisplayCallback, displayCallback, overlayDisplayCallback,

   -- * Reshape callback
   ReshapeCallback, reshapeCallback,

   -- * Callbacks for visibility changes
   Visibility(..), VisibilityCallback, visibilityCallback,
   WindowState(..), WindowStateCallback, windowStateCallback,

   -- * Window close callback
   CloseCallback, closeCallback,

   -- * Keyboard callback
   KeyboardCallback, keyboardCallback, keyboardUpCallback,

   -- * Special callback
   SpecialCallback, specialCallback, specialUpCallback,

   -- * Mouse callback
   MouseCallback, mouseCallback,

   -- * Keyboard and mouse input callback
   Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..),
   KeyboardMouseCallback, keyboardMouseCallback,

   -- * Mouse wheel callback
   WheelNumber, WheelDirection, MouseWheelCallback, mouseWheelCallback,

   -- * Mouse movement callbacks
   MotionCallback, motionCallback, passiveMotionCallback,
   Crossing(..), CrossingCallback, crossingCallback,

   -- * Spaceball callback
   SpaceballMotion, SpaceballRotation, ButtonIndex, SpaceballInput(..),
   SpaceballCallback, spaceballCallback,

   -- * Dial & button box callback
   DialAndButtonBoxInput(..), DialIndex,
   DialAndButtonBoxCallback, dialAndButtonBoxCallback,

   -- * Tablet callback
   TabletPosition(..), TabletInput(..), TabletCallback, tabletCallback,

   -- * Joystick callback
   JoystickButtons(..), JoystickPosition(..),
   JoystickCallback, joystickCallback
) where

import Data.Bits hiding ( shift )
import Data.Char
import Data.Maybe
import Data.StateVar
import Foreign.C.Types
import Graphics.Rendering.OpenGL ( Position(..), Size(..) )
import Graphics.UI.GLUT.Callbacks.Registration
import Graphics.UI.GLUT.Raw
import Graphics.UI.GLUT.State
import Graphics.UI.GLUT.Types

--------------------------------------------------------------------------------

-- | A display callback

type DisplayCallback = IO ()

-- | Controls the display callback for the /current window./ When GLUT determines
-- that the normal plane for the window needs to be redisplayed, the display
-- callback for the window is called. Before the callback, the /current window/
-- is set to the window needing to be redisplayed and (if no overlay display
-- callback is registered) the /layer in use/ is set to the normal plane. The
-- entire normal plane region should be redisplayed in response to the callback
-- (this includes ancillary buffers if your program depends on their state).
--
-- GLUT determines when the display callback should be triggered based on the
-- window\'s redisplay state. The redisplay state for a window can be either set
-- explicitly by calling 'Graphics.UI.GLUT.Window.postRedisplay' or implicitly
-- as the result of window damage reported by the window system. Multiple posted
-- redisplays for a window are coalesced by GLUT to minimize the number of
-- display callbacks called.
--
-- When an overlay is established for a window, but there is no overlay display
-- callback registered, the display callback is used for redisplaying both the
-- overlay and normal plane (that is, it will be called if either the redisplay
-- state or overlay redisplay state is set). In this case, the /layer in use/ is
-- not implicitly changed on entry to the display callback.
--
-- See 'overlayDisplayCallback' to understand how distinct callbacks for the
-- overlay and normal plane of a window may be established.
--
-- When a window is created, no display callback exists for the window. It is
-- the responsibility of the programmer to install a display callback for the
-- window before the window is shown. A display callback must be registered for
-- any window that is shown. If a window becomes displayed without a display
-- callback being registered, a fatal error occurs. There is no way to
-- \"deregister\" a display callback (though another callback routine can always
-- be registered).
--
-- Upon return from the display callback, the normal damaged state of the window
-- (see 'Graphics.UI.GLUT.State.damaged') is cleared. If there is no overlay
-- display callback registered the overlay damaged state of the window (see
-- 'Graphics.UI.GLUT.State.damaged') is also cleared.

displayCallback :: SettableStateVar DisplayCallback
displayCallback = makeSettableStateVar $
   setCallback DisplayCB glutDisplayFunc makeDisplayFunc . Just

--------------------------------------------------------------------------------

-- | Controls the overlay display callback for the /current window./ The overlay
-- display callback is functionally the same as the window\'s display callback
-- except that the overlay display callback is used to redisplay the window\'s
-- overlay.
--
-- When GLUT determines that the overlay plane for the window needs to be
-- redisplayed, the overlay display callback for the window is called. Before
-- the callback, the /current window/ is set to the window needing to be
-- redisplayed and the /layer in use/ is set to the overlay. The entire overlay
-- region should be redisplayed in response to the callback (this includes
-- ancillary buffers if your program depends on their state).
--
-- GLUT determines when the overlay display callback should be triggered based
-- on the window\'s overlay redisplay state. The overlay redisplay state for a
-- window can be either set explicitly by calling
-- 'Graphics.UI.GLUT.Overlay.postOverlayRedisplay' or implicitly as the result
-- of window damage reported by the window system. Multiple posted overlay
-- redisplays for a window are coalesced by GLUT to minimize the number of
-- overlay display callbacks called.
--
-- Upon return from the overlay display callback, the overlay damaged state of
-- the window (see 'Graphics.UI.GLUT.State.damaged') is cleared.
--
-- Initially there is no overlay display callback registered when an overlay is
-- established. See 'displayCallback' to understand how the display callback
-- alone is used if an overlay display callback is not registered.

overlayDisplayCallback :: SettableStateVar (Maybe DisplayCallback)
overlayDisplayCallback = makeSettableStateVar $
   setCallback OverlayDisplayCB glutOverlayDisplayFunc makeOverlayDisplayFunc

--------------------------------------------------------------------------------

-- | A reshape callback

type ReshapeCallback = Size -> IO ()

-- | Controls the reshape callback for the /current window./ The reshape callback
-- is triggered when a window is reshaped. A reshape callback is also triggered
-- immediately before a window\'s first display callback after a window is
-- created or whenever an overlay for the window is established. The parameter
-- of the callback specifies the new window size in pixels. Before the callback,
-- the /current window/ is set to the window that has been reshaped.
--
-- If a reshape callback is not registered for a window or 'reshapeCallback' is
-- set to 'Nothing' (to deregister a previously registered callback), the
-- default reshape callback is used. This default callback will simply call
--
-- @
-- 'Graphics.Rendering.OpenGL.GL.CoordTrans.viewport' ('Graphics.Rendering.OpenGL.GL.CoordTrans.Position' 0 0) ('Graphics.Rendering.OpenGL.GL.CoordTrans.Size' /width/ /height/)
-- @
--
-- on the normal plane (and on the overlay if one exists).
--
-- If an overlay is established for the window, a single reshape callback is
-- generated. It is the callback\'s responsibility to update both the normal
-- plane and overlay for the window (changing the layer in use as necessary).
--
-- When a top-level window is reshaped, subwindows are not reshaped. It is up to
-- the GLUT program to manage the size and positions of subwindows within a
-- top-level window. Still, reshape callbacks will be triggered for subwindows
-- when their size is changed using 'Graphics.UI.GLUT.Window.windowSize'.

reshapeCallback :: SettableStateVar (Maybe ReshapeCallback)
reshapeCallback = makeSettableStateVar $
   setCallback ReshapeCB glutReshapeFunc (makeReshapeFunc . unmarshal)
   where unmarshal cb w h = cb (Size (fromIntegral w) (fromIntegral h))

--------------------------------------------------------------------------------

-- | The visibility state of the /current window/

data Visibility
   = NotVisible -- ^ No part of the /current window/ is visible, i.e., until the
                --   window\'s visibility changes, all further rendering to the
                --   window is discarded.
   | Visible    -- ^ The /current window/ is totally or partially visible. GLUT
                --   considers a window visible if any pixel of the window is
                --   visible or any pixel of any descendant window is visible on
                --   the screen.
   deriving ( Eq, Ord, Show )

unmarshalVisibility :: CInt -> Visibility
unmarshalVisibility x
   | x == glut_NOT_VISIBLE = NotVisible
   | x == glut_VISIBLE = Visible
   | otherwise = error ("unmarshalVisibility: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- | A visibility callback

type VisibilityCallback = Visibility -> IO ()

-- | Controls the visibility callback for the /current window./ The visibility
-- callback for a window is called when the visibility of a window changes.
--
-- If the visibility callback for a window is disabled and later re-enabled, the
-- visibility status of the window is undefined; any change in window visibility
-- will be reported, that is if you disable a visibility callback and re-enable
-- the callback, you are guaranteed the next visibility change will be reported.
--
-- Note that you can either use 'visibilityCallback' or 'windowStateCallback',
-- but not both, because the former is implemented via the latter.

visibilityCallback :: SettableStateVar (Maybe VisibilityCallback)
visibilityCallback = makeSettableStateVar $
   setCallback VisibilityCB glutVisibilityFunc
               (makeVisibilityFunc . unmarshal)
   where unmarshal cb  = cb . unmarshalVisibility

--------------------------------------------------------------------------------

-- | The window state of the /current window/

data WindowState
   = Unmapped          -- ^ The /current window/ is unmapped.
   | FullyRetained     -- ^ The /current window/ is unobscured.
   | PartiallyRetained -- ^ The /current window/ is partially obscured.
   | FullyCovered      -- ^ The /current window/ is fully obscured.
   deriving ( Eq, Ord, Show )

unmarshalWindowState :: CInt -> WindowState
unmarshalWindowState x
   | x == glut_HIDDEN = Unmapped
   | x == glut_FULLY_RETAINED = FullyRetained
   | x == glut_PARTIALLY_RETAINED = PartiallyRetained
   | x == glut_FULLY_COVERED = FullyCovered
   | otherwise = error ("unmarshalWindowState: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- | A window state callback

type WindowStateCallback = WindowState -> IO ()

-- | Controls the window state callback for the
-- /current window./ The window state callback for a window is called when the
-- window state of a window changes.
--
-- If the window state callback for a window is disabled and later re-enabled,
-- the window state state of the window is undefined; any change in the window
-- state will be reported, that is if you disable a window state callback and
-- re-enable the callback, you are guaranteed the next window state change will
-- be reported.
--
-- Note that you can either use 'visibilityCallback' or 'windowStateCallback',
-- but not both, because the former is implemented via the latter.

windowStateCallback :: SettableStateVar (Maybe WindowStateCallback)
windowStateCallback = makeSettableStateVar $
   setCallback WindowStatusCB glutWindowStatusFunc
               (makeWindowStatusFunc . unmarshal)
   where unmarshal cb  = cb . unmarshalWindowState

--------------------------------------------------------------------------------

type CloseCallback = IO ()

closeCallback :: SettableStateVar (Maybe CloseCallback)
closeCallback = makeSettableStateVar $
   setCallback CloseCB glutCloseFunc makeCloseFunc

--------------------------------------------------------------------------------

-- | A keyboard callback
type KeyboardCallback = Char -> Position -> IO ()

setKeyboardCallback :: Maybe KeyboardCallback -> IO ()
setKeyboardCallback =
   setCallback KeyboardCB glutKeyboardFunc (makeKeyboardFunc . unmarshal)
   where unmarshal cb c x y = cb (chr (fromIntegral c))
                                 (Position (fromIntegral x) (fromIntegral y))

-- | Controls the keyboard callback for the /current window/. This is
-- activated only when a key is pressed.
keyboardCallback :: SettableStateVar (Maybe KeyboardCallback)
keyboardCallback = makeSettableStateVar setKeyboardCallback

--------------------------------------------------------------------------------

setKeyboardUpCallback :: Maybe KeyboardCallback -> IO ()
setKeyboardUpCallback =
   setCallback KeyboardUpCB glutKeyboardUpFunc
               (makeKeyboardUpFunc . unmarshal)
   where unmarshal cb c x y = cb (chr (fromIntegral c))
                                 (Position (fromIntegral x) (fromIntegral y))

-- | Controls the keyboard callback for the /current window/. This is
-- activated only when a key is released.
keyboardUpCallback :: SettableStateVar (Maybe KeyboardCallback)
keyboardUpCallback = makeSettableStateVar setKeyboardUpCallback
--------------------------------------------------------------------------------

-- | Special keys

data SpecialKey
   = KeyF1
   | KeyF2
   | KeyF3
   | KeyF4
   | KeyF5
   | KeyF6
   | KeyF7
   | KeyF8
   | KeyF9
   | KeyF10
   | KeyF11
   | KeyF12
   | KeyLeft
   | KeyUp
   | KeyRight
   | KeyDown
   | KeyPageUp
   | KeyPageDown
   | KeyHome
   | KeyEnd
   | KeyInsert
   | KeyNumLock
   | KeyBegin
   | KeyDelete
   | KeyUnknown Int
   deriving ( Eq, Ord, Show )

unmarshalSpecialKey :: CInt -> SpecialKey
unmarshalSpecialKey x
   | x == glut_KEY_F1 = KeyF1
   | x == glut_KEY_F2 = KeyF2
   | x == glut_KEY_F3 = KeyF3
   | x == glut_KEY_F4 = KeyF4
   | x == glut_KEY_F5 = KeyF5
   | x == glut_KEY_F6 = KeyF6
   | x == glut_KEY_F7 = KeyF7
   | x == glut_KEY_F8 = KeyF8
   | x == glut_KEY_F9 = KeyF9
   | x == glut_KEY_F10 = KeyF10
   | x == glut_KEY_F11 = KeyF11
   | x == glut_KEY_F12 = KeyF12
   | x == glut_KEY_LEFT = KeyLeft
   | x == glut_KEY_UP = KeyUp
   | x == glut_KEY_RIGHT = KeyRight
   | x == glut_KEY_DOWN = KeyDown
   | x == glut_KEY_PAGE_UP = KeyPageUp
   | x == glut_KEY_PAGE_DOWN = KeyPageDown
   | x == glut_KEY_HOME = KeyHome
   | x == glut_KEY_END = KeyEnd
   | x == glut_KEY_INSERT = KeyInsert
   | x == glut_KEY_NUM_LOCK = KeyNumLock
   | x == glut_KEY_BEGIN = KeyBegin
   | x == glut_KEY_DELETE = KeyDelete
   | otherwise = KeyUnknown (fromIntegral x)

--------------------------------------------------------------------------------

-- | A special key callback
type SpecialCallback = SpecialKey -> Position -> IO ()

setSpecialCallback :: Maybe SpecialCallback -> IO ()
setSpecialCallback =
   setCallback SpecialCB glutSpecialFunc (makeSpecialFunc . unmarshal)
   where unmarshal cb k x y = cb (unmarshalSpecialKey k)
                                 (Position (fromIntegral x) (fromIntegral y))

-- | Controls the special key callback for the /current window/. This is
-- activated only when a special key is pressed.
specialCallback :: SettableStateVar (Maybe SpecialCallback)
specialCallback = makeSettableStateVar setSpecialCallback
--------------------------------------------------------------------------------

setSpecialUpCallback :: Maybe SpecialCallback -> IO ()
setSpecialUpCallback =
   setCallback SpecialUpCB glutSpecialUpFunc (makeSpecialUpFunc . unmarshal)
   where unmarshal cb k x y = cb (unmarshalSpecialKey k)
                                 (Position (fromIntegral x) (fromIntegral y))

-- | Controls the special key callback for the /current window/. This is
-- activated only when a special key is released.
specialUpCallback :: SettableStateVar (Maybe SpecialCallback)
specialUpCallback = makeSettableStateVar setSpecialUpCallback
--------------------------------------------------------------------------------

-- | The current state of a key or button

data KeyState
   = Down
   | Up
   deriving ( Eq, Ord, Show )

unmarshalKeyState :: CInt -> KeyState
unmarshalKeyState x
   | x == glut_DOWN = Down
   | x == glut_UP = Up
   | otherwise = error ("unmarshalKeyState: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- | A mouse callback
type MouseCallback = MouseButton -> KeyState -> Position -> IO ()

setMouseCallback :: Maybe MouseCallback -> IO ()
setMouseCallback =
   setCallback MouseCB glutMouseFunc (makeMouseFunc . unmarshal)
   where unmarshal cb b s x y = cb (unmarshalMouseButton b)
                                   (unmarshalKeyState s)
                                   (Position (fromIntegral x) (fromIntegral y))

-- | Controls the mouse callback for the /current window/.
mouseCallback :: SettableStateVar (Maybe MouseCallback)
mouseCallback = makeSettableStateVar setMouseCallback
--------------------------------------------------------------------------------

-- | The state of the keyboard modifiers

data Modifiers = Modifiers { shift, ctrl, alt :: KeyState }
   deriving ( Eq, Ord, Show )

-- Could use fromBitfield + Enum/Bounded instances + marshalModifier instead...
unmarshalModifiers :: CInt -> Modifiers
unmarshalModifiers m = Modifiers {
   shift = if (m .&. glut_ACTIVE_SHIFT) /= 0 then Down else Up,
   ctrl  = if (m .&. glut_ACTIVE_CTRL ) /= 0 then Down else Up,
   alt   = if (m .&. glut_ACTIVE_ALT  ) /= 0 then Down else Up }

getModifiers :: IO Modifiers
getModifiers = fmap unmarshalModifiers glutGetModifiers

--------------------------------------------------------------------------------

-- | A generalized view of keys

data Key
   = Char Char
   | SpecialKey SpecialKey
   | MouseButton MouseButton
   deriving ( Eq, Ord, Show )

-- | A keyboard\/mouse callback

type KeyboardMouseCallback =
   Key -> KeyState -> Modifiers -> Position -> IO ()

-- | Controls the keyboard\/mouse callback for the /current window./ The
-- keyboard\/mouse callback for a window is called when the state of a key or
-- mouse button changes. The callback parameters indicate the new state of the
-- key\/button, the state of the keyboard modifiers, and the mouse location in
-- window relative coordinates.
--
-- Note that this is a convenience function that should not ordinarily be used
-- in conjunction with `keyboardCallback`, `keyboardUpCallback`,
-- `specialCallback`, `specialUpCallback`, or `mouseCallback`.

keyboardMouseCallback :: SettableStateVar (Maybe KeyboardMouseCallback)
keyboardMouseCallback = makeSettableStateVar setKeyboardMouseCallback

setKeyboardMouseCallback :: Maybe KeyboardMouseCallback -> IO ()
setKeyboardMouseCallback Nothing = do
   setKeyboardCallback   Nothing
   setKeyboardUpCallback Nothing
   setSpecialCallback    Nothing
   setSpecialUpCallback  Nothing
   setMouseCallback      Nothing
setKeyboardMouseCallback (Just cb) = do
   setKeyboardCallback   (Just (\c   p -> do m <- getModifiers
                                             cb (Char        c) Down m p))
   setKeyboardUpCallback (Just (\c   p -> do m <- getModifiers
                                             cb (Char        c) Up   m p))
   setSpecialCallback    (Just (\s   p -> do m <- getModifiers
                                             cb (SpecialKey  s) Down m p))
   setSpecialUpCallback  (Just (\s   p -> do m <- getModifiers
                                             cb (SpecialKey  s) Up   m p))
   setMouseCallback      (Just (\b s p -> do m <- getModifiers
                                             cb (MouseButton b) s    m p))

--------------------------------------------------------------------------------

type WheelNumber = Int

type WheelDirection = Int

type MouseWheelCallback = WheelNumber -> WheelDirection -> Position -> IO ()

-- | (/freeglut only/) Controls the mouse wheel callback for the
-- /current window./ The mouse wheel callback for a window is called when a
-- mouse wheel is used and the wheel number is greater than or equal to
-- 'Graphics.UI.GLUT.State.numMouseButtons'.

mouseWheelCallback :: SettableStateVar (Maybe MouseWheelCallback)
mouseWheelCallback = makeSettableStateVar $
   setCallback MouseWheelCB glutMouseWheelFunc (makeMouseWheelFunc . unmarshal)
   where unmarshal cb n d x y = cb (fromIntegral n) (fromIntegral d)
                                   (Position (fromIntegral x) (fromIntegral y))

--------------------------------------------------------------------------------

-- | A motion callback

type MotionCallback = Position -> IO ()

-- | Controls the motion callback for the /current window./ The motion callback
-- for a window is called when the mouse moves within the window while one or
-- more mouse buttons are pressed. The callback parameter indicates the mouse
-- location in window relative coordinates.

motionCallback :: SettableStateVar (Maybe MotionCallback)
motionCallback = makeSettableStateVar $
   setCallback MotionCB glutMotionFunc (makeMotionFunc . unmarshal)
   where unmarshal cb x y = cb (Position (fromIntegral x) (fromIntegral y))

--------------------------------------------------------------------------------

-- | Controls the passive motion callback for the /current window./ The passive
-- motion callback for a window is called when the mouse moves within the window
-- while /no/ mouse buttons are pressed. The callback parameter indicates the
-- mouse location in window relative coordinates.

passiveMotionCallback :: SettableStateVar (Maybe MotionCallback)
passiveMotionCallback = makeSettableStateVar $
   setCallback PassiveMotionCB glutPassiveMotionFunc
               (makePassiveMotionFunc . unmarshal)
   where unmarshal cb x y = cb (Position (fromIntegral x) (fromIntegral y))

--------------------------------------------------------------------------------

-- | The relation between the mouse pointer and the /current window/ has
-- changed.

data Crossing
   = WindowLeft    -- ^ The mouse pointer has left the /current window./
   | WindowEntered -- ^ The mouse pointer has entered the /current window./
   deriving ( Eq, Ord, Show )

unmarshalCrossing :: CInt -> Crossing
unmarshalCrossing x
   | x == glut_LEFT = WindowLeft
   | x == glut_ENTERED = WindowEntered
   | otherwise = error ("unmarshalCrossing: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- | An enter\/leave callback

type CrossingCallback = Crossing -> IO ()

-- | Controls the mouse enter\/leave callback for the /current window./ Note
-- that some window systems may not generate accurate enter\/leave callbacks.
--
-- /X Implementation Notes:/ An X implementation of GLUT should generate
-- accurate enter\/leave callbacks.

crossingCallback :: SettableStateVar (Maybe CrossingCallback)
crossingCallback = makeSettableStateVar $
   setCallback CrossingCB glutEntryFunc (makeEntryFunc . unmarshal)
   where unmarshal cb = cb . unmarshalCrossing

--------------------------------------------------------------------------------

-- | Translation of the Spaceball along one axis, normalized to be in the range
-- of -1000 to +1000 inclusive

type SpaceballMotion = Int

-- | Rotation of the Spaceball along one axis, normalized to be in the range
-- of -1800 .. +1800 inclusive

type SpaceballRotation = Int

-- | The index of a specific buttons of an input device.

type ButtonIndex = Int

-- | The state of the Spaceball has changed.

data SpaceballInput
   = SpaceballMotion   SpaceballMotion SpaceballMotion SpaceballMotion
   | SpaceballRotation SpaceballRotation SpaceballRotation SpaceballRotation
   | SpaceballButton   ButtonIndex KeyState
   deriving ( Eq, Ord, Show )

-- | A SpaceballButton callback

type SpaceballCallback = SpaceballInput -> IO ()

-- | Controls the Spaceball callback for the /current window./ The Spaceball
-- callback for a window is called when the window has Spaceball input focus
-- (normally, when the mouse is in the window) and the user generates Spaceball
-- translations, rotations, or button presses. The number of available Spaceball
-- buttons can be determined with 'Graphics.UI.GLUT.State.numSpaceballButtons'.
--
-- Registering a Spaceball callback when a Spaceball device is not available has
-- no effect and is not an error. In this case, no Spaceball callbacks will be
-- generated.

spaceballCallback :: SettableStateVar (Maybe SpaceballCallback)
spaceballCallback = makeSettableStateVar setSpaceballCallback

setSpaceballCallback :: Maybe SpaceballCallback -> IO ()
setSpaceballCallback Nothing = do
   setSpaceballMotionCallback   Nothing
   setSpaceballRotationCallback Nothing
   setSpaceballButtonCallback   Nothing
setSpaceballCallback (Just cb) = do
   setSpaceballMotionCallback   (Just (\x y z -> cb (SpaceballMotion   x y z)))
   setSpaceballRotationCallback (Just (\x y z -> cb (SpaceballRotation x y z)))
   setSpaceballButtonCallback   (Just (\b s   -> cb (SpaceballButton   b s)))

--------------------------------------------------------------------------------

type SpaceballMotionCallback =
   SpaceballMotion -> SpaceballMotion -> SpaceballMotion -> IO ()

setSpaceballMotionCallback :: Maybe SpaceballMotionCallback -> IO ()
setSpaceballMotionCallback =
   setCallback SpaceballMotionCB glutSpaceballMotionFunc
               (makeSpaceballMotionFunc . unmarshal)
   where unmarshal cb x y z =
            cb (fromIntegral x) (fromIntegral y) (fromIntegral z)

--------------------------------------------------------------------------------

type SpaceballRotationCallback =
   SpaceballRotation -> SpaceballRotation -> SpaceballRotation -> IO ()

setSpaceballRotationCallback :: Maybe SpaceballRotationCallback -> IO ()
setSpaceballRotationCallback =
   setCallback SpaceballRotateCB glutSpaceballRotateFunc
               (makeSpaceballRotateFunc . unmarshal)
   where unmarshal cb x y z =
            cb (fromIntegral x) (fromIntegral y) (fromIntegral z)

--------------------------------------------------------------------------------

type SpaceballButtonCallback = ButtonIndex -> KeyState -> IO ()

setSpaceballButtonCallback :: Maybe SpaceballButtonCallback -> IO ()
setSpaceballButtonCallback =
   setCallback SpaceballButtonCB glutSpaceballButtonFunc
               (makeSpaceballButtonFunc . unmarshal)
   where unmarshal cb b s = cb (fromIntegral b) (unmarshalKeyState s)

--------------------------------------------------------------------------------

-- | The index of a specific dial of a dial and button box.

type DialIndex = Int

-- | The dial & button box state has changed.

data DialAndButtonBoxInput
   = DialAndButtonBoxButton ButtonIndex KeyState
   | DialAndButtonBoxDial   DialIndex Int
   deriving ( Eq, Ord, Show )

-- | A dial & button box callback

type DialAndButtonBoxCallback = DialAndButtonBoxInput -> IO ()

-- | Controls the dial & button box callback for the /current window./ The dial
-- & button box button callback for a window is called when the window has dial
-- & button box input focus (normally, when the mouse is in the window) and the
-- user generates dial & button box button presses or dial changes. The number
-- of available dial & button box buttons and dials can be determined with
-- 'Graphics.UI.GLUT.State.numDialsAndButtons'.
--
-- Registering a dial & button box callback when a dial & button box device is
-- not available is ineffectual and not an error. In this case, no dial & button
-- box button will be generated.

dialAndButtonBoxCallback :: SettableStateVar (Maybe DialAndButtonBoxCallback)
dialAndButtonBoxCallback = makeSettableStateVar setDialAndButtonBoxCallback

setDialAndButtonBoxCallback :: Maybe DialAndButtonBoxCallback -> IO ()
setDialAndButtonBoxCallback Nothing = do
   setButtonBoxCallback Nothing
   setDialsCallback     Nothing
setDialAndButtonBoxCallback (Just cb) = do
   setButtonBoxCallback (Just (\b s -> cb (DialAndButtonBoxButton b s)))
   setDialsCallback     (Just (\d x -> cb (DialAndButtonBoxDial   d x)))

--------------------------------------------------------------------------------

type ButtonBoxCallback = ButtonIndex -> KeyState -> IO ()

setButtonBoxCallback :: Maybe ButtonBoxCallback -> IO ()
setButtonBoxCallback =
   setCallback ButtonBoxCB glutButtonBoxFunc (makeButtonBoxFunc . unmarshal)
   where unmarshal cb b s = cb (fromIntegral b) (unmarshalKeyState s)

--------------------------------------------------------------------------------

type DialsCallback = DialIndex -> Int -> IO ()

setDialsCallback :: Maybe DialsCallback -> IO ()
setDialsCallback =
    setCallback DialsCB glutDialsFunc (makeDialsFunc . unmarshal)
    where unmarshal cb d x = cb (fromIntegral d) (fromIntegral x)

--------------------------------------------------------------------------------

-- | Absolute tablet position, with coordinates normalized to be in the range of
-- 0 to 2000 inclusive

data TabletPosition = TabletPosition Int Int
   deriving ( Eq, Ord, Show )

-- | The table state has changed.

data TabletInput
   = TabletMotion
   | TabletButton ButtonIndex KeyState
   deriving ( Eq, Ord, Show )

-- | A tablet callback

type TabletCallback = TabletInput -> TabletPosition -> IO ()

-- | Controls the tablet callback for the /current window./ The tablet callback
-- for a window is called when the window has tablet input focus (normally, when
-- the mouse is in the window) and the user generates tablet motion or button
-- presses. The number of available tablet buttons can be determined with
-- 'Graphics.UI.GLUT.State.numTabletButtons'.
--
-- Registering a tablet callback when a tablet device is not available is
-- ineffectual and not an error. In this case, no tablet callbacks will be
-- generated.

tabletCallback :: SettableStateVar (Maybe TabletCallback)
tabletCallback = makeSettableStateVar setTabletCallback

setTabletCallback :: Maybe TabletCallback -> IO ()
setTabletCallback Nothing = do
   setTabletMotionCallback Nothing
   setTabletButtonCallback Nothing
setTabletCallback (Just cb) = do
   setTabletMotionCallback (Just (\p     -> cb TabletMotion       p))
   setTabletButtonCallback (Just (\b s p -> cb (TabletButton b s) p))

--------------------------------------------------------------------------------

type TabletMotionCallback = TabletPosition -> IO ()

setTabletMotionCallback :: Maybe TabletMotionCallback -> IO ()
setTabletMotionCallback =
    setCallback TabletMotionCB glutTabletMotionFunc
                (makeTabletMotionFunc . unmarshal)
    where unmarshal cb x y =
             cb (TabletPosition (fromIntegral x) (fromIntegral y))

--------------------------------------------------------------------------------

type TabletButtonCallback = ButtonIndex -> KeyState -> TabletPosition -> IO ()

setTabletButtonCallback :: Maybe TabletButtonCallback -> IO ()
setTabletButtonCallback =
    setCallback TabletButtonCB glutTabletButtonFunc
                (makeTabletButtonFunc . unmarshal)
    where unmarshal cb b s x y =
             cb (fromIntegral b) (unmarshalKeyState s)
                (TabletPosition (fromIntegral x) (fromIntegral y))

--------------------------------------------------------------------------------

-- | The state of the joystick buttons

data JoystickButtons = JoystickButtons {
   joystickButtonA, joystickButtonB,
   joystickButtonC, joystickButtonD :: KeyState }
   deriving ( Eq, Ord, Show )

-- Could use fromBitfield + Enum/Bounded instances + unmarshalJoystickButton
-- instead...
unmarshalJoystickButtons :: CUInt -> JoystickButtons
unmarshalJoystickButtons m = JoystickButtons {
   joystickButtonA = if (m .&. glut_JOYSTICK_BUTTON_A) /= 0 then Down else Up,
   joystickButtonB = if (m .&. glut_JOYSTICK_BUTTON_B) /= 0 then Down else Up,
   joystickButtonC = if (m .&. glut_JOYSTICK_BUTTON_C) /= 0 then Down else Up,
   joystickButtonD = if (m .&. glut_JOYSTICK_BUTTON_D) /= 0 then Down else Up }

--------------------------------------------------------------------------------

-- | Absolute joystick position, with coordinates normalized to be in the range
-- of -1000 to 1000 inclusive. The signs of the three axes mean the following:
--
-- * negative = left, positive = right
--
-- * negative = towards player, positive = away
--
-- * if available (e.g. rudder): negative = down, positive = up

data JoystickPosition = JoystickPosition Int Int Int
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | A joystick callback

type JoystickCallback = JoystickButtons -> JoystickPosition -> IO ()

-- | Controls the joystick callback for the /current window./ The joystick
-- callback is called either due to polling of the joystick at the uniform timer
-- interval specified (if > 0) or in response to an explicit call of
-- 'Graphics.UI.GLUT.DeviceControl.forceJoystickCallback'.
--
-- /X Implementation Notes:/ Currently GLUT has no joystick support for X11.

-- joystickCallback :: SettableStateVar (Maybe JoystickCallback, PollRate)
joystickCallback :: SettableStateVar (Maybe (JoystickCallback, PollRate))
joystickCallback =
   makeSettableStateVar $ \maybeCBAndRate ->
      setCallback JoystickCB
                  (\f -> glutJoystickFunc f (fromIntegral (snd (fromJust maybeCBAndRate))))
                  (makeJoystickFunc . unmarshal)
                  (fmap fst maybeCBAndRate)
    where unmarshal cb b x y z = cb (unmarshalJoystickButtons b)
                                    (JoystickPosition (fromIntegral x)
                                                      (fromIntegral y)
                                                      (fromIntegral z))
