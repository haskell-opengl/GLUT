--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Callbacks.Window
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Callbacks.Window (
   -- * Redisplay callbacks
   DisplayCallback, displayCallback, overlayDisplayCallback,

   -- * Reshape callback
   ReshapeCallback, reshapeCallback,

   -- * Callback for visibility changes
   Visibility(..), VisibilityCallback, visibilityCallback,

   -- * Keyboard and mouse input callback
   Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..),
   KeyboardMouseCallback, keyboardMouseCallback,

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

import Control.Monad ( liftM )
import Data.Bits ( Bits((.&.)) )
import Data.Char ( chr )
import Data.Maybe ( fromJust )
import Foreign.C.Types ( CInt, CUInt, CUChar )
import Foreign.Ptr ( FunPtr )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.StateVar (
   SettableStateVar, makeSettableStateVar )
import Graphics.UI.GLUT.Callbacks.Registration ( CallbackType(..), setCallback )
import Graphics.UI.GLUT.Constants (
   glut_NOT_VISIBLE, glut_VISIBLE,
   glut_KEY_F1, glut_KEY_F2, glut_KEY_F3, glut_KEY_F4, glut_KEY_F5, glut_KEY_F6,
   glut_KEY_F7, glut_KEY_F8, glut_KEY_F9, glut_KEY_F10, glut_KEY_F11,
   glut_KEY_F12, glut_KEY_LEFT, glut_KEY_UP, glut_KEY_RIGHT, glut_KEY_DOWN,
   glut_KEY_PAGE_UP, glut_KEY_PAGE_DOWN, glut_KEY_HOME, glut_KEY_END,
   glut_KEY_INSERT,
   glut_DOWN, glut_UP,
   glut_ACTIVE_SHIFT, glut_ACTIVE_CTRL, glut_ACTIVE_ALT,
   glut_LEFT, glut_ENTERED,
   glut_JOYSTICK_BUTTON_A, glut_JOYSTICK_BUTTON_B,
   glut_JOYSTICK_BUTTON_C, glut_JOYSTICK_BUTTON_D )
import Graphics.UI.GLUT.State ( PollRate )
import Graphics.UI.GLUT.Types ( MouseButton(..), unmarshalMouseButton )

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
   setCallback DisplayCB glutDisplayFunc makeDisplayCallback . Just

foreign import ccall "wrapper" makeDisplayCallback ::
   DisplayCallback -> IO (FunPtr DisplayCallback)

foreign import CALLCONV unsafe "glutDisplayFunc" glutDisplayFunc ::
   FunPtr DisplayCallback -> IO ()

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
   setCallback OverlayDisplayCB glutOverlayDisplayFunc makeDisplayCallback

foreign import CALLCONV unsafe "glutOverlayDisplayFunc" glutOverlayDisplayFunc
   :: FunPtr DisplayCallback -> IO ()

--------------------------------------------------------------------------------

-- | A reshape callback

type ReshapeCallback = Size -> IO ()

type ReshapeCallback' = CInt -> CInt -> IO ()

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
-- 'viewport' ('Graphics.Rendering.OpenGL.GL.CoordTrans.Position' 0 0) ('Graphics.Rendering.OpenGL.GL.CoordTrans.Size' /width/ /height/)
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
   setCallback ReshapeCB glutReshapeFunc (makeReshapeCallback . unmarshal)
   where unmarshal cb w h = cb (Size (fromIntegral w) (fromIntegral h))

foreign import ccall "wrapper" makeReshapeCallback ::
   ReshapeCallback' -> IO (FunPtr ReshapeCallback')

foreign import CALLCONV unsafe "glutReshapeFunc" glutReshapeFunc ::
   FunPtr ReshapeCallback' -> IO ()

--------------------------------------------------------------------------------

-- | The visibility state of the /current window/

data Visibility
   = NotVisible -- ^ The /current window/ is totally or partially visible. GLUT
                --   considers a window visible if any pixel of the window is
                --   visible or any pixel of any descendant window is visible on
                --   the screen.
   | Visible    -- ^ No part of the /current window/ is visible, i.e., until the
                --   window\'s visibility changes, all further rendering to the
                --   window is discarded.
   deriving ( Eq, Ord, Show )

unmarshalVisibility :: CInt -> Visibility
unmarshalVisibility v
   | v == glut_NOT_VISIBLE = NotVisible
   | v == glut_VISIBLE     = Visible
   | otherwise = error "unmarshalVisibility"

--------------------------------------------------------------------------------

-- | A visibilty callback

type VisibilityCallback = Visibility -> IO ()

type VisibilityCallback' = CInt -> IO ()

-- | Controls the visibility callback for the /current window./ The visibility
-- callback for a window is called when the visibility of a window changes.
-- 
-- If the visibility callback for a window is disabled and later re-enabled, the
-- visibility status of the window is undefined; any change in window visibility
-- will be reported, that is if you disable a visibility callback and re-enable
-- the callback, you are guaranteed the next visibility change will be reported.

visibilityCallback :: SettableStateVar (Maybe VisibilityCallback)
visibilityCallback = makeSettableStateVar $
   setCallback VisibilityCB glutVisibilityFunc
               (makeVisibilityCallback . unmarshal)
   where unmarshal cb  = cb . unmarshalVisibility

foreign import ccall "wrapper" makeVisibilityCallback ::
   VisibilityCallback' -> IO (FunPtr VisibilityCallback')

foreign import CALLCONV unsafe "glutVisibilityFunc" glutVisibilityFunc ::
   FunPtr VisibilityCallback' -> IO ()

--------------------------------------------------------------------------------

type KeyboardCallback = Char -> Position -> IO ()

type KeyboardCallback' = CUChar -> CInt -> CInt -> IO ()

setKeyboardCallback :: Maybe KeyboardCallback -> IO ()
setKeyboardCallback =
   setCallback KeyboardCB glutKeyboardFunc (makeKeyboardCallback . unmarshal)
   where unmarshal cb c x y = cb (chr (fromIntegral c))
                                 (Position (fromIntegral x) (fromIntegral y))

foreign import ccall "wrapper" makeKeyboardCallback ::
   KeyboardCallback' -> IO (FunPtr KeyboardCallback')

foreign import CALLCONV unsafe "glutKeyboardFunc" glutKeyboardFunc ::
   FunPtr KeyboardCallback' -> IO ()

--------------------------------------------------------------------------------

setKeyboardUpCallback :: Maybe KeyboardCallback -> IO ()
setKeyboardUpCallback =
   setCallback KeyboardUpCB glutKeyboardUpFunc
               (makeKeyboardCallback . unmarshal)
   where unmarshal cb c x y = cb (chr (fromIntegral c))
                                 (Position (fromIntegral x) (fromIntegral y))

foreign import CALLCONV unsafe "glutKeyboardUpFunc" glutKeyboardUpFunc ::
   FunPtr KeyboardCallback' -> IO ()

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
   deriving ( Eq, Ord, Show )

unmarshalSpecialKey :: CInt -> SpecialKey
unmarshalSpecialKey k
   | k == glut_KEY_F1        = KeyF1
   | k == glut_KEY_F2        = KeyF2
   | k == glut_KEY_F3        = KeyF3
   | k == glut_KEY_F4        = KeyF4
   | k == glut_KEY_F5        = KeyF5
   | k == glut_KEY_F6        = KeyF6
   | k == glut_KEY_F7        = KeyF7
   | k == glut_KEY_F8        = KeyF8
   | k == glut_KEY_F9        = KeyF9
   | k == glut_KEY_F10       = KeyF10
   | k == glut_KEY_F11       = KeyF11
   | k == glut_KEY_F12       = KeyF12
   | k == glut_KEY_LEFT      = KeyLeft
   | k == glut_KEY_UP        = KeyUp
   | k == glut_KEY_RIGHT     = KeyRight
   | k == glut_KEY_DOWN      = KeyDown
   | k == glut_KEY_PAGE_UP   = KeyPageUp
   | k == glut_KEY_PAGE_DOWN = KeyPageDown
   | k == glut_KEY_HOME      = KeyHome
   | k == glut_KEY_END       = KeyEnd
   | k == glut_KEY_INSERT    = KeyInsert
   | otherwise = error "unmarshalSpecialKey"

--------------------------------------------------------------------------------

type SpecialCallback = SpecialKey -> Position -> IO ()

type SpecialCallback' = CInt -> CInt -> CInt -> IO ()

setSpecialCallback :: Maybe SpecialCallback -> IO ()
setSpecialCallback =
   setCallback SpecialCB glutSpecialFunc (makeSpecialCallback . unmarshal)
   where unmarshal cb k x y = cb (unmarshalSpecialKey k)
                                 (Position (fromIntegral x) (fromIntegral y))

foreign import ccall "wrapper" makeSpecialCallback ::
   SpecialCallback' -> IO (FunPtr SpecialCallback')

foreign import CALLCONV unsafe "glutSpecialFunc" glutSpecialFunc ::
   FunPtr SpecialCallback' -> IO ()

--------------------------------------------------------------------------------

setSpecialUpCallback :: Maybe SpecialCallback -> IO ()
setSpecialUpCallback =
   setCallback SpecialUpCB glutSpecialUpFunc (makeSpecialCallback . unmarshal)
   where unmarshal cb k x y = cb (unmarshalSpecialKey k)
                                 (Position (fromIntegral x) (fromIntegral y))

foreign import CALLCONV unsafe "glutSpecialUpFunc" glutSpecialUpFunc ::
   FunPtr SpecialCallback' -> IO ()

--------------------------------------------------------------------------------

-- | The current state of a key or button

data KeyState
   = Down
   | Up
   deriving ( Eq, Ord, Show )

unmarshalKeyState :: CInt -> KeyState
unmarshalKeyState s
   | s == glut_DOWN = Down
   | s == glut_UP   = Up
   | otherwise = error "unmarshalKeyState"

--------------------------------------------------------------------------------

type MouseCallback = MouseButton -> KeyState -> Position -> IO ()

type MouseCallback' = CInt -> CInt -> CInt -> CInt -> IO ()

setMouseCallback :: Maybe MouseCallback -> IO ()
setMouseCallback =
   setCallback MouseCB glutMouseFunc (makeMouseCallback . unmarshal)
   where unmarshal cb b s x y = cb (unmarshalMouseButton b)
                                   (unmarshalKeyState s)
                                   (Position (fromIntegral x) (fromIntegral y))

foreign import ccall "wrapper" makeMouseCallback ::
   MouseCallback' -> IO (FunPtr MouseCallback')

foreign import CALLCONV unsafe "glutMouseFunc" glutMouseFunc ::
   FunPtr MouseCallback' -> IO ()

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
getModifiers = liftM unmarshalModifiers glutGetModifiers

foreign import CALLCONV unsafe "glutGetModifiers" glutGetModifiers :: IO CInt

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

-- | A motion callback

type MotionCallback = Position -> IO ()

type MotionCallback' = CInt -> CInt -> IO ()

-- | Controls the motion callback for the /current window./ The motion callback
-- for a window is called when the mouse moves within the window while one or
-- more mouse buttons are pressed. The callback parameter indicates the mouse
-- location in window relative coordinates.

motionCallback :: SettableStateVar (Maybe MotionCallback)
motionCallback = makeSettableStateVar $
   setCallback MotionCB glutMotionFunc (makeMotionCallback . unmarshal)
   where unmarshal cb x y = cb (Position (fromIntegral x) (fromIntegral y))

foreign import ccall "wrapper" makeMotionCallback ::
   MotionCallback' -> IO (FunPtr MotionCallback')

foreign import CALLCONV unsafe "glutMotionFunc" glutMotionFunc ::
   FunPtr MotionCallback' -> IO ()

--------------------------------------------------------------------------------

-- | Controls the passive motion callback for the /current window./ The passive
-- motion callback for a window is called when the mouse moves within the window
-- while /no/ mouse buttons are pressed. The callback parameter indicates the
-- mouse location in window relative coordinates.

passiveMotionCallback :: SettableStateVar (Maybe MotionCallback)
passiveMotionCallback = makeSettableStateVar $
   setCallback MotionCB glutPassiveMotionFunc (makeMotionCallback . unmarshal)
   where unmarshal cb x y = cb (Position (fromIntegral x) (fromIntegral y))

foreign import CALLCONV unsafe "glutPassiveMotionFunc" glutPassiveMotionFunc ::
   FunPtr MotionCallback' -> IO ()


--------------------------------------------------------------------------------

-- | The relation between the mouse pointer and the /current window/ has
-- changed.

data Crossing
   = WindowLeft    -- ^ The mouse pointer has left the /current window./
   | WindowEntered -- ^ The mouse pointer has entered the /current window./
   deriving ( Eq, Ord, Show )

unmarshalCrossing :: CInt -> Crossing
unmarshalCrossing c
   | c == glut_LEFT    = WindowLeft
   | c == glut_ENTERED = WindowEntered
   | otherwise = error "unmarshalCrossing"

--------------------------------------------------------------------------------

-- | An enter\/leave callback

type CrossingCallback = Crossing -> IO ()

type CrossingCallback' = CInt -> IO ()

-- | Controls the mouse enter\/leave callback for the /current window./ Note
-- that some window systems may not generate accurate enter\/leave callbacks.
--
-- /X Implementation Notes:/ An X implementation of GLUT should generate
-- accurate enter\/leave callbacks.

crossingCallback :: SettableStateVar (Maybe CrossingCallback)
crossingCallback = makeSettableStateVar $
   setCallback CrossingCB glutEntryFunc (makeCrossingCallback . unmarshal)
   where unmarshal cb = cb . unmarshalCrossing

foreign import ccall "wrapper" makeCrossingCallback ::
   CrossingCallback' -> IO (FunPtr CrossingCallback')

foreign import CALLCONV unsafe "glutEntryFunc" glutEntryFunc ::
   FunPtr CrossingCallback' -> IO ()

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
               (makeSpaceballMotionCallback . unmarshal)
   where unmarshal cb x y z =
            cb (fromIntegral x) (fromIntegral y) (fromIntegral z)

foreign import ccall "wrapper" makeSpaceballMotionCallback ::
   SpaceballMotionCallback -> IO (FunPtr SpaceballMotionCallback)

foreign import CALLCONV unsafe "glutSpaceballMotionFunc" glutSpaceballMotionFunc
   :: FunPtr SpaceballMotionCallback -> IO ()

--------------------------------------------------------------------------------

type SpaceballRotationCallback =
   SpaceballRotation -> SpaceballRotation -> SpaceballRotation -> IO ()

setSpaceballRotationCallback :: Maybe SpaceballRotationCallback -> IO ()
setSpaceballRotationCallback =
   setCallback SpaceballRotateCB glutSpaceballRotateFunc
               (makeSpaceballRotationCallback . unmarshal)
   where unmarshal cb x y z =
            cb (fromIntegral x) (fromIntegral y) (fromIntegral z)

foreign import ccall "wrapper" makeSpaceballRotationCallback ::
   SpaceballRotationCallback -> IO (FunPtr SpaceballRotationCallback)

foreign import CALLCONV unsafe "glutSpaceballRotateFunc" glutSpaceballRotateFunc
   :: FunPtr SpaceballRotationCallback -> IO ()

--------------------------------------------------------------------------------

type SpaceballButtonCallback = ButtonIndex -> KeyState -> IO ()

type SpaceballButtonCallback' = CInt -> CInt -> IO ()

setSpaceballButtonCallback :: Maybe SpaceballButtonCallback -> IO ()
setSpaceballButtonCallback =
   setCallback SpaceballButtonCB glutSpaceballButtonFunc
               (makeSpaceballButtonCallback . unmarshal)
   where unmarshal cb b s = cb (fromIntegral b) (unmarshalKeyState s)

foreign import ccall "wrapper" makeSpaceballButtonCallback ::
   SpaceballButtonCallback' -> IO (FunPtr SpaceballButtonCallback')

foreign import CALLCONV unsafe "glutSpaceballButtonFunc"
   glutSpaceballButtonFunc :: FunPtr SpaceballButtonCallback' -> IO ()

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

type ButtonBoxCallback' = CInt -> CInt -> IO ()

setButtonBoxCallback :: Maybe ButtonBoxCallback -> IO ()
setButtonBoxCallback =
   setCallback ButtonBoxCB glutButtonBoxFunc (makeButtonBoxFunc . unmarshal)
   where unmarshal cb b s = cb (fromIntegral b) (unmarshalKeyState s)

foreign import ccall "wrapper" makeButtonBoxFunc ::
   ButtonBoxCallback' -> IO (FunPtr ButtonBoxCallback')

foreign import CALLCONV unsafe "glutButtonBoxFunc" glutButtonBoxFunc ::
   FunPtr ButtonBoxCallback' -> IO ()

--------------------------------------------------------------------------------

type DialsCallback = DialIndex -> Int -> IO ()

type DialsCallback' = CInt -> CInt -> IO ()

setDialsCallback :: Maybe DialsCallback -> IO ()
setDialsCallback =
    setCallback DialsCB glutDialsFunc (makeDialsFunc . unmarshal)
    where unmarshal cb d x = cb (fromIntegral d) (fromIntegral x)

foreign import ccall "wrapper" makeDialsFunc ::
   DialsCallback -> IO (FunPtr DialsCallback')

foreign import CALLCONV unsafe "glutDialsFunc" glutDialsFunc ::
   FunPtr DialsCallback' -> IO ()

--------------------------------------------------------------------------------

-- | Absolute tablet position, with coordinates normalized to be in the range of
-- 0 to 2000 inclusive

data TabletPosition = TabletPosition Int Int

-- | The table state has changed.

data TabletInput
   = TabletMotion
   | TabletButton ButtonIndex KeyState

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

type TabletMotionCallback' = CInt -> CInt -> IO ()

setTabletMotionCallback :: Maybe TabletMotionCallback -> IO ()
setTabletMotionCallback =
    setCallback TabletMotionCB glutTabletMotionFunc
                (makeTabletMotionFunc . unmarshal)
    where unmarshal cb x y =
             cb (TabletPosition (fromIntegral x) (fromIntegral y))

foreign import ccall "wrapper" makeTabletMotionFunc ::
   TabletMotionCallback' -> IO (FunPtr TabletMotionCallback')

foreign import CALLCONV unsafe "glutTabletMotionFunc" glutTabletMotionFunc ::
   FunPtr TabletMotionCallback' -> IO ()

--------------------------------------------------------------------------------

type TabletButtonCallback = ButtonIndex -> KeyState -> TabletPosition -> IO ()

type TabletButtonCallback' = CInt -> CInt -> CInt -> CInt -> IO ()

setTabletButtonCallback :: Maybe TabletButtonCallback -> IO ()
setTabletButtonCallback =
    setCallback TabletButtonCB glutTabletButtonFunc
                (makeTabletButtonFunc . unmarshal)
    where unmarshal cb b s x y =
             cb (fromIntegral b) (unmarshalKeyState s)
                (TabletPosition (fromIntegral x) (fromIntegral y))

foreign import ccall "wrapper" makeTabletButtonFunc ::
   TabletButtonCallback' -> IO (FunPtr TabletButtonCallback')

foreign import CALLCONV unsafe "glutTabletButtonFunc" glutTabletButtonFunc ::
   FunPtr TabletButtonCallback' -> IO ()

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

type JoystickCallback' = CUInt -> CInt -> CInt -> CInt -> IO ()

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

foreign import ccall "wrapper" makeJoystickFunc ::
   JoystickCallback' -> IO (FunPtr JoystickCallback')

foreign import CALLCONV unsafe "glutJoystickFunc" glutJoystickFunc ::
   FunPtr JoystickCallback' -> CInt -> IO ()
