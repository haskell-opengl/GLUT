--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Callbacks.Window
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Callbacks.Window (
   -- * Redisplay callbacks
   DisplayCallback, setDisplayCallback, setOverlayDisplayCallback,

   -- * Reshape callback
   ReshapeCallback, setReshapeCallback,

   -- * Callback for visibility changes
   Visibility(..), VisibilityCallback, setVisibilityCallback,

   -- * Keyboard and mouse input callback
   Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..),
   KeyboardMouseCallback, setKeyboardMouseCallback,
   marshalMouseButton,   -- used only internally

   -- * Mouse movement callbacks
   MotionCallback, setMotionCallback, setPassiveMotionCallback,
   Crossing(..), CrossingCallback, setCrossingCallback,

   -- * Spaceball callback
   SpaceballMotion, SpaceballRotation, SpaceballInput(..),
   SpaceballCallback, setSpaceballCallback,

   -- * Dial & button box callback
   DialAndButtonBoxInput(..),
   DialAndButtonBoxCallback, setDialAndButtonBoxCallback,

   -- * Tablet callback
   TabletPosition(..), TabletInput(..), TabletCallback, setTabletCallback
) where

import Control.Monad ( liftM )
import Data.Bits ( Bits((.&.)) )
import Data.Char ( chr )
import Foreign.C.Types ( CInt, CUChar )
import Foreign.Ptr ( FunPtr )
import Graphics.UI.GLUT.Callbacks.Registration ( CallbackType(..), setCallback )
import Graphics.UI.GLUT.Initialization ( WindowSize(..), WindowPosition(..) )
import Graphics.UI.GLUT.State ( NumButtons, NumDials )
import Graphics.UI.GLUT.Constants

--------------------------------------------------------------------------------

type DisplayCallback = IO ()

-- | Set the display callback for the /current window./ When GLUT determines
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
-- See 'setOverlayDisplayCallback' to understand how distinct callbacks for the
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
-- (returned by calling 'Graphics.UI.GLUT.State.isNormalDamaged') is cleared. If
-- there is no overlay display callback registered the overlay damaged state of
-- the window (returned by calling 'Graphics.UI.GLUT.State.isOverlayDamaged') is
-- also cleared.

setDisplayCallback :: DisplayCallback -> IO ()
setDisplayCallback =
   setCallback DisplayCB glutDisplayFunc makeDisplayCallback . Just

foreign import ccall "wrapper" makeDisplayCallback ::
   DisplayCallback -> IO (FunPtr DisplayCallback)

foreign import CALLCONV unsafe "glutDisplayFunc" glutDisplayFunc ::
   FunPtr DisplayCallback -> IO ()

--------------------------------------------------------------------------------

-- | Set the overlay display callback for the /current window./ The overlay
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
-- the window (returned by calling 'Graphics.UI.GLUT.State.isOverlayDamaged')
-- is cleared.
--
-- Initially there is no overlay display callback registered when an overlay is
-- established. See 'setDisplayCallback' to understand how the display callback
-- alone is used if an overlay display callback is not registered.

setOverlayDisplayCallback :: Maybe DisplayCallback -> IO ()
setOverlayDisplayCallback =
   setCallback OverlayDisplayCB glutOverlayDisplayFunc makeDisplayCallback

foreign import CALLCONV unsafe "glutOverlayDisplayFunc" glutOverlayDisplayFunc
   :: FunPtr DisplayCallback -> IO ()

--------------------------------------------------------------------------------

type ReshapeCallback = WindowSize -> IO ()

type ReshapeCallback' = CInt -> CInt -> IO ()

-- | Set the reshape callback for the /current window./ The reshape callback is
-- triggered when a window is reshaped. A reshape callback is also triggered
-- immediately before a window\'s first display callback after a window is
-- created or whenever an overlay for the window is established. The parameter
-- of the callback specifies the new window size in pixels. Before the callback,
-- the /current window/ is set to the window that has been reshaped.
--
-- If a reshape callback is not registered for a window or 'Nothing' is passed
-- to setReshapeCallback (to deregister a previously registered callback), the
-- default reshape callback is used. This default callback will simply call
--
-- @
-- 'viewport' ('Graphics.UI.GLUT.Initialization.WindowPosition' 0 0) ('Graphics.UI.GLUT.Initialization.WindowSize' /width/ /height/)
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
-- when their size is changed using 'Graphics.UI.GLUT.Window.reshapeWindow'.

setReshapeCallback :: Maybe ReshapeCallback -> IO ()
setReshapeCallback =
   setCallback ReshapeCB glutReshapeFunc (makeReshapeCallback . unmarshal)
   where unmarshal cb w h  = cb (WindowSize w h)

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
   deriving ( Eq, Ord )

unmarshalVisibility :: CInt -> Visibility
unmarshalVisibility v
   | v == glut_NOT_VISIBLE = NotVisible
   | v == glut_VISIBLE     = Visible
   | otherwise = error "unmarshalVisibility"

--------------------------------------------------------------------------------

type VisibilityCallback = Visibility -> IO ()

type VisibilityCallback' = CInt -> IO ()

-- | Set the visibility callback for the /current window./ The visibility
-- callback for a window is called when the visibility of a window changes.
-- 
-- If the visibility callback for a window is disabled and later re-enabled, the
-- visibility status of the window is undefined; any change in window visibility
-- will be reported, that is if you disable a visibility callback and re-enable
-- the callback, you are guaranteed the next visibility change will be reported.

setVisibilityCallback :: Maybe VisibilityCallback -> IO ()
setVisibilityCallback = setCallback VisibilityCB glutVisibilityFunc
                                    (makeVisibilityCallback . unmarshal)
   where unmarshal cb  = cb . unmarshalVisibility

foreign import ccall "wrapper" makeVisibilityCallback ::
   VisibilityCallback' -> IO (FunPtr VisibilityCallback')

foreign import CALLCONV unsafe "glutVisibilityFunc" glutVisibilityFunc ::
   FunPtr VisibilityCallback' -> IO ()

--------------------------------------------------------------------------------

type KeyboardCallback = Char -> WindowPosition -> IO ()

type KeyboardCallback' = CUChar -> CInt -> CInt -> IO ()

setKeyboardCallback :: Maybe KeyboardCallback -> IO ()
setKeyboardCallback =
   setCallback KeyboardCB glutKeyboardFunc (makeKeyboardCallback . unmarshal)
   where unmarshal cb c x y = cb (chr (fromIntegral c)) (WindowPosition x y)

foreign import ccall "wrapper" makeKeyboardCallback ::
   KeyboardCallback' -> IO (FunPtr KeyboardCallback')

foreign import CALLCONV unsafe "glutKeyboardFunc" glutKeyboardFunc ::
   FunPtr KeyboardCallback' -> IO ()

--------------------------------------------------------------------------------

setKeyboardUpCallback :: Maybe KeyboardCallback -> IO ()
setKeyboardUpCallback =
   setCallback KeyboardUpCB glutKeyboardUpFunc
               (makeKeyboardCallback . unmarshal)
   where unmarshal cb c x y = cb (chr (fromIntegral c)) (WindowPosition x y)

foreign import CALLCONV unsafe "glutKeyboardUpFunc" glutKeyboardUpFunc ::
   FunPtr KeyboardCallback' -> IO ()

--------------------------------------------------------------------------------

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
   deriving ( Eq, Ord )

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

type SpecialCallback = SpecialKey -> WindowPosition -> IO ()

type SpecialCallback' = CInt -> CInt -> CInt -> IO ()

setSpecialCallback :: Maybe SpecialCallback -> IO ()
setSpecialCallback =
   setCallback SpecialCB glutSpecialFunc (makeSpecialCallback . unmarshal)
   where unmarshal cb k x y = cb (unmarshalSpecialKey k) (WindowPosition x y)

foreign import ccall "wrapper" makeSpecialCallback ::
   SpecialCallback' -> IO (FunPtr SpecialCallback')

foreign import CALLCONV unsafe "glutSpecialFunc" glutSpecialFunc ::
   FunPtr SpecialCallback' -> IO ()

--------------------------------------------------------------------------------

setSpecialUpCallback :: Maybe SpecialCallback -> IO ()
setSpecialUpCallback =
   setCallback SpecialUpCB glutSpecialUpFunc (makeSpecialCallback . unmarshal)
   where unmarshal cb k x y = cb (unmarshalSpecialKey k) (WindowPosition x y)

foreign import CALLCONV unsafe "glutSpecialUpFunc" glutSpecialUpFunc ::
   FunPtr SpecialCallback' -> IO ()

--------------------------------------------------------------------------------

data MouseButton
   = LeftButton
   | MiddleButton
   | RightButton
   | WheelUp
   | WheelDown
   deriving ( Eq, Ord )

marshalMouseButton :: MouseButton -> CInt
marshalMouseButton LeftButton   = glut_LEFT_BUTTON
marshalMouseButton MiddleButton = glut_MIDDLE_BUTTON
marshalMouseButton RightButton  = glut_RIGHT_BUTTON
marshalMouseButton WheelUp      = glut_WHEEL_UP
marshalMouseButton WheelDown    = glut_WHEEL_DOWN

unmarshalMouseButton :: CInt -> MouseButton
unmarshalMouseButton b
   | b == glut_LEFT_BUTTON   = LeftButton
   | b == glut_MIDDLE_BUTTON = MiddleButton
   | b == glut_RIGHT_BUTTON  = RightButton
   | b == glut_WHEEL_UP      = WheelUp
   | b == glut_WHEEL_DOWN    = WheelDown
   | otherwise = error "unmarshalMouseButton"

--------------------------------------------------------------------------------

data KeyState
   = Down
   | Up
   deriving ( Eq, Ord )

unmarshalKeyState :: CInt -> KeyState
unmarshalKeyState s
   | s == glut_DOWN = Down
   | s == glut_UP   = Up
   | otherwise = error "unmarshalKeyState"

--------------------------------------------------------------------------------

type MouseCallback = MouseButton -> KeyState -> WindowPosition -> IO ()

type MouseCallback' = CInt -> CInt -> CInt -> CInt -> IO ()

setMouseCallback :: Maybe MouseCallback -> IO ()
setMouseCallback =
   setCallback MouseCB glutMouseFunc (makeMouseCallback . unmarshal)
   where unmarshal cb b s x y = cb (unmarshalMouseButton b)
                                   (unmarshalKeyState s) (WindowPosition x y)

foreign import ccall "wrapper" makeMouseCallback ::
   MouseCallback' -> IO (FunPtr MouseCallback')

foreign import CALLCONV unsafe "glutMouseFunc" glutMouseFunc ::
   FunPtr MouseCallback' -> IO ()

--------------------------------------------------------------------------------

data Modifiers = Modifiers { shift, ctrl, alt :: Bool } deriving ( Eq, Ord )

-- Could use fromBitfield + Enum/Bounded instances + marshalModifier instead...
unmarshalModifiers :: CInt -> Modifiers
unmarshalModifiers m = Modifiers {
   shift = (m .&. glut_ACTIVE_SHIFT) /= 0,
   ctrl  = (m .&. glut_ACTIVE_CTRL ) /= 0,
   alt   = (m .&. glut_ACTIVE_ALT  ) /= 0 }

getModifiers :: IO Modifiers
getModifiers = liftM unmarshalModifiers glutGetModifiers

foreign import CALLCONV unsafe "glutGetModifiers" glutGetModifiers :: IO CInt

--------------------------------------------------------------------------------

data Key
   = Char Char
   | SpecialKey SpecialKey
   | MouseButton MouseButton
   deriving ( Eq, Ord )

type KeyboardMouseCallback =
   Key -> KeyState -> Modifiers -> WindowPosition -> IO ()

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

type MotionCallback = WindowPosition -> IO ()

type MotionCallback' = CInt -> CInt -> IO ()

-- | Set the motion callback for the /current window./ The motion callback for a
-- window is called when the mouse moves within the window while one or more
-- mouse buttons are pressed. The callback parameter indicates the mouse
-- location in window relative coordinates.

setMotionCallback :: Maybe MotionCallback -> IO ()
setMotionCallback =
   setCallback MotionCB glutMotionFunc (makeMotionCallback . unmarshal)
   where unmarshal cb x y  = cb (WindowPosition x y)

foreign import ccall "wrapper" makeMotionCallback ::
   MotionCallback' -> IO (FunPtr MotionCallback')

foreign import CALLCONV unsafe "glutMotionFunc" glutMotionFunc ::
   FunPtr MotionCallback' -> IO ()

--------------------------------------------------------------------------------

-- | Set the passive motion callback for the /current window./ The passive
-- motion callback for a window is called when the mouse moves within the window
-- while /no/ mouse buttons are pressed. The callback parameter indicates the
-- mouse location in window relative coordinates.

setPassiveMotionCallback :: Maybe MotionCallback -> IO ()
setPassiveMotionCallback =
   setCallback MotionCB glutPassiveMotionFunc (makeMotionCallback . unmarshal)
   where unmarshal cb x y  = cb (WindowPosition x y)

foreign import CALLCONV unsafe "glutPassiveMotionFunc" glutPassiveMotionFunc ::
   FunPtr MotionCallback' -> IO ()


--------------------------------------------------------------------------------

data Crossing
   = WindowLeft    -- ^ The mouse pointer has left the /current window./
   | WindowEntered -- ^ The mouse pointer has entered the /current window./
   deriving ( Eq, Ord )

unmarshalCrossing :: CInt -> Crossing
unmarshalCrossing c
   | c == glut_LEFT    = WindowLeft
   | c == glut_ENTERED = WindowEntered
   | otherwise = error "unmarshalCrossing"

--------------------------------------------------------------------------------

type CrossingCallback = Crossing -> IO ()

type CrossingCallback' = CInt -> IO ()

-- | Set the mouse enter\/leave callback for the /current window./ Note that
-- some window systems may not generate accurate enter\/leave callbacks.
--
-- /X Implementation Notes:/ An X implementation of GLUT should generate
-- accurate enter\/leave callbacks.

setCrossingCallback :: Maybe CrossingCallback -> IO ()
setCrossingCallback =
   setCallback CrossingCB glutEntryFunc (makeCrossingCallback . unmarshal)
   where unmarshal cb = cb . unmarshalCrossing

foreign import ccall "wrapper" makeCrossingCallback ::
   CrossingCallback' -> IO (FunPtr CrossingCallback')

foreign import CALLCONV unsafe "glutEntryFunc" glutEntryFunc ::
   FunPtr CrossingCallback' -> IO ()

--------------------------------------------------------------------------------

-- | Translation of the Spaceball along one axis, normalized to be in the range
-- of -1000 to +1000 inclusive
type SpaceballMotion = CInt

-- | Rotation of the Spaceball along one axis, normalized to be in the range
-- of -1800 .. +1800 inclusive
type SpaceballRotation = CInt

data SpaceballInput
   = SpaceballMotion   SpaceballMotion SpaceballMotion SpaceballMotion
   | SpaceballRotation SpaceballRotation SpaceballRotation SpaceballRotation
   | SpaceballButton   NumButtons KeyState

type SpaceballCallback = SpaceballInput -> IO ()

-- | Set the Spaceball callback for the /current window./ The Spaceball callback
-- for a window is called when the window has Spaceball input focus (normally,
-- when the mouse is in the window) and the user generates Spaceball
-- translations, rotations, or button presses. The number of available Spaceball
-- buttons can be determined with 'Graphics.UI.GLUT.State.getSpaceballInfo'.
--
-- Registering a Spaceball callback when a Spaceball device is not available has
-- no effect and is not an error. In this case, no Spaceball callbacks will be
-- generated.

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
   where unmarshal cb x y z = cb x y z

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
   where unmarshal cb x y z = cb x y z

foreign import ccall "wrapper" makeSpaceballRotationCallback ::
   SpaceballRotationCallback -> IO (FunPtr SpaceballRotationCallback)

foreign import CALLCONV unsafe "glutSpaceballRotateFunc" glutSpaceballRotateFunc
   :: FunPtr SpaceballRotationCallback -> IO ()

--------------------------------------------------------------------------------

type SpaceballButtonCallback = NumButtons -> KeyState -> IO ()

type SpaceballButtonCallback' = CInt -> CInt -> IO ()

setSpaceballButtonCallback :: Maybe SpaceballButtonCallback -> IO ()
setSpaceballButtonCallback =
   setCallback SpaceballButtonCB glutSpaceballButtonFunc
               (makeSpaceballButtonCallback . unmarshal)
   where unmarshal cb b s = cb b (unmarshalKeyState s)

foreign import ccall "wrapper" makeSpaceballButtonCallback ::
   SpaceballButtonCallback' -> IO (FunPtr SpaceballButtonCallback')

foreign import CALLCONV unsafe "glutSpaceballButtonFunc"
   glutSpaceballButtonFunc :: FunPtr SpaceballButtonCallback' -> IO ()

--------------------------------------------------------------------------------

data DialAndButtonBoxInput
   = DialAndButtonBoxButton NumButtons KeyState
   | DialAndButtonBoxDial   NumDials CInt
   deriving ( Eq, Ord )

type DialAndButtonBoxCallback = DialAndButtonBoxInput -> IO ()

-- | Set the dial & button box callback for the /current window./ The dial &
-- button box button callback for a window is called when the window has dial &
-- button box input focus (normally, when the mouse is in the window) and the
-- user generates dial & button box button presses or dial changes. The number
-- of available dial & button box buttons and dials can be determined with
-- 'Graphics.UI.GLUT.State.getDialAndButtonBoxInfo'.
--
-- Registering a dial & button box callback when a dial & button box device is
-- not available is ineffectual and not an error. In this case, no dial & button
-- box button will be generated.


setDialAndButtonBoxCallback :: Maybe DialAndButtonBoxCallback -> IO ()
setDialAndButtonBoxCallback Nothing = do
   setButtonBoxCallback Nothing
   setDialsCallback     Nothing
setDialAndButtonBoxCallback (Just cb) = do
   setButtonBoxCallback (Just (\b s -> cb (DialAndButtonBoxButton b s)))
   setDialsCallback     (Just (\d x -> cb (DialAndButtonBoxDial   d x)))

--------------------------------------------------------------------------------

type ButtonBoxCallback = NumButtons -> KeyState -> IO ()

type ButtonBoxCallback' = CInt -> CInt -> IO ()

setButtonBoxCallback :: Maybe ButtonBoxCallback -> IO ()
setButtonBoxCallback =
   setCallback ButtonBoxCB glutButtonBoxFunc (makeButtonBoxFunc . unmarshal)
   where unmarshal cb b s = cb b (unmarshalKeyState s)

foreign import ccall "wrapper" makeButtonBoxFunc ::
   ButtonBoxCallback' -> IO (FunPtr ButtonBoxCallback')

foreign import CALLCONV unsafe "glutButtonBoxFunc" glutButtonBoxFunc ::
   FunPtr ButtonBoxCallback' -> IO ()

--------------------------------------------------------------------------------

type DialsCallback = NumDials -> CInt -> IO ()

setDialsCallback :: Maybe DialsCallback -> IO ()
setDialsCallback =
    setCallback DialsCB glutDialsFunc (makeDialsFunc . unmarshal)
    where unmarshal cb d x = cb d x

foreign import ccall "wrapper" makeDialsFunc ::
   DialsCallback -> IO (FunPtr DialsCallback)

foreign import CALLCONV unsafe "glutDialsFunc" glutDialsFunc ::
   FunPtr DialsCallback -> IO ()

--------------------------------------------------------------------------------

-- | Absolute tablet position, with coordinates normalized to be in the range of
-- 0 to 2000 inclusive
data TabletPosition = TabletPosition CInt CInt

data TabletInput
   = TabletMotion
   | TabletButton NumButtons KeyState

type TabletCallback = TabletInput -> TabletPosition -> IO ()

-- | Set the tablet callback for the /current window./ The tablet callback for a
-- window is called when the window has tablet input focus (normally, when the
-- mouse is in the window) and the user generates tablet motion or button
-- presses. The number of available tablet buttons can be determined with
-- 'Graphics.UI.GLUT.State.getTabletInfo'.
--
-- Registering a tablet callback when a tablet device is not available is
-- ineffectual and not an error. In this case, no tablet callbacks will be
-- generated.

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
    where unmarshal cb x y = cb (TabletPosition x y)

foreign import ccall "wrapper" makeTabletMotionFunc ::
   TabletMotionCallback' -> IO (FunPtr TabletMotionCallback')

foreign import CALLCONV unsafe "glutTabletMotionFunc" glutTabletMotionFunc ::
   FunPtr TabletMotionCallback' -> IO ()

--------------------------------------------------------------------------------

type TabletButtonCallback = NumButtons -> KeyState -> TabletPosition -> IO ()

type TabletButtonCallback' = CInt -> CInt -> CInt -> CInt -> IO ()

setTabletButtonCallback :: Maybe TabletButtonCallback -> IO ()
setTabletButtonCallback =
    setCallback TabletButtonCB glutTabletButtonFunc
                (makeTabletButtonFunc . unmarshal)
    where unmarshal cb b s x y = cb b (unmarshalKeyState s) (TabletPosition x y)

foreign import ccall "wrapper" makeTabletButtonFunc ::
   TabletButtonCallback' -> IO (FunPtr TabletButtonCallback')

foreign import CALLCONV unsafe "glutTabletButtonFunc" glutTabletButtonFunc ::
   FunPtr TabletButtonCallback' -> IO ()
