--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.GameMode
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- In addition to the functionality offered by
-- 'Graphics.UI.GLUT.Window.fullScreen', GLUT offers an sub-API to change the
-- screen resolution, color depth, and refresh rate of the display for a single
-- full screen window. This mode of operation is called /game mode/, and is
-- restricted in various ways: No pop-up menus are allowed for this full screen
-- window, no other (sub-)windows can be created, and all other applications are
-- hidden.
--
-- /X Implementation Notes:/ Note that game mode is not fully supported in
-- GLUT for X, it is essentially the same as using
-- 'Graphics.UI.GLUT.Window.fullScreen', with the additional restrictions
-- mentioned above.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.GameMode (
   GameModeCapability(..), GameModeCapabilityDescription(..),
   gameModeCapabilities, enterGameMode, leaveGameMode,
   BitsPerPlane, RefreshRate, GameModeInfo(..), gameModeInfo,
   gameModeActive
) where

import Control.Monad ( liftM )
import Data.List ( intersperse )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar,
   SettableStateVar, makeSettableStateVar )
import Graphics.UI.GLUT.Constants (
   glut_GAME_MODE_DISPLAY_CHANGED, glut_GAME_MODE_POSSIBLE,
   glut_GAME_MODE_WIDTH, glut_GAME_MODE_HEIGHT,
   glut_GAME_MODE_PIXEL_DEPTH, glut_GAME_MODE_REFRESH_RATE,
   glut_GAME_MODE_ACTIVE )
import Graphics.UI.GLUT.Types ( makeWindow, relationToString )
import Graphics.UI.GLUT.Window ( Window )
import Graphics.UI.GLUT.Initialization ( Relation(..) )

--------------------------------------------------------------------------------

-- | Capabilities for 'gameModeCapabilities'

data GameModeCapability
   = GameModeWidth         -- ^ Width of the screen resolution in pixels
   | GameModeHeight        -- ^ Height of the screen resolution in pixels
   | GameModeBitsPerPlane  -- ^ Color depth of the screen in bits
   | GameModeRefreshRate   -- ^ Refresh rate in Hertz
   | GameModeNum           -- ^ Match the Nth frame buffer configuration
                           --   compatible with the given capabilities
                           --   (numbering starts at 1)
   deriving ( Eq, Ord, Show )

gameModeCapabilityToString :: GameModeCapability -> String
gameModeCapabilityToString x = case x of
   GameModeWidth        -> "width"
   GameModeHeight       -> "height"
   GameModeBitsPerPlane -> "bpp"
   GameModeRefreshRate  -> "hertz"
   GameModeNum          -> "num"

-- | A single capability description for 'gameModeCapabilities'.

data GameModeCapabilityDescription = Where' GameModeCapability Relation Int
   deriving ( Eq, Ord, Show )

gameModeCapabilityDescriptionToString :: GameModeCapabilityDescription -> String
gameModeCapabilityDescriptionToString (Where' c r i) =
      gameModeCapabilityToString c ++ relationToString r ++ show i

--------------------------------------------------------------------------------

-- | Controls the /game mode/ to be used when 'enterGameMode' is called. It is
-- described by a list of zero or more capability descriptions, which are
-- translated into a set of criteria used to select the appropriate screen
-- configuration. The criteria are matched in strict left to right order of
-- precdence. That is, the first specified criterion (leftmost) takes precedence
-- over the later criteria for non-exact criteria
-- ('Graphics.UI.GLUT.Initialization.IsGreaterThan',
-- 'Graphics.UI.GLUT.Initialization.IsLessThan', etc.). Exact criteria
-- ('Graphics.UI.GLUT.Initialization.IsEqualTo',
-- 'Graphics.UI.GLUT.Initialization.IsNotEqualTo') must match exactly so
-- precedence is not relevant.
--
-- To determine which configuration will actually be tried by 'enterGameMode'
-- (if any), use 'gameModeInfo'.
--
-- Note that even for game mode the current values of
-- 'Graphics.UI.GLUT.Initialization.initialDisplayMode'or
-- 'Graphics.UI.GLUT.Initialization.initialDisplayCapabilities' will
-- determine which buffers are available, if double buffering is used or not,
-- etc.

gameModeCapabilities :: SettableStateVar [GameModeCapabilityDescription]
gameModeCapabilities = makeSettableStateVar $ \settings ->
   withCString
      (concat . intersperse " " . map gameModeCapabilityDescriptionToString $
       settings)
      glutGameModeString

foreign import CALLCONV unsafe "glutGameModeString" glutGameModeString ::
   CString -> IO ()

--------------------------------------------------------------------------------

-- | Enter /game mode/, trying to change resolution, refresh rate, etc., as
-- specified by the current value of 'gameModeCapabilities'. An identifier for
-- the game mode window and a flag, indicating if the display mode actually
-- changed, are returned. The game mode window is made the /current window/.
--
-- Re-entering /game mode/ is allowed, the previous game mode window gets
-- destroyed by this, and a new one is created.
--
-- /X Implementation Notes:/ GLUT for X never changes the display mode, but
-- simply creates a full screen window, requesting absolutely no decorations
-- from the window manager.

enterGameMode :: IO (Window, Bool)
enterGameMode = do
   w <- glutEnterGameMode
   c <- getBool glut_GAME_MODE_DISPLAY_CHANGED
   return (makeWindow w, c)

foreign import CALLCONV unsafe "glutEnterGameMode" glutEnterGameMode :: IO CInt

--------------------------------------------------------------------------------

-- | Leave /game mode/, restoring the old display mode and destroying the game
-- mode window.

foreign import CALLCONV unsafe "glutLeaveGameMode" leaveGameMode :: IO ()

--------------------------------------------------------------------------------

-- | The color depth of the screen, measured in bits (e.g. 8, 16, 24, 32, ...)

type BitsPerPlane = Int

-- | The refresh rate of the screen, measured in Hertz (e.g. 60, 75, 100, ...)

type RefreshRate = Int

data GameModeInfo = GameModeInfo Size BitsPerPlane RefreshRate

--------------------------------------------------------------------------------

-- | Return 'Just' the mode which would be tried by the next call to
-- 'enterGameMode'. Returns 'Nothing' if the mode requested by the current value
-- of 'gameModeCapabilities' is not possible, in which case 'enterGameMode'
-- would simply create a full screen window using the current mode.
--
-- /X Implementation Notes:/ GLUT for X will always return 'Nothing'.

gameModeInfo :: GettableStateVar (Maybe GameModeInfo)
gameModeInfo = makeGettableStateVar $ do
   possible <- getBool glut_GAME_MODE_POSSIBLE
   if possible
      then do
         w <- glutGameModeGet glut_GAME_MODE_WIDTH
         h <- glutGameModeGet glut_GAME_MODE_HEIGHT
         let size = Size (fromIntegral w) (fromIntegral h)
         b <- glutGameModeGet glut_GAME_MODE_PIXEL_DEPTH
         r <- glutGameModeGet glut_GAME_MODE_REFRESH_RATE
         return $ Just $ GameModeInfo size (fromIntegral b) (fromIntegral r)
      else return Nothing

getBool :: GLenum -> IO Bool
getBool = liftM (/= 0) . glutGameModeGet

foreign import CALLCONV unsafe "glutGameModeGet" glutGameModeGet ::
   GLenum -> IO CInt

--------------------------------------------------------------------------------

-- | Contains 'True' when the /game mode/ is active, 'False' otherwise.

gameModeActive :: GettableStateVar Bool
gameModeActive = makeGettableStateVar $ getBool glut_GAME_MODE_ACTIVE
