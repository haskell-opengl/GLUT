--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.GameMode
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
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
   Capability'(..), CapabilityDescription'(..), setGameModeCapabilities,
   enterGameMode, leaveGameMode,
   BitsPerPlane, RefreshRate, GameModeInfo(..), getGameModeInfo,
   isGameModeActive
) where

import Control.Monad ( liftM )
import Data.List ( intersperse )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.UI.GLUT.Constants
import Graphics.UI.GLUT.Initialization ( WindowSize(..), Relation,
                                         relationToString )
import Graphics.UI.GLUT.Window ( Window, makeWindow )

--------------------------------------------------------------------------------

-- | Capabilities for 'setGameModeCapabilities'

data Capability'
   = Width         -- ^ Width of the screen resolution in pixels
   | Height        -- ^ Height of the screen resolution in pixels
   | BitsPerPlane  -- ^ Color depth of the screen in bits
   | RefreshRate   -- ^ Refresh rate in Hertz
   | Num'          -- ^ Match the Nth frame buffer configuration compatible with
                   --   the given capabilities (numbering starts at 1)
   deriving ( Eq, Ord )

capabilityToString :: Capability' -> String
capabilityToString Width        = "width"
capabilityToString Height       = "height"
capabilityToString BitsPerPlane = "bpp"
capabilityToString RefreshRate  = "hertz"
capabilityToString Num'         = "num"

-- | A single capability description for 'setGameModeCapabilities'.

data CapabilityDescription' = Where' Capability' Relation CInt
   deriving ( Eq, Ord )

capabilityDescriptionToString :: CapabilityDescription' -> String
capabilityDescriptionToString (Where' c r i) =
      capabilityToString c ++ relationToString r ++ show i

--------------------------------------------------------------------------------

-- | Set the /game mode/ to be used when 'enterGameMode' is called. It is
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
-- (if any), use 'getGameModeInfo'.
--
-- Note that even for game mode previous calls to
-- 'Graphics.UI.GLUT.Initialization.setInitialDisplayMode'or
-- 'Graphics.UI.GLUT.Initialization.setInitialDisplayCapabilities' will
-- determine which buffers are available, if double buffering is used or not,
-- etc.

setGameModeCapabilities :: [CapabilityDescription'] -> IO ()
setGameModeCapabilities settings =
   withCString
      (concat . intersperse " " . map capabilityDescriptionToString $ settings)
      glutGameModeString

foreign import CALLCONV unsafe "glutGameModeString" glutGameModeString ::
   CString -> IO ()

--------------------------------------------------------------------------------

-- | Enter /game mode/, trying to change resolution, refresh rate, etc., as
-- specified by the last call to 'setGameModeCapabilities'. An identifier for
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

type BitsPerPlane = CInt

-- | The refresh rate of the screen, measured in Hertz (e.g. 60, 75, 100, ...)

type RefreshRate = CInt

data GameModeInfo = GameModeInfo WindowSize BitsPerPlane RefreshRate

--------------------------------------------------------------------------------

-- | Return 'Just' the mode which would be tried by the next call to
-- 'enterGameMode'. Returns 'Nothing' if the mode requested by the last call to
-- 'setGameModeCapabilities' is not possible, in which case 'enterGameMode'
-- would simply create a full screen window using the current mode.
--
-- /X Implementation Notes:/ GLUT for X will always return 'Nothing'.

getGameModeInfo :: IO (Maybe GameModeInfo)
getGameModeInfo = do
   possible <- getBool glut_GAME_MODE_POSSIBLE
   if possible
      then do w <- glutGameModeGet glut_GAME_MODE_WIDTH
              h <- glutGameModeGet glut_GAME_MODE_HEIGHT
              let size = WindowSize (fromIntegral w) (fromIntegral h)
              b <- glutGameModeGet glut_GAME_MODE_PIXEL_DEPTH
              r <- glutGameModeGet glut_GAME_MODE_REFRESH_RATE
              return $ Just $ GameModeInfo size b r
      else return Nothing

getBool :: GLenum -> IO Bool
getBool = liftM (/= 0) . glutGameModeGet

foreign import CALLCONV unsafe "glutGameModeGet" glutGameModeGet ::
   GLenum -> IO CInt

--------------------------------------------------------------------------------

-- | Test whether /game mode/ is active or not.

isGameModeActive :: IO Bool
isGameModeActive = getBool glut_GAME_MODE_ACTIVE
