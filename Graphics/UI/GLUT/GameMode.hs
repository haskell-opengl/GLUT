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
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.GameMode (
   GameModeInfo(..), BitsPerPlane, RefreshRate,
   Capability'(..), CapabilityDescription'(..),
   initGameMode, enterGameMode, leaveGameMode, isGameModeActive
) where

import Control.Monad ( liftM )
import Data.List ( intersperse )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Types ( CInt )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.UI.GLUT.Constants
import Graphics.UI.GLUT.Initialization ( WindowSize(..), Relation,
                                         ToString(..) )
import Graphics.UI.GLUT.Window ( Window(..) )

--------------------------------------------------------------------------------

type BitsPerPlane = CInt

type RefreshRate = CInt

data GameModeInfo = GameModeInfo WindowSize BitsPerPlane RefreshRate

--------------------------------------------------------------------------------

data Capability'
   = Width
   | Height
   | BitsPerPlane
   | RefreshRate
   | Num'              -- BETTER NAME!
   deriving ( Eq, Ord )

instance ToString Capability' where
   toString Width        = "width"
   toString Height       = "height"
   toString BitsPerPlane = "bpp"
   toString RefreshRate  = "hertz"
   toString Num'         = "num"

data CapabilityDescription' = Where' Capability' Relation CInt
   deriving ( Eq, Ord )

instance ToString CapabilityDescription' where
   toString (Where' c r i) = toString c ++ toString r ++ show i

initGameMode :: [CapabilityDescription'] -> IO (Maybe GameModeInfo)
initGameMode settings = do
   withCString (concat . intersperse " " . map toString $ settings)
               glutGameModeString
   p <- getBool glut_GAME_MODE_POSSIBLE
   if p
      then do w <- glutGameModeGet glut_GAME_MODE_WIDTH
              h <- glutGameModeGet glut_GAME_MODE_HEIGHT
              b <- glutGameModeGet glut_GAME_MODE_PIXEL_DEPTH
              r <- glutGameModeGet glut_GAME_MODE_REFRESH_RATE
              return $ Just $ GameModeInfo (WindowSize w h) b r
      else return Nothing

foreign import ccall unsafe "glutGameModeString" glutGameModeString ::
   CString -> IO ()

--------------------------------------------------------------------------------

enterGameMode :: IO (Window, Bool)
enterGameMode = do
   w <- glutEnterGameMode
   c <- getBool glut_GAME_MODE_DISPLAY_CHANGED
   return (Window w, c)

foreign import ccall unsafe "glutEnterGameMode" glutEnterGameMode :: IO CInt

--------------------------------------------------------------------------------

foreign import ccall unsafe "glutLeaveGameMode" leaveGameMode :: IO ()

--------------------------------------------------------------------------------

isGameModeActive :: IO Bool
isGameModeActive = getBool glut_GAME_MODE_ACTIVE

getBool :: GLenum -> IO Bool
getBool = liftM (/= 0) . glutGameModeGet

foreign import ccall unsafe "glutGameModeGet" glutGameModeGet ::
   GLenum -> IO CInt
