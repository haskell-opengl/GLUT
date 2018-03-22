--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Fonts
-- Copyright   :  (c) Sven Panne 2002-2018
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- GLUT supports two types of font rendering: stroke fonts, meaning each
-- character is rendered as a set of line segments; and bitmap fonts, where each
-- character is a bitmap generated with
-- 'Graphics.Rendering.OpenGL.GL.Bitmaps.bitmap'. Stroke fonts have the
-- advantage that because they are geometry, they can be arbitrarily scale and
-- rendered. Bitmap fonts are less flexible since they are rendered as bitmaps
-- but are usually faster than stroke fonts.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Fonts (
   Font(..), BitmapFont(..), StrokeFont(..),
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Char ( ord )
import Foreign.C.String ( withCString )
import Foreign.C.Types ( CInt )
import Foreign.Ptr ( castPtr )
import Graphics.Rendering.OpenGL ( GLint, GLfloat )

import Graphics.UI.GLUT.Raw

--------------------------------------------------------------------------------

class Font a where
   -- | Render the string in the named font, without using any display lists.
   -- Rendering a nonexistent character has no effect.
   --
   -- If the font is a bitmap font, 'renderString' automatically sets the OpenGL
   -- unpack pixel storage modes it needs appropriately and saves and restores
   -- the previous modes before returning. The generated call to
   -- 'Graphics.Rendering.OpenGL.GL.bitmap' will adjust the current raster
   -- position based on the width of the string.
   -- If the font is a stroke font,
   -- 'Graphics.Rendering.OpenGL.GL.CoordTrans.translate' is used to translate
   -- the current model view matrix to advance the width of the string.

   renderString :: MonadIO m => a -> String -> m ()

   -- | For a bitmap font, return the width in pixels of a string. For a stroke
   -- font, return the width in units. While the width of characters in a font
   -- may vary (though fixed width fonts do not vary), the maximum height
   -- characteristics of a particular font are fixed.

   stringWidth :: MonadIO m => a -> String -> m GLint

   -- | (/freeglut only/) For a bitmap font, return the maximum height of the
   -- characters in the given font measured in pixels. For a stroke font,
   -- return the width in units.

   fontHeight :: MonadIO m => a -> m GLfloat

instance Font BitmapFont where
   renderString = bitmapString
   stringWidth  = bitmapLength
   fontHeight   = bitmapHeight


instance Font StrokeFont where
   renderString = strokeString
   stringWidth  = strokeLength
   fontHeight   = strokeHeight

--------------------------------------------------------------------------------

bitmapString :: MonadIO m => BitmapFont -> String -> m ()
bitmapString f s = do
   i <- marshalBitmapFont f
   mapM_ (\c -> withChar c (glutBitmapCharacter i)) s

withChar :: Char -> (CInt -> m a) -> m a
withChar c f = f . fromIntegral . ord $ c

--------------------------------------------------------------------------------

strokeString :: MonadIO m => StrokeFont -> String -> m ()
strokeString f s = do
   i <- marshalStrokeFont f
   mapM_ (\c -> withChar c (glutStrokeCharacter i)) s

--------------------------------------------------------------------------------

bitmapLength :: MonadIO m
             => BitmapFont -- ^ Bitmap font to use.
             -> String     -- ^ String to return width of (not confined to 8
                           --   bits).
             -> m GLint    -- ^ Width in pixels.
bitmapLength f s = liftIO $ do
   i <- marshalBitmapFont f
   fmap fromIntegral $ withCString s (glutBitmapLength i . castPtr)

--------------------------------------------------------------------------------

strokeLength :: MonadIO m
             => StrokeFont -- ^ Stroke font to use.
             -> String     -- ^ String to return width of (not confined to 8
                           --   bits).
             -> m GLint    -- ^ Width in units.
strokeLength f s = liftIO $ do
   i <- marshalStrokeFont f
   fmap fromIntegral $ withCString s (glutStrokeLength i . castPtr)

--------------------------------------------------------------------------------

bitmapHeight :: MonadIO m
             => BitmapFont -- ^ Bitmap font to use.
             -> m GLfloat  -- ^ Height in pixels.
bitmapHeight f = liftIO $ do
  i <- marshalBitmapFont f
  fromIntegral `fmap` glutBitmapHeight  i

--------------------------------------------------------------------------------

strokeHeight :: MonadIO m
             => StrokeFont -- ^ Stroke font to use.
             -> m GLfloat  -- ^ Height in units.
strokeHeight f = glutStrokeHeight =<< marshalStrokeFont f
