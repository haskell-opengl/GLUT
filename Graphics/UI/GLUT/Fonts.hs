--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Fonts
-- Copyright   :  (c) Sven Panne 2002
-- License     :  BSD-style (see the file libraries/GLUT/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- GLUT supports two type of font rendering: stroke fonts, meaning each
-- character is rendered as a set of line segments; and bitmap fonts, where each
-- character is a bitmap generated with 'Graphics.Rendering.OpenGL.GL.bitmap'.
-- Stroke fonts have the advantage that because they are geometry, they can be
-- arbitrarily scale and rendered. Bitmap fonts are less flexible since they are
-- rendered as bitmaps but are usually faster than stroke fonts.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Fonts (
   -- * Bitmap fonts
   BitmapFont(..), bitmapCharacter, bitmapWidth,
   -- * Stroke fonts
   StrokeFont(..), strokeCharacter, strokeWidth
) where

import Foreign.C.Types ( CInt )
import Foreign.Ptr ( Ptr )

--------------------------------------------------------------------------------

-- | The bitmap fonts available in GLUT. The exact bitmap to be used is
-- defined by the standard X glyph bitmaps for the X font with the given name.

data BitmapFont
   = Fixed8By13   -- ^ A fixed width font with every character fitting in an 8
                  --   by 13 pixel rectangle.
                  --   (@-misc-fixed-medium-r-normal--13-120-75-75-C-80-iso8859-1@)
   | Fixed9By15   -- ^ A fixed width font with every character fitting in an 9
                  --   by 15 pixel rectangle.
                  --   (@-misc-fixed-medium-r-normal--15-140-75-75-C-90-iso8859-1@)
   | TimesRoman10 -- ^ A 10-point proportional spaced Times Roman font.
                  --   (@-adobe-times-medium-r-normal--10-100-75-75-p-54-iso8859-1@)
   | TimesRoman24 -- ^ A 24-point proportional spaced Times Roman font.
                  --   (@-adobe-times-medium-r-normal--24-240-75-75-p-124-iso8859-1@)
   | Helvetica10  -- ^ A 10-point proportional spaced Helvetica font.
                  --   (@-adobe-helvetica-medium-r-normal--10-100-75-75-p-56-iso8859-1@)
   | Helvetica12  -- ^ A 12-point proportional spaced Helvetica font.
                  --   (@-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1@)
   | Helvetica18  -- ^ A 18-point proportional spaced Helvetica font.
                  --   (@-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1@)
   deriving ( Eq, Ord )

-- Alas, fonts in GLUT are not denoted by some integral value, but by opaque
-- pointers on the C side. Even worse: For WinDoze, they are simply small ints,
-- casted to void*, for other platforms addresses of global variables are used.
-- And all is done via ugly #ifdef-ed #defines... Aaaaargl! So the only portable
-- way is using integers on the Haskell side and doing the marshaling via some
-- small C wrappers around those macros. *sigh*
type GLUTbitmapFont = Ptr ()

foreign import ccall "hOpenGL_marshalBitmapFont" hOpenGL_marshalBitmapFont ::
   CInt -> IO GLUTbitmapFont

marhshalBitmapFont :: BitmapFont -> IO GLUTbitmapFont
marhshalBitmapFont f = case f of
   Fixed8By13   -> hOpenGL_marshalBitmapFont 0
   Fixed9By15   -> hOpenGL_marshalBitmapFont 1
   TimesRoman10 -> hOpenGL_marshalBitmapFont 2
   TimesRoman24 -> hOpenGL_marshalBitmapFont 3
   Helvetica10  -> hOpenGL_marshalBitmapFont 4
   Helvetica12  -> hOpenGL_marshalBitmapFont 5
   Helvetica18  -> hOpenGL_marshalBitmapFont 6

-- | Render the character in the named bitmap font, without using any display
-- lists. Rendering a nonexistent character has no effect. 'bitmapCharacter'
-- automatically sets the OpenGL unpack pixel storage modes it needs
-- appropriately and saves and restores the previous modes before returning.
-- The generated call to 'Graphics.Rendering.OpenGL.GL.bitmap' will adjust the
-- current raster position based on the width of the character.

bitmapCharacter :: BitmapFont -- ^ Bitmap font to use.
                -> CInt       -- ^ Character to return width of (not confined to
                              --   8 bits).
                -> IO ()
bitmapCharacter f c = flip glutBitmapCharacter c =<< marhshalBitmapFont f

foreign import ccall "glutBitmapCharacter" glutBitmapCharacter ::
   GLUTbitmapFont -> CInt -> IO ()

-- | Return the width in pixels of a bitmap character in a supported bitmap
-- font. While the width of characters in a font may vary (though fixed width
-- fonts do not vary), the maximum height characteristics of a particular font
-- are fixed.

bitmapWidth :: BitmapFont -- ^ Bitmap font to use.
            -> CInt       -- ^ Character to render (not confined to 8 bits).
            -> IO CInt    -- ^ Width in pixels.
bitmapWidth f c = flip glutBitmapWidth c =<< marhshalBitmapFont f

foreign import ccall "glutBitmapWidth" glutBitmapWidth ::
   GLUTbitmapFont -> CInt -> IO CInt

--------------------------------------------------------------------------------

data StrokeFont
   = Roman     -- ^ A proportionally spaced Roman Simplex font for ASCII
               --   characters 32 through 127. The maximum top character in the
               --   font is 119.05 units; the bottom descends 33.33 units.
   | MonoRoman -- ^ A mono-spaced spaced Roman Simplex font (same characters as
               --   'Roman') for ASCII characters 32 through 127. The maximum
               --   top character in the font is 119.05 units; the bottom
               --   descends 33.33 units. Each character is 104.76 units wide.
   deriving ( Eq, Ord )


-- Same remarks as for GLUTstrokeFont
type GLUTstrokeFont = Ptr ()

foreign import ccall "hOpenGL_marshalStrokeFont" hOpenGL_marshalStrokeFont ::
   CInt -> IO GLUTstrokeFont

marhshalStrokeFont :: StrokeFont -> IO GLUTstrokeFont
marhshalStrokeFont f = case f of
   Roman     -> hOpenGL_marshalStrokeFont 0
   MonoRoman -> hOpenGL_marshalStrokeFont 1

-- | Render the character in the named stroke font, without using any display
-- lists. Rendering a nonexistent character has no effect. A
-- 'Graphics.Rendering.OpenGL.GL.translatef' is used to translate the current
-- model view matrix to advance the width of the character.

strokeCharacter :: StrokeFont -- ^ Stroke font to use.
                -> CInt       -- ^ Character to render (not confined to 8 bits).
                -> IO ()
strokeCharacter f i = flip glutStrokeCharacter i =<< marhshalStrokeFont f

foreign import ccall "glutStrokeCharacter" glutStrokeCharacter ::
   GLUTstrokeFont -> CInt -> IO ()

-- | Return the width in pixels of a stroke character in a supported stroke
-- font. While the width of characters in a font may vary (though fixed width
-- fonts do not vary), the maximum height characteristics of a particular font
-- are fixed.

strokeWidth :: StrokeFont -- ^ Stroke font to use.
            -> CInt       -- ^ Character to return width of (not confined to 8
                          --   bits).
            -> IO CInt    -- ^ Width in pixels.
strokeWidth f c = flip glutStrokeWidth c =<< marhshalStrokeFont f

foreign import ccall "glutStrokeWidth" glutStrokeWidth ::
   GLUTstrokeFont -> CInt -> IO CInt
