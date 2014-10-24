{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Raw.Fonts
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Our own functions to access font identifiers in a portable way.
--
-----------------------------------------------------------------------------

module Graphics.UI.GLUT.Raw.Fonts (
   BitmapFont(..), GLUTbitmapFont, marshalBitmapFont,
   StrokeFont(..), GLUTstrokeFont, marshalStrokeFont
) where

import Foreign.C.Types
import Foreign.Ptr

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
   deriving ( Eq, Ord, Show )

-- Alas, fonts in GLUT are not denoted by some integral value, but by opaque
-- pointers on the C side. Even worse: For WinDoze, they are simply small ints,
-- casted to void*, for other platforms addresses of global variables are used.
-- And all is done via ugly #ifdef-ed #defines... Aaaaargl! So the only portable
-- way is using integers on the Haskell side and doing the marshaling via some
-- small C wrappers around those macros. *sigh*
type GLUTbitmapFont = Ptr ()

marshalBitmapFont :: BitmapFont -> IO GLUTbitmapFont
marshalBitmapFont x = case x of
   Fixed8By13 -> hs_GLUT_marshalBitmapFont 0
   Fixed9By15 -> hs_GLUT_marshalBitmapFont 1
   TimesRoman10 -> hs_GLUT_marshalBitmapFont 2
   TimesRoman24 -> hs_GLUT_marshalBitmapFont 3
   Helvetica10 -> hs_GLUT_marshalBitmapFont 4
   Helvetica12 -> hs_GLUT_marshalBitmapFont 5
   Helvetica18 -> hs_GLUT_marshalBitmapFont 6


foreign import ccall unsafe "hs_GLUT_marshalBitmapFont"
   hs_GLUT_marshalBitmapFont :: CInt -> IO (Ptr a)

--------------------------------------------------------------------------------

-- | The stroke fonts available in GLUT.
data StrokeFont
   = Roman     -- ^ A proportionally spaced Roman Simplex font for ASCII
               --   characters 32 through 127. The maximum top character in the
               --   font is 119.05 units; the bottom descends 33.33 units.
   | MonoRoman -- ^ A mono-spaced spaced Roman Simplex font (same characters as
               --   'Roman') for ASCII characters 32 through 127. The maximum
               --   top character in the font is 119.05 units; the bottom
               --   descends 33.33 units. Each character is 104.76 units wide.
   deriving ( Eq, Ord, Show )

-- Same remarks as for GLUTbitmapFont
type GLUTstrokeFont = Ptr ()

marshalStrokeFont :: StrokeFont -> IO GLUTstrokeFont
marshalStrokeFont x = case x of
   Roman -> hs_GLUT_marshalStrokeFont 0
   MonoRoman -> hs_GLUT_marshalStrokeFont 1

foreign import ccall unsafe "hs_GLUT_marshalStrokeFont"
   hs_GLUT_marshalStrokeFont :: CInt -> IO (Ptr a)
