/* -----------------------------------------------------------------------------
 *
 * Module      :  C support for Graphics.UI.GLUT.Fonts
 * Copyright   :  (c) Sven Panne 2003
 * License     :  BSD-style (see the file libraries/GLUT/LICENSE)
 * 
 * Maintainer  :  sven_panne@yahoo.com
 * Stability   :  provisional
 * Portability :  portable
 *
 * -------------------------------------------------------------------------- */

#include "HsGLUT.h"

void*
hOpenGL_marshalBitmapFont(int fontID)
{
  switch (fontID) {
  case 0 : return GLUT_BITMAP_8_BY_13;
  case 1 : return GLUT_BITMAP_9_BY_15;
  case 2 : return GLUT_BITMAP_TIMES_ROMAN_10;
  case 3 : return GLUT_BITMAP_TIMES_ROMAN_24;
  case 4 : return GLUT_BITMAP_HELVETICA_10;
  case 5 : return GLUT_BITMAP_HELVETICA_12;
  case 6 : return GLUT_BITMAP_HELVETICA_18;
  }
  return (void*)0;
}

void*
hOpenGL_marshalStrokeFont(int fontID)
{
  switch (fontID) {
  case 0 : return GLUT_STROKE_ROMAN;
  case 1 : return GLUT_STROKE_MONO_ROMAN;
  }
  return (void*)0;
}
