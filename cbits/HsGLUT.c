/* -----------------------------------------------------------------------------
 *
 * Module      :  C support for Graphics.UI.GLUT.Fonts
 * Copyright   :  (c) Sven Panne 2002-2005
 * License     :  BSD-style (see the file libraries/GLUT/LICENSE)
 * 
 * Maintainer  :  sven.panne@aedion.de
 * Stability   :  provisional
 * Portability :  portable
 *
 * -------------------------------------------------------------------------- */

#include "HsGLUT.h"

/* needed only for GLUT_GET_PROC_ADDRESS_IS_BROKEN */
#include "HsGLUTConfig.h"

#if (FREEGLUT || GLUT_API_VERSION >= 5) && GLUT_GET_PROC_ADDRESS_IS_BROKEN
#include <string.h>
#endif

void*
hs_GLUT_marshalBitmapFont(int fontID)
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
hs_GLUT_marshalStrokeFont(int fontID)
{
  switch (fontID) {
  case 0 : return GLUT_STROKE_ROMAN;
  case 1 : return GLUT_STROKE_MONO_ROMAN;
  }
  return (void*)0;
}

/* procName is really a const char*, but currently we can't specify this in
   Haskell's FFI and consequently get a warning from the C compiler. */
void*
hs_GLUT_getProcAddress(char *procName)
{
#if (FREEGLUT || GLUT_API_VERSION >= 5)
#if GLUT_GET_PROC_ADDRESS_IS_BROKEN
  /* There are a few typos/omissions in freeglut 2.20 */
  if (strcmp(procName, "glutWireCylinder"         ) == 0) return (void*)glutWireCylinder;
  if (strcmp(procName, "glutSolidCylinder"        ) == 0) return (void*)glutSolidCylinder;
  if (strcmp(procName, "glutWireSierpinskiSponge" ) == 0) return (void*)glutWireSierpinskiSponge;
  if (strcmp(procName, "glutSolidSierpinskiSponge") == 0) return (void*)glutSolidSierpinskiSponge;
#endif
  return glutGetProcAddress(procName);
#else
  return (void*)0;
#endif
}
