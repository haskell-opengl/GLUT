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

#ifndef HSGLUT_H
#define HSGLUT_H

#include "config.h"

#ifdef USE_QUARTZ_OPENGL /* Mac OS X only */
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

extern void* hOpenGL_marshalBitmapFont(int fontID);
extern void* hOpenGL_marshalStrokeFont(int fontID);

#endif
