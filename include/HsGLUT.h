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

#include <GL/glut.h>

extern void* hOpenGL_marshalBitmapFont(int fontID);
extern void* hOpenGL_marshalStrokeFont(int fontID);

#endif
