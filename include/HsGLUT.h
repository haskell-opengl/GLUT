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
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if HAVE_GL_GLUT_H
#include <GL/glut.h>
#else
#include "glut_local.h"
#endif
#endif

extern void* hOpenGL_marshalBitmapFont(int fontID);
extern void* hOpenGL_marshalStrokeFont(int fontID);

#endif
