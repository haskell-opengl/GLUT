/* -----------------------------------------------------------------------------
 *
 * Module      :  C support for Graphics.UI.GLUT.Raw
 * Copyright   :  (c) Sven Panne 2002-2013
 * License     :  BSD3
 *
 * Maintainer  :  Sven Panne <svenpanne@gmail.com>
 * Stability   :  stable
 * Portability :  portable
 *
 * -------------------------------------------------------------------------- */

#if defined(_MSC_VER) || defined(__CYGWIN__) || defined(__MINGW32__) || defined(__WATCOMC__)

#define GLUT_STROKE_ROMAN          ((void *)0x0000)
#define GLUT_STROKE_MONO_ROMAN     ((void *)0x0001)
#define GLUT_BITMAP_9_BY_15        ((void *)0x0002)
#define GLUT_BITMAP_8_BY_13        ((void *)0x0003)
#define GLUT_BITMAP_TIMES_ROMAN_10 ((void *)0x0004)
#define GLUT_BITMAP_TIMES_ROMAN_24 ((void *)0x0005)
#define GLUT_BITMAP_HELVETICA_10   ((void *)0x0006)
#define GLUT_BITMAP_HELVETICA_12   ((void *)0x0007)
#define GLUT_BITMAP_HELVETICA_18   ((void *)0x0008)

#else

extern void* glutStrokeRoman;
extern void* glutStrokeMonoRoman;
extern void* glutBitmap9By15;
extern void* glutBitmap8By13;
extern void* glutBitmapTimesRoman10;
extern void* glutBitmapTimesRoman24;
extern void* glutBitmapHelvetica10;
extern void* glutBitmapHelvetica12;
extern void* glutBitmapHelvetica18;

#define GLUT_STROKE_ROMAN          ((void *)&glutStrokeRoman)
#define GLUT_STROKE_MONO_ROMAN     ((void *)&glutStrokeMonoRoman)
#define GLUT_BITMAP_9_BY_15        ((void *)&glutBitmap9By15)
#define GLUT_BITMAP_8_BY_13        ((void *)&glutBitmap8By13)
#define GLUT_BITMAP_TIMES_ROMAN_10 ((void *)&glutBitmapTimesRoman10)
#define GLUT_BITMAP_TIMES_ROMAN_24 ((void *)&glutBitmapTimesRoman24)
#define GLUT_BITMAP_HELVETICA_10   ((void *)&glutBitmapHelvetica10)
#define GLUT_BITMAP_HELVETICA_12   ((void *)&glutBitmapHelvetica12)
#define GLUT_BITMAP_HELVETICA_18   ((void *)&glutBitmapHelvetica18)

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

/* -------------------------------------------------------------------------- */
#if defined(USE_GETPROCADDRESS)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

void*
hs_GLUT_getProcAddress(const char *name)
{
  static int firstTime = 1;
  static HMODULE handle = NULL;

  if (firstTime) {
    firstTime = 0;
    handle = LoadLibrary(TEXT("glut32"));
  }

  return handle ? GetProcAddress(handle, name) : NULL;
}

/* -------------------------------------------------------------------------- */
#elif defined(USE_NSADDRESSOFSYMBOL)

#include <mach-o/dyld.h>
#include <stdlib.h>
#include <string.h>

void*
hs_GLUT_getProcAddress(const char *name)
{
  NSSymbol symbol;

  /* Prepend a '_' for the Unix C symbol mangling convention */
  char* symbolName = (char*)malloc(strlen(name) + 2);
  if (!symbolName) {
    return NULL;
  }
  symbolName[0] = '_';
  strcpy(symbolName + 1, name);

  if (!NSIsSymbolNameDefined(symbolName)) {
    free(symbolName);
    return NULL;
  }

  symbol = NSLookupAndBindSymbol(symbolName);
  free(symbolName);
  if (!symbol) {
    return NULL;
  }

  return NSAddressOfSymbol(symbol);
}

/* -------------------------------------------------------------------------- */
#elif defined(USE_DLSYM)

#include <stdlib.h>
#include <dlfcn.h>

void*
hs_GLUT_getProcAddress(const char *name)
{
  static int firstTime = 1;
  static void *handle = NULL;

  if (firstTime) {
    firstTime = 0;
    /* Get a handle for our executable. */
    handle = dlopen(NULL, RTLD_LAZY);
  }

  return handle ? dlsym(handle, name) : NULL;
}

/* -------------------------------------------------------------------------- */
#else

#error "Don't know how to retrieve GLUT entries"

#endif
