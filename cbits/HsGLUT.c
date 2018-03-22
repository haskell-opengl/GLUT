/* -----------------------------------------------------------------------------
 *
 * Module      :  C support for Graphics.UI.GLUT.Raw
 * Copyright   :  (c) Sven Panne 2002-2018
 * License     :  BSD3
 *
 * Maintainer  :  Sven Panne <svenpanne@gmail.com>
 * Stability   :  stable
 * Portability :  portable
 *
 * -------------------------------------------------------------------------- */

#if defined(USE_GETPROCADDRESS)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static LPCTSTR libNames[] = {
  /* Try to load freeglut first, it has a few extra features compared to classic
     GLUT. */
  TEXT("freeglut"),
  /* The MinGW-w64 version of freeglut prefixes "lib" onto the DLL name. */
  TEXT("libfreeglut"),
  /* If no freeglut version is found, try plain old glut32 instead. */
  TEXT("glut32")
};

void*
hs_GLUT_getProcAddress(const char *name)
{
  static int firstTime = 1;
  static HMODULE handle = NULL;

  if (firstTime) {
    int i, numNames = (int)(sizeof(libNames) / sizeof(libNames[0]));
    firstTime = 0;
    for (i = 0;   (!handle) && (i < numNames);   ++i) {
      handle = LoadLibrary(libNames[i]);
    }
  }

  return handle ? GetProcAddress(handle, name) : NULL;
}

/* -------------------------------------------------------------------------- */
#elif defined(USE_DLSYM)

#include <stdlib.h>
#include <dlfcn.h>

static const char* libNames[] = {
#ifdef __APPLE__
  /* Try to use freeglut, checking the LD_LIBRARY_PATH */
  "libglut.dylib",
  /* Try public framework path first. */
  "/Library/Frameworks/GLUT.framework/GLUT",
  /* If the public path failed, try the system framework path. */
  "/System/Library/Frameworks/GLUT.framework/GLUT"
#else
  "libglut.so", "libglut.so.3"
#endif
};

void*
hs_GLUT_getProcAddress(const char *name)
{
  static int firstTime = 1;
  static void *handle = NULL;

  if (firstTime) {
    int i, numNames = (int)(sizeof(libNames) / sizeof(libNames[0]));
    firstTime = 0;
    for (i = 0;   (!handle) && (i < numNames);   ++i) {
      handle = dlopen(libNames[i], RTLD_LAZY | RTLD_GLOBAL);
    }
  }

  return handle ? dlsym(handle, name) : NULL;
}

/* -------------------------------------------------------------------------- */
#else

#error "Don't know how to retrieve GLUT entries"

#endif

/* -------------------------------------------------------------------------- */

/* Note: This #if below is in sync with freeglut_std.h, classic GLUT simply used
   #if defined(_WIN32). */
#if defined(_MSC_VER) || defined(__CYGWIN__) || defined(__MINGW32__) || defined(__WATCOMC__)
#define INIT_FONT(name,num) hs_##name = ((void*)(num))
#else
#define INIT_FONT(name,num) hs_##name = hs_GLUT_getProcAddress(#name)
#endif

void*
hs_GLUT_marshalBitmapFont(int fontID)
{
  static int firstTime = 1;
  static void *hs_glutBitmap9By15        = NULL;
  static void *hs_glutBitmap8By13        = NULL;
  static void *hs_glutBitmapTimesRoman10 = NULL;
  static void *hs_glutBitmapTimesRoman24 = NULL;
  static void *hs_glutBitmapHelvetica10  = NULL;
  static void *hs_glutBitmapHelvetica12  = NULL;
  static void *hs_glutBitmapHelvetica18  = NULL;

  if (firstTime) {
    firstTime = 0;

    INIT_FONT(glutBitmap9By15, 0x0002);
    INIT_FONT(glutBitmap8By13, 0x0003);
    INIT_FONT(glutBitmapTimesRoman10, 0x0004);
    INIT_FONT(glutBitmapTimesRoman24, 0x0005);
    INIT_FONT(glutBitmapHelvetica10, 0x0006);
    INIT_FONT(glutBitmapHelvetica12, 0x0007);
    INIT_FONT(glutBitmapHelvetica18, 0x0008);
  }

  switch (fontID) {
  case 0 : return hs_glutBitmap8By13;
  case 1 : return hs_glutBitmap9By15;
  case 2 : return hs_glutBitmapTimesRoman10;
  case 3 : return hs_glutBitmapTimesRoman24;
  case 4 : return hs_glutBitmapHelvetica10;
  case 5 : return hs_glutBitmapHelvetica12;
  case 6 : return hs_glutBitmapHelvetica18;
  }
  return (void*)0;
}

void*
hs_GLUT_marshalStrokeFont(int fontID)
{
  static int firstTime = 1;
  static void *hs_glutStrokeRoman     = NULL;
  static void *hs_glutStrokeMonoRoman = NULL;

  if (firstTime) {
    firstTime = 0;

    INIT_FONT(glutStrokeRoman, 0x0000);
    INIT_FONT(glutStrokeMonoRoman, 0x0001);
  }

  switch (fontID) {
  case 0 : return hs_glutStrokeRoman;
  case 1 : return hs_glutStrokeMonoRoman;
  }
  return (void*)0;
}
