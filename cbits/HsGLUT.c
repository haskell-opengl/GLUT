/* -----------------------------------------------------------------------------
 *
 * Module      :  C support for Graphics.UI.GLUT.Raw
 * Copyright   :  (c) Sven Panne 2002-2016
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

void*
hs_GLUT_getProcAddress(const char *name)
{
  static int firstTime = 1;
  static HMODULE handle = NULL;

  if (firstTime) {
    firstTime = 0;

    /* Try to load freeglut first, it has a few extra features compared to
       classic GLUT. */
    handle = LoadLibrary(TEXT("freeglut"));

    /* The MinGW-w64 version of freeglut prefixes "lib" onto the DLL name. */
    if (!handle) {
      handle = LoadLibrary(TEXT("libfreeglut"));
    }

    /* If no freeglut version is found, try plain old glut32 instead. */
    if (!handle) {
      handle = LoadLibrary(TEXT("glut32"));
    }
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
    handle = dlopen("libglut.so", RTLD_LAZY);
  }

  return handle ? dlsym(handle, name) : NULL;
}

/* -------------------------------------------------------------------------- */
#else

#error "Don't know how to retrieve GLUT entries"

#endif

/* -------------------------------------------------------------------------- */
#if defined(_MSC_VER) || defined(__CYGWIN__) || defined(__MINGW32__) || defined(__WATCOMC__)

void*
hs_GLUT_glutStrokeRoman(void)
{
  return ((void *)0x0000);
}

void*
hs_GLUT_glutStrokeMonoRoman(void)
{
  return ((void *)0x0001);
}

void*
hs_GLUT_glutBitmap9By15(void)
{
  return ((void *)0x0002);
}

void*
hs_GLUT_glutBitmap8By13(void)
{
  return ((void *)0x0003);
}

void*
hs_GLUT_glutBitmapTimesRoman10(void)
{
  return ((void *)0x0004);
}

void*
hs_GLUT_glutBitmapTimesRoman24(void)
{
  return ((void *)0x0005);
}

void*
hs_GLUT_glutBitmapHelvetica10(void)
{
  return ((void *)0x0006);
}

void*
hs_GLUT_glutBitmapHelvetica12(void)
{
  return ((void *)0x0007);
}

void*
hs_GLUT_glutBitmapHelvetica18(void)
{
  return ((void *)0x0008);
}

#else

void*
hs_GLUT_glutStrokeRoman(void)
{
  return hs_GLUT_getProcAddress("glutStrokeRoman");
}

void*
hs_GLUT_glutStrokeMonoRoman(void)
{
  return hs_GLUT_getProcAddress("glutStrokeMonoRoman");
}

void*
hs_GLUT_glutBitmap9By15(void)
{
  return hs_GLUT_getProcAddress("glutBitmap9By15");
}

void*
hs_GLUT_glutBitmap8By13(void)
{
  return hs_GLUT_getProcAddress("glutBitmap8By13");
}

void*
hs_GLUT_glutBitmapTimesRoman10(void)
{
  return hs_GLUT_getProcAddress("glutBitmapTimesRoman10");
}

void*
hs_GLUT_glutBitmapTimesRoman24(void)
{
  return hs_GLUT_getProcAddress("glutBitmapTimesRoman24");
}

void*
hs_GLUT_glutBitmapHelvetica10(void)
{
  return hs_GLUT_getProcAddress("glutBitmapHelvetica10");
}

void*
hs_GLUT_glutBitmapHelvetica12(void)
{
  return hs_GLUT_getProcAddress("glutBitmapHelvetica12");
}

void*
hs_GLUT_glutBitmapHelvetica18(void)
{
  return hs_GLUT_getProcAddress("glutBitmapHelvetica18");
}

#endif

void*
hs_GLUT_marshalBitmapFont(int fontID)
{
  static int firstTime = 1;
  static void *h_glutBitmap9By15        = NULL;
  static void *h_glutBitmap8By13        = NULL;
  static void *h_glutBitmapTimesRoman10 = NULL;
  static void *h_glutBitmapTimesRoman24 = NULL;
  static void *h_glutBitmapHelvetica10  = NULL;
  static void *h_glutBitmapHelvetica12  = NULL;
  static void *h_glutBitmapHelvetica18  = NULL;


  if (firstTime) {
    firstTime = 0;

    h_glutBitmap9By15        = hs_GLUT_glutBitmap9By15();
    h_glutBitmap8By13        = hs_GLUT_glutBitmap8By13();
    h_glutBitmapTimesRoman10 = hs_GLUT_glutBitmapTimesRoman10();
    h_glutBitmapTimesRoman24 = hs_GLUT_glutBitmapTimesRoman24();
    h_glutBitmapHelvetica10  = hs_GLUT_glutBitmapHelvetica10();
    h_glutBitmapHelvetica12  = hs_GLUT_glutBitmapHelvetica12();
    h_glutBitmapHelvetica18  = hs_GLUT_glutBitmapHelvetica18();
  }

  switch (fontID) {
  case 0 : return h_glutBitmap8By13;
  case 1 : return h_glutBitmap9By15;
  case 2 : return h_glutBitmapTimesRoman10;
  case 3 : return h_glutBitmapTimesRoman24;
  case 4 : return h_glutBitmapHelvetica10;
  case 5 : return h_glutBitmapHelvetica12;
  case 6 : return h_glutBitmapHelvetica18;
  }
  return (void*)0;
}

void*
hs_GLUT_marshalStrokeFont(int fontID)
{
  static int firstTime = 1;
  static void *h_glutStrokeRoman     = NULL;
  static void *h_glutStrokeMonoRoman = NULL;

  if (firstTime) {
    firstTime = 0;

    h_glutStrokeRoman     = hs_GLUT_glutStrokeRoman();
    h_glutStrokeMonoRoman = hs_GLUT_glutStrokeMonoRoman();
  }

  switch (fontID) {
  case 0 : return h_glutStrokeRoman;
  case 1 : return h_glutStrokeMonoRoman;
  }
  return (void*)0;
}
