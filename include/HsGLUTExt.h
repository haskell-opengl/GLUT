/* -----------------------------------------------------------------------------
 *
 * Module      :  GLUT extension support for Graphics.UI.GLUT
 * Copyright   :  (c) Sven Panne 2002-2005
 * License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
 * 
 * Maintainer  :  sven.panne@aedion.de
 * Stability   :  provisional
 * Portability :  portable
 *
 * This header should only define preprocessor macros!
 *
 * -------------------------------------------------------------------------- */

#ifndef HSGLUTEXT_H
#define HSGLUTEXT_H

/* NOTE: The macro must immediately start with the foreign declaration,
   otherwise the magic mangler (hack_foreign) in the Hugs build system
   doesn't recognize it. */
#define EXTENSION_ENTRY(_saftey,_msg,_entry,_ty)			\
foreign import CALLCONV _saftey "dynamic" dyn_/**/_entry :: Graphics.UI.GLUT.Extensions.Invoker (_ty) ; \
_entry :: (_ty) ; \
_entry = dyn_/**/_entry ptr_/**/_entry ; \
ptr_/**/_entry :: FunPtr a ; \
ptr_/**/_entry = unsafePerformIO (Graphics.UI.GLUT.Extensions.getProcAddress (_msg) ("_entry")) ; \
{-# NOINLINE ptr_/**/_entry #-}

#endif
