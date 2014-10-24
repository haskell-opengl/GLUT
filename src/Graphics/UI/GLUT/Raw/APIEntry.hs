{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Raw.APIEntry
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling an OpenGL-like extension
-- mechanism for GLUT.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Raw.APIEntry (
   Invoker, getAPIEntry, getAPIEntryInternal,
   FunPtr, unsafePerformIO
) where

import Foreign.C.String
import Foreign.Marshal.Error
import Foreign.Ptr
import System.IO.Unsafe

#ifdef __HUGS__
{-# CFILES cbits/HsGLUT.c #-}
#endif

--------------------------------------------------------------------------------

type Invoker a = FunPtr a -> a

-- | Retrieve a GLUT API entry by name. Throws a userError when no entry with
-- the given name was found.
getAPIEntry :: String -> IO (FunPtr a)
getAPIEntry extensionEntry =
   throwIfNullFunPtr ("unknown GLUT entry " ++ extensionEntry) $
      getAPIEntryInternal extensionEntry

throwIfNullFunPtr :: String -> IO (FunPtr a) -> IO (FunPtr a)
throwIfNullFunPtr = throwIf (== nullFunPtr) . const

getAPIEntryInternal :: String -> IO (FunPtr a)
getAPIEntryInternal extensionEntry =
   withCString extensionEntry hs_GLUT_getProcAddress

foreign import ccall unsafe "hs_GLUT_getProcAddress"
   hs_GLUT_getProcAddress :: CString -> IO (FunPtr a)
