-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.GLUT.Extensions
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for handling an OpenGL-like extension
-- mechanism for GLUT.
--
--------------------------------------------------------------------------------

module Graphics.UI.GLUT.Extensions (
   FunPtr, unsafePerformIO,
   Invoker, getProcAddress, getProcAddressInternal   -- used only internally
) where

import Foreign.C.String ( CString, withCString )
import Foreign.Ptr ( FunPtr, nullFunPtr )
import System.IO.Unsafe ( unsafePerformIO )

--------------------------------------------------------------------------------

type Invoker a = FunPtr a -> a

getProcAddress :: String -> String -> IO (FunPtr a)
getProcAddress ext call =
   throwIfNull ("unknown GLUT call " ++ call ++ ", check for " ++ ext) $
      getProcAddressInternal call

throwIfNull :: String -> IO (FunPtr a) -> IO (FunPtr a)
throwIfNull msg act = do
   res <- act
   if res == nullFunPtr
      then ioError (userError msg)
      else return res

getProcAddressInternal :: String -> IO (FunPtr a)
getProcAddressInternal call = withCString call hs_GLUT_getProcAddress

foreign import CALLCONV unsafe "hs_GLUT_getProcAddress" hs_GLUT_getProcAddress
   :: CString -> IO (FunPtr a)
