{-
   TexProx.hs (adapted from texprox.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   The brief program illustrates use of texture proxies. This program only
   prints out some messages about whether certain size textures are supported
   and then exits.
-}

import Control.Monad ( when )
import Foreign.Ptr ( nullPtr )
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO ()
myInit = do
   let check = do
          ok <- get (textureProxyOK (Left Texture2D) 0)
          putStrLn ("proxy allocation " ++ if ok then "succeeded" else "failed")

   texImage2D Nothing Proxy 0 RGBA8 (TextureSize2D 64 64) 0 (PixelData RGBA UnsignedByte nullPtr)
   check

   -- Note: We use a larger texture size here to demonstrate failure,
   -- modern graphic cards can handle the original size.
   texImage2D Nothing Proxy 0 RGBA16 (TextureSize2D 8192 8192) 0 (PixelData RGBA UnsignedShort nullPtr)
   check


display :: DisplayCallback
display = exitWith ExitSuccess

reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   -- we have to do this *after* createWindow, otherwise we have no OpenGL context
   version <- get glVersion
   when (take 3 version == "1.0") $ do
      putStrLn "This program demonstrates a feature which is not in OpenGL Version 1.0."
      putStrLn "If your implementation of OpenGL Version 1.0 has the right extensions,"
      putStrLn "you may be able to modify this program to make it run."
      exitFailure
   myInit
   displayCallback $= display
   reshapeCallback $= Just reshape
   mainLoop
