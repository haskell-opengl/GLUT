{-
   DrawF.hs  (adapted from drawf.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Draws the bitmapped letter F on the screen (several times).
   This demonstrates use of the bitmap call.
-}

import Foreign ( Ptr, newArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO (Ptr GLubyte)
myInit = do
   rowAlignment Unpack $= 1
   clearColor $= Color4 0 0 0 0
   newArray [
      0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00,
      0xff, 0x00, 0xff, 0x00, 0xc0, 0x00, 0xc0, 0x00, 0xc0, 0x00,
      0xff, 0xc0, 0xff, 0xc0 ]

display :: Ptr GLubyte -> DisplayCallback
display rasters = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   color3f (Color3 1 1 1)
   rasterPos2i (Vertex2 20 20)
   sequence_ $ replicate 3 $
      bitmap (Size 10 12) (Vertex2 0 0) (Vector2 11 0) rasters
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho 0 (fromIntegral w) 0 (fromIntegral h) (-1) 1
   matrixMode $= Modelview 0

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

-- Main Loop
-- Open window with initial window size, title bar, 
-- RGBA display mode, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 100 100
   initialWindowPosition $= Position 100 100
   createWindow progName
   rasters <- myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= display rasters
   mainLoop
