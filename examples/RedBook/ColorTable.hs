{-
   ColorTable.hs (adapted from colortable.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Invert a passed block of pixels.  This program illustrates the use of the
   colorTable function.
-}

import Foreign ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import ReadImage

myInit :: IO ()
myInit = do
   rowAlignment Unpack $= 1
   clearColor $= Color4 0 0 0 0
   -- Set up an inverse color table
   let tableSize = 256
       t = fromIntegral (tableSize - 1) :: GLubyte
   withArray [ Color3 i i i | i <- [ t, t - 1 .. 0 ] ] $ \buf ->
      colorTable NoProxy ColorTable RGB' tableSize (PixelData RGB UnsignedByte buf)
   colorTableStage ColorTableStage $= Enabled

display :: Size -> PixelData a -> DisplayCallback
display size pixels = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   rasterPos2i (Vertex2 1 1)
   drawPixels size pixels
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

-- Main Loop: Open window with initial window size, title bar, RGBA display
-- mode, and handle input events.
main :: IO ()
main = do
   (progName, args) <- getArgsAndInitialize
   (size, pixels) <- readImage (if null args then "Data/leeds.bin" else head args)
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= size
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= display size pixels
   mainLoop
