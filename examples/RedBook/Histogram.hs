{-
   Histogram.hs (adapted from histogram.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2004 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Invert a passed block of pixels.  This program illustrates the use of the
   colorTable function.
-}

import Control.Monad
import Foreign
import Foreign.Marshal.Array
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import ReadImage

histogramSize :: GLsizei
histogramSize = 256   -- Must be a power of 2

myInit :: IO ()
myInit = do
   rowAlignment Unpack $= 1
   clearColor $= Color4 0 0 0 0

   histogram NoProxy $= Just (histogramSize, RGB', PassThrough)

display :: Size -> PixelData a -> DisplayCallback
display size pixels = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   rasterPos2i (Vertex2 1 1)
   drawPixels size pixels

   allocaArray (3 * fromIntegral histogramSize) $ \values -> do
      -- Note: The example in the Red Book uses GL_UNSIGNED_SHORT
      -- here, but we are more honest by using Short...
      getHistogram Reset (PixelData RGB Short values)

      -- Plot histogram
      zipWithM_ (plotColor values)
                [ Color3 1 0 0, Color3 0 1 0, Color3 0 0 1 ]
                [ 0 .. ]
   flush

plotColor :: Ptr GLshort -> Color3 GLfloat -> Int -> IO ()
plotColor values c off = do
   renderPrimitive LineStrip $ do
      color c
      mapM_ (\i -> do v <- peekElemOff values (off + 3 * fromIntegral i)
                      vertex (Vertex2 i v))
            [ 0 .. (fromIntegral histogramSize :: GLshort) - 1 ]

reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho 0 (fromIntegral histogramSize) 0 10000 (-1) 1
   matrixMode $= Modelview 0

keyboard :: KeyboardMouseCallback
keyboard (Char 's')   Down _ _ = do
   Just (width, format, sink) <- get (histogram NoProxy)
   let newSink = if sink == Sink then PassThrough else Sink
   histogram NoProxy $= Just (width, format, newSink)
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
