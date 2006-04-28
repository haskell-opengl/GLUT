{-
   Histogram.hs (adapted from histogram.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Compute the histogram of the image.  This program illustrates the use of
   the histogram function.
-}

import Control.Monad ( zipWithM_ )
import Foreign ( allocaArray, peekArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import ReadImage

histogramSize :: Int
histogramSize = 256   -- Must be a power of 2

myInit :: IO ()
myInit = do
   rowAlignment Unpack $= 1
   clearColor $= Color4 0 0 0 0

   histogram NoProxy $= Just (fromIntegral histogramSize, RGB', PassThrough)

display :: Size -> PixelData a -> DisplayCallback
display size pixels = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   rasterPos2i (Vertex2 1 1)
   drawPixels size pixels

   values <- allocaArray histogramSize $ \buf -> do
      -- Note: The example in the Red Book uses GL_UNSIGNED_SHORT here,
      -- but we are more honest by using Short...
      getHistogram Reset (PixelData RGB Short buf)
      peekArray histogramSize buf

   -- Plot histogram
   zipWithM_ (plotHistogram values)
             [  Color3 1 0 0,         Color3 0 1 0,         Color3 0 0 1       ]
             [\(Color3 r _ _) -> r, \(Color3 _ g _) -> g, \(Color3 _ _ b) -> b ]
   flush

plotHistogram ::
   [Color3 GLshort] -> Color3 GLfloat -> (Color3 GLshort -> GLshort) -> IO ()
plotHistogram values c selectComponent =
   renderPrimitive LineStrip $ do
      color c
      zipWithM_ (\i h -> vertex (Vertex2 i (selectComponent h))) [ 0 .. ] values

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
   postRedisplay Nothing
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
