{-
   Minmax.hs (adapted from minmax.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Determine the minimum and maximum values of a group of pixels. This
   demonstrates use of the minmax function.
-}

import Foreign ( allocaArray, peekArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import ReadImage

myInit :: IO ()
myInit = do
   rowAlignment Unpack $= 1
   clearColor $= Color4 0 0 0 0
   minmax $= Just (RGB', PassThrough)

display :: Size -> PixelData a -> DisplayCallback
display size pixels = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   rasterPos2i (Vertex2 1 1)
   drawPixels size pixels
   flush

   [Color3 minR minG minB, Color3 maxR maxG maxB] <- allocaArray 2 $ \buf -> do
      getMinmax Reset (PixelData RGB UnsignedByte buf)
      peekArray 2 buf :: IO [Color3 GLubyte]
   putStrLn (" Red   : min = " ++ show minR ++ "   max = " ++ show maxR)
   putStrLn (" Green : min = " ++ show minG ++ "   max = " ++ show maxG)
   putStrLn (" Blue  : min = " ++ show minB ++ "   max = " ++ show maxB)

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
