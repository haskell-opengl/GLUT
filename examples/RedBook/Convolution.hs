{-
   Convolution.hs (adapted from convolution.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Use various 2D convolutions filters to find edges in an image.
-}

import Foreign ( withArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import ReadImage

data Filter = Filter String [GLfloat]

filterTable :: [(Key,Filter)]
filterTable = [
   (Char 'h', Filter "horizontal" [ 0, -1, 0,
                                    0,  1, 0,
                                    0,  0, 0 ]),

   (Char 'v', Filter "vertical" [ 0, 0, 0,
                                 -1, 1, 0,
                                  0, 0, 0 ]),

   (Char 'l', Filter "laplacian" [ -0.125, -0.125, -0.125,
                                   -0.125,  1.0  , -0.125,
                                   -0.125, -0.125, -0.125 ])]

setFilter :: Filter -> IO ()
setFilter (Filter filterName filterData) = do
   putStrLn ("Using the " ++ filterName ++ " filter")
   withArray filterData $ \buf ->
      convolutionFilter2D Luminance' (Size 3 3) (PixelData Luminance Float buf)

myInit :: IO ()
myInit = do
   rowAlignment Unpack $= 1
   clearColor $= Color4 0 0 0 0
   setFilter (snd (head filterTable))
   convolution Convolution2D $= Enabled

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
keyboard key          Down _ _ =
   maybe (return ())
         (\f -> do setFilter f; postRedisplay Nothing)
         (lookup key filterTable)
keyboard _            _    _ _ = return ()

-- Main Loop: Open window with initial window size, title bar, RGBA display
-- mode, and handle input events.
main :: IO ()
main = do
   (progName, args) <- getArgsAndInitialize
   (size, pixels) <- readImage (if null args then "Data/leeds.bin" else head args)
   initialDisplayMode $= [ SingleBuffered, RGBAMode ]
   initialWindowSize $= size
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= display size pixels
   mainLoop
