{-
   Image.hs  (adapted from image.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2004 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates drawing pixels and shows the effect of
   drawPixels, copyPixels, and pixelZoom.

   Interaction: moving the mouse while pressing the mouse button will copy
   the image in the lower-left corner of the window to the mouse position,
   using the current pixel zoom factors. There is no attempt to prevent you
   from drawing over the original image. If you press the 'r' key, the
   original image and zoom factors are reset. If you press the 'z' or 'Z'
   keys, you change the zoom factors.
-}

import Data.Bits ( (.&.) )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )
import Foreign ( newArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- Create checkerboard image
checkImageSize :: Size
checkImageSize = Size 64 64

makeCheckImage :: IO (PixelData (Color3 GLubyte))
makeCheckImage = do
   let Size w h = checkImageSize
   buf <- newArray [ Color3 c c c |
                     i <- [ 0 .. w - 1 ],
                     j <- [ 0 .. h - 1 ],
                     let c | (i .&. 0x8) == (j .&. 0x8) = 0
                           | otherwise                  = 255 ]
   return $ PixelData RGB UnsignedByte buf

myInit :: IO (PixelData (Color3 GLubyte))
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   rowAlignment Unpack $= 1
   makeCheckImage

display ::  PixelData a -> DisplayCallback
display pixelData = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   rasterPos2i (Vertex2 0 0)
   drawPixels checkImageSize pixelData
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
   matrixMode $= Modelview 0
   loadIdentity

motion :: IORef GLfloat -> MotionCallback
motion zoomFactor (Position x y) = do
   Size _ height <- get windowSize
   let screenY = height - y
   -- resolve overloading, not needed in "real" programs
   let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   rasterPos2i (Vertex2 x screenY)
   z <- readIORef zoomFactor
   pixelZoom $= (z, z)
   copyPixels (Position 0 0) checkImageSize CopyColor
   pixelZoom $= (1, 1)
   flush

resetZoomFactor :: IORef GLfloat -> IO ()
resetZoomFactor zoomFactor = do
   writeIORef zoomFactor 1.0
   postRedisplay Nothing
   putStrLn "zoomFactor reset to 1.0"

incZoomFactor :: IORef GLfloat -> GLfloat -> IO ()
incZoomFactor zoomFactor inc = do
   modifyIORef zoomFactor (max 0.5 . min 3.0 . (+ inc))
   readIORef zoomFactor >>= putStrLn . ("zoomFactor is now " ++) . show

keyboard :: IORef GLfloat -> KeyboardMouseCallback
keyboard zoomFactor (Char 'r')   Down _   _ = resetZoomFactor zoomFactor
keyboard zoomFactor (Char 'R')   Down _   _ = resetZoomFactor zoomFactor
keyboard zoomFactor (Char 'z')   Down _   _ = incZoomFactor   zoomFactor   0.5
keyboard zoomFactor (Char 'Z')   Down _   _ = incZoomFactor   zoomFactor (-0.5)
keyboard _          (Char '\27') Down _   _ = exitWith ExitSuccess
keyboard _          _            _    _   _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   createWindow progName
   checkImage <- myInit
   displayCallback $= display checkImage
   reshapeCallback $= Just reshape
   zoomFactor <- newIORef 1.0
   keyboardMouseCallback $= Just (keyboard zoomFactor)
   motionCallback $= Just (motion zoomFactor)
   mainLoop
