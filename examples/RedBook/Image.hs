{-
   Image.hs  (adapted from image.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
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
import Data.IORef ( IORef, newIORef )
import Foreign ( newArray )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { zoomFactor :: IORef GLfloat }

makeState :: IO State
makeState = do
   z <- newIORef 1
   return $ State { zoomFactor = z }

-- Create checkerboard image
checkImageSize :: Size
checkImageSize = Size 64 64

type Image = PixelData (Color3 GLubyte)

makeCheckImage :: Size -> GLsizei -> (GLubyte -> (Color3 GLubyte)) -> IO Image
makeCheckImage (Size w h) n f =
   fmap (PixelData RGB UnsignedByte) $
      newArray [ f c |
                 i <- [ 0 .. w - 1 ],
                 j <- [ 0 .. h - 1 ],
                 let c | (i .&. n) == (j .&. n) = 0
                       | otherwise              = 255 ]

myInit :: IO Image
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   rowAlignment Unpack $= 1
   makeCheckImage checkImageSize 0x8 (\c -> Color3 c c c)

display ::  Image -> DisplayCallback
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

motion :: State -> MotionCallback
motion state (Position x y) = do
   Size _ height <- get windowSize
   let screenY = height - y
   -- resolve overloading, not needed in "real" programs
   let rasterPos2i = rasterPos :: Vertex2 GLint -> IO ()
   rasterPos2i (Vertex2 x screenY)
   z <- get (zoomFactor state)
   pixelZoom $= (z, z)
   copyPixels (Position 0 0) checkImageSize CopyColor
   pixelZoom $= (1, 1)
   flush

resetZoomFactor :: State -> IO ()
resetZoomFactor state = do
   zoomFactor state $= 1.0
   postRedisplay Nothing
   putStrLn "zoomFactor reset to 1.0"

incZoomFactor :: State -> GLfloat -> IO ()
incZoomFactor state inc = do
   zoomFactor state $~! (max 0.5 . min 3.0 . (+ inc))
   get (zoomFactor state) >>= putStrLn . ("zoomFactor is now " ++) . show

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char 'r')   Down _   _ = resetZoomFactor state
keyboard state (Char 'R')   Down _   _ = resetZoomFactor state
keyboard state (Char 'z')   Down _   _ = incZoomFactor   state   0.5
keyboard state (Char 'Z')   Down _   _ = incZoomFactor   state (-0.5)
keyboard _     (Char '\27') Down _   _ = exitWith ExitSuccess
keyboard _     _            _    _   _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   createWindow progName
   state <- makeState
   checkImage <- myInit
   displayCallback $= display checkImage
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   motionCallback $= Just (motion state)
   mainLoop
