{-
   Planet.hs (adapted from planet.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2004 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program shows how to composite modeling transformations to draw
   translated and rotated models. Interaction: pressing the d and y keys
   (day and year) alters the rotation of the planet around the sun.
-}

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

display :: IORef Int -> IORef Int -> DisplayCallback
display year day = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       translatef = translate :: Vector3 GLfloat -> IO ()
   color3f (Color3 1 1 1)

   preservingMatrix $ do
      renderObject Wireframe (Sphere' 1 20 16)   -- draw sun
      y <- get year
      rotate (fromIntegral y :: GLfloat) (Vector3 0 1 0)
      translatef (Vector3 2 0 0)
      d <- get day
      rotate (fromIntegral d :: GLfloat) (Vector3 0 1 0)
      renderObject Wireframe (Sphere' 0.2 10 8)  -- draw smaller planet

   swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 60 (fromIntegral w / fromIntegral h) 1 20
   matrixMode $= Modelview 0
   loadIdentity
   lookAt (Vertex3 0 0 5) (Vertex3 0 0 0) (Vector3 0 1 0)

keyboard :: IORef Int -> IORef Int -> KeyboardMouseCallback
keyboard day _    (Char 'd')   Down _ _ = update day    10
keyboard day _    (Char 'D')   Down _ _ = update day  (-10)
keyboard _   year (Char 'y')   Down _ _ = update year    5
keyboard _   year (Char 'Y')   Down _ _ = update year ( -5)
keyboard _   _    (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _   _    _            _    _ _ = return ()

update :: IORef Int -> Int -> IO ()
update angle inc = do
   angle $~ ((`mod` 360) . (+ inc))
   postRedisplay Nothing

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   year <- newIORef 0
   day <- newIORef 0
   displayCallback $= display year day
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard year day)
   mainLoop
