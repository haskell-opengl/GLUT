{-
   Robot.hs (adapted from robot.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2004 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

  This program shows how to composite modeling transformations to draw
  translated and rotated hierarchical models. Interaction: pressing the s
  and e keys (shoulder and elbow) alters the rotation of the robot arm.
-}

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

display :: IORef Int -> IORef Int -> DisplayCallback
display shoulder elbow = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
       scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
   preservingMatrix $ do
      translatef (Vector3 (-1) 0 0)
      s <- get shoulder
      rotate (fromIntegral s :: GLfloat) (Vector3 0 0 1)
      translatef (Vector3 1 0 0)
      preservingMatrix $ do
         scalef 2 0.4 1
         renderObject Wireframe (Cube 1)
      translatef (Vector3 1 0 0)
      e <- get elbow
      rotate (fromIntegral e :: GLfloat) (Vector3 0 0 1)
      translatef (Vector3 1 0 0)
      preservingMatrix $ do
         scalef 2 0.4 1
         renderObject Wireframe (Cube 1)
   swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 65 (fromIntegral w / fromIntegral h) 1 20
   matrixMode $= Modelview 0
   loadIdentity
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
   translatef (Vector3 0 0 (-5))

keyboard :: IORef Int -> IORef Int -> KeyboardMouseCallback
keyboard shoulder _     (Char 's')   Down _ _ = update shoulder   5
keyboard shoulder _     (Char 'S')   Down _ _ = update shoulder (-5)
keyboard _        elbow (Char 'e')   Down _ _ = update elbow      5
keyboard _        elbow (Char 'E')   Down _ _ = update elbow    (-5)
keyboard _        _     (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _        _     _            _    _ _ = return ()

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
   shoulder <- newIORef 0
   elbow <- newIORef 0
   displayCallback $= display shoulder elbow
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard shoulder elbow)
   mainLoop
