{-
   FogCoordinate.hs (adapted from smooth.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2003 <sven_panne@yahoo.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates per-vertex fog coordinates.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO ()
myInit = do
   let c = Color4 0.5 0.5 0.5 1.0
   clearColor $= c
   fogColor $= c
   fogMode $= Exp2 0.4;
   fogCoordSrc $= FogCoord
   shadeModel $= Smooth

triangle :: IO ()
triangle =
   -- resolve overloading, not needed in "real" programs
   let vertex2f = vertex :: Vertex2 GLfloat -> IO ()
       fogCoordf = fogCoord :: FogCoord1 GLfloat -> IO ()
       color3f = color :: Color3 GLfloat -> IO ()
   in renderPrimitive Triangles $ do
      color3f (Color3 1 0 0)
      fogCoordf (FogCoord1 0)
      vertex2f (Vertex2 5 5)
      color3f (Color3 0 1 0)
      fogCoordf (FogCoord1 3)
      vertex2f (Vertex2 12.5 5)
      color3f (Color3 0 0 1)
      fogCoordf (FogCoord1 7)
      vertex2f (Vertex2 5 25)

display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   fog $= Disabled
   triangle
   preservingMatrix $ do
      let translatef = translate :: Vector3 GLfloat -> IO ()
      translatef (Vector3 12.5 0 0)
      fog $= Enabled
      triangle
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho2D 0 30 0 (30 * hf/wf)
      else ortho2D 0 (30 * wf/hf) 0 30
   matrixMode $= Modelview 0

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
