{-
   Scene.hs (adapted from scene.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates the use of the GL lighting model. Objects are
   drawn using a grey material characteristic. A single light source
   illuminates the objects.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- Initialize material property and light source.
myInit :: IO ()
myInit = do
   ambient (Light 0) $= Color4 0 0 0 1
   diffuse (Light 0) $= Color4 1 1 1 1
   specular (Light 0) $= Color4 1 1 1 1
   -- light position is NOT default value
   position (Light 0) $= Vertex4  1 1 1 0

   lighting $= Enabled
   light (Light 0) $= Enabled
   depthFunc $= Just Less

display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      rotate (20 :: GLfloat) (Vector3 1 0 0)
      preservingMatrix $ do
         translate (Vector3 (-0.75) 0.5 (0 :: GLfloat))
         rotate (90 :: GLfloat) (Vector3 1 0 0)
         renderObject Solid (Torus 0.275 0.85 15 15)
      preservingMatrix $ do
         translate (Vector3 (-0.75) (-0.5) (0 :: GLfloat))
         rotate (270 :: GLfloat) (Vector3 1 0 0)
         renderObject Solid (Cone 1 2 15 15)
      preservingMatrix $ do
         translate (Vector3 0.75 0 (-1 :: GLfloat))
         renderObject Solid (Sphere' 1 15 15)
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-2.5) 2.5 (-2.5 * hf/wf) (2.5 * hf/wf) (-10) 10
      else ortho (-2.5 * wf/hf) (2.5 * wf/hf) (-2.5) 2.5 (-10) 10
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 500
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
