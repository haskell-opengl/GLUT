{-
   BezCurve.hs (adapted from fog.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2003 <sven_panne@yahoo.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program renders a lighted, filled Bezier surface, using two-dimensional
   evaluators.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.List ( transpose )
import Graphics.UI.GLUT

ctrlPoints :: [[Vertex3 GLfloat]]
ctrlPoints = [
   [ Vertex3 (-1.5) (-1.5)   4.0,  Vertex3 (-0.5) (-1.5)   2.0,
     Vertex3   0.5  (-1.5) (-1.0), Vertex3   1.5  (-1.5)   2.0 ],
   [ Vertex3 (-1.5) (-0.5)   1.0,  Vertex3 (-0.5) (-0.5)   3.0,
     Vertex3   0.5  (-0.5)   0.0,  Vertex3   1.5  (-0.5) (-1.0) ],
   [ Vertex3 (-1.5)   0.5    4.0,  Vertex3 (-0.5)   0.5    0.0,
     Vertex3   0.5    0.5    3.0,  Vertex3   1.5    0.5    4.0 ],
   [ Vertex3 (-1.5)   1.5  (-2.0), Vertex3 (-0.5)   1.5  (-2.0),
     Vertex3   0.5    1.5    0.0,  Vertex3   1.5    1.5  (-1.0) ]]

initlights :: IO ()
initlights = do
   lighting $= Enabled
   light (Light 0) $= Enabled

   ambient  (Light 0) $= Color4 0.2 0.2 0.2 1.0
   position (Light 0) $= Vertex4 0 0 2 1

   materialDiffuse   Front $= Color4 0.6 0.6 0.6 1.0
   materialSpecular  Front $= Color4 1.0 1.0 1.0 1.0
   materialShininess Front $= 50

display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      rotate (85 :: GLfloat) (Vector3 1 1 1)
      evalMesh2 Fill (0, 20) (0, 20)
   flush

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   depthFunc $= Just Less
   m <- newMap2 (0, 1) (0, 1) (transpose ctrlPoints)
   map2 $= Just (m :: GLmap2 Vertex3 GLfloat)
   autoNormal $= Enabled
   mapGrid2 $= ((20, (0, 1)), (20, (0, 1 :: GLfloat)))
   initlights  -- for lighted version only

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-4.0) 4.0 (-4.0*hf/wf) (4.0*hf/wf) (-4.0) 4.0
      else ortho (-4.0*wf/hf) (4.0*wf/hf) (-4.0) 4.0 (-4.0) 4.0
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
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
