{-
   BezCurve.hs (adapted from fog.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2003 <sven_panne@yahoo.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program uses evaluators to draw a Bezier curve.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

ctrlPoints :: [Vertex3 GLfloat]
ctrlPoints = [ Vertex3 (-4)(-4) 0, Vertex3 (-2) 4 0,
               Vertex3   2 (-4) 0, Vertex3   4  4 0 ]

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   m <- newMap1 (0, 1) ctrlPoints
   map1 $= Just (m :: GLmap1 Vertex3 GLfloat)

display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
   color3f (Color3 1 1 1)
   renderPrimitive LineStrip $
      mapM_ evalCoord1 [ i/30.0 :: GLfloat | i <- [0..30] ]
   -- The following code displays the control points as dots.
   pointSize $= 5
   color3f (Color3 1 1 0)
   renderPrimitive Points $
      mapM_ vertex ctrlPoints
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-5.0) 5.0 (-5.0*hf/wf) (5.0*hf/wf) (-5.0) 5.0
      else ortho (-5.0*wf/hf) (5.0*wf/hf) (-5.0) 5.0 (-5.0) 5.0
   matrixMode $= Modelview 0
   loadIdentity

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
