{-
   BezCurve.hs (adapted from fog.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2003 <sven_panne@yahoo.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program renders a wireframe Bezier surface, using two-dimensional
   evaluators.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
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

-- Hey mom, look, it's C!  ;-)
for :: GLfloat -> GLfloat -> (GLfloat -> IO ()) -> IO ()
for s e f = mapM_ f [ i | i <- [ s, if s <= e then s + 1 else s - 1 .. e ] ]

display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
   color (Color3 1 1 1 :: Color3 GLfloat)
   preservingMatrix $ do
      rotate (85 :: GLfloat) (Vector3 1 1 1)
      for 0 8 $ \j -> do
         renderPrimitive LineStrip $ do
            for 0 30 $ \i -> evalCoord2 (i/30, j/ 8)
         renderPrimitive LineStrip $ do
            for 0 30 $ \i -> evalCoord2 (j/ 8, i/30)
   flush

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   m <- newMap2 (0, 1) (0, 1) ctrlPoints
   map2 $= Just (m :: GLmap2 Vertex3 GLfloat)
   mapGrid2 $= ((20, (0, 1)), (20, (0, 1 :: GLfloat)))
   depthFunc $= Just Less
   shadeModel $= Flat

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
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
