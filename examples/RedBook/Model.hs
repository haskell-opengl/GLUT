{-
   Model.hs (adapted from model.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates modeling transformations.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

drawTriangle :: IO ()
drawTriangle = do
   -- resolve overloading, not needed in "real" programs
   let vertex2f = vertex :: Vertex2 GLfloat -> IO ()
   renderPrimitive LineLoop $ do
      vertex2f (Vertex2    0    25 )
      vertex2f (Vertex2   25  (-25))
      vertex2f (Vertex2 (-25) (-25))

display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       translatef = translate :: Vector3 GLfloat -> IO ()
       scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
       rotatef = rotate :: GLfloat -> Vector3 GLfloat -> IO ()
   color3f (Color3 1 1 1)

   loadIdentity
   color3f (Color3 1 1 1)
   drawTriangle

   lineStipple $= Just (1, 0xF0F0)
   loadIdentity
   translatef (Vector3 (-20) 0 0)
   drawTriangle

   lineStipple $= Just (1, 0xF00F)
   loadIdentity
   scalef 1.5 0.5 1.0
   drawTriangle

   lineStipple $= Just (1, 0x8888)
   loadIdentity
   rotatef 90 (Vector3 0 0 1)
   drawTriangle
   lineStipple $= Nothing

   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-50) 50 (-50 * hf/wf) (50 * hf/wf) (-1) 1
      else ortho (-50 * wf/hf) (50 * wf/hf) (-50) 50 (-1) 1
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
