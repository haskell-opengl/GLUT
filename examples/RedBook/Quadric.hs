{-
   Quadric.hs (adapted from quadric.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates the use of the renderQuadric routine. Quadric
   objects are created with some quadric properties and errors are reported.
   Note that the cylinder has no top or bottom and the circle has a hole in it.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO (DisplayList, DisplayList, DisplayList, DisplayList)
myInit = do
   clearColor $= Color4 0 0 0 0

   materialAmbient Front $= Color4 0.5 0.5 0.5 1
   materialSpecular Front $= Color4 1 1 1 1
   materialShininess Front $= 50
   position (Light 0) $= Vertex4 1 1 1 0
   lightModelAmbient $= Color4 0.5 0.5 0.5 1

   lighting $= Enabled
   light (Light 0) $= Enabled
   depthFunc $= Just Less

   -- Create 4 display lists, each with a different quadric object.
   -- Different drawing styles and surface normal specifications
   -- are demonstrated.
   -- smooth shaded
   dl1 <- newQuadricDL (Just Smooth) FillStyle (Sphere 0.75 15 10)
   -- flat shaded
   dl2 <- newQuadricDL (Just Flat) FillStyle (Cylinder 0.5 0.3 1 15 5)
   -- all polygons wireframe
   dl3 <- newQuadricDL Nothing LineStyle (Disk 0.25 1 20 4)
   -- boundary only
   dl4 <- newQuadricDL Nothing SilhouetteStyle (PartialDisk 0 1 20 4 0 225)
   return (dl1, dl2, dl3, dl4)

newQuadricDL :: QuadricNormal -> QuadricDrawStyle -> QuadricPrimitive -> IO DisplayList
newQuadricDL n s p =
   defineNewList Compile $ do
      renderQuadric (QuadricStyle n NoTextureCoordinates Outside s) p
      reportErrors

display :: (DisplayList, DisplayList, DisplayList, DisplayList) -> DisplayCallback
display (dl1, dl2, dl3, dl4) = do
   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      -- resolve overloading, not needed in "real" programs
      let translatef = translate :: Vector3 GLfloat -> IO ()
          rotatef = rotate :: GLfloat -> Vector3 GLfloat -> IO ()
          color3f = color :: Color3 GLfloat -> IO ()

      lighting $= Enabled
      shadeModel $= Smooth
      translatef (Vector3 (-1) (-1) 0)
      callList dl1

      shadeModel $= Flat
      translatef (Vector3 0 2 0)
      preservingMatrix $ do
         rotatef 300 (Vector3 1 0 0)
         callList dl2

      lighting $= Disabled
      color3f (Color3 0 1 1)
      translatef (Vector3 2 (-2) 0)
      callList dl3

      color3f (Color3 1 1 0)
      translatef (Vector3 0 2 0)
      callList dl4

   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-2.5) 2.5 (-2.5*hf/wf) (2.5*hf/wf) (-10) 10
      else ortho (-2.5*wf/hf) (2.5*wf/hf) (-2.5) 2.5 (-10) 10
   matrixMode $= Modelview 0

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
   displayLists <- myInit
   displayCallback $= display displayLists
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
