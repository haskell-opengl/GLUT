{-
   Clip.hs (adapted from clip.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates arbitrary clipping planes.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       translatef = translate :: Vector3 GLfloat -> IO ()
       rotatef = rotate :: GLfloat -> Vector3 GLfloat -> IO ()
   color3f (Color3 1 1 1)

   preservingMatrix $ do
      translatef (Vector3 0 0 (-5))

      -- clip lower half -- y < 0
      clipPlane (ClipPlaneName 0) $= Just (Plane 0 1 0 0)
      -- clip left half -- x < 0
      clipPlane (ClipPlaneName 1) $= Just (Plane 1 0 0 0)

      rotatef 90 (Vector3 1 0 0)
      renderObject Wireframe (Sphere' 1 20 16)

   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 60 (fromIntegral w / fromIntegral h) 1 20
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
