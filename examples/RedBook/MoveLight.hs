{-
   MoveLight.hs (adapted from movelight.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates when to issue lighting and transformation
   commands to render a model with a light which is moved by a
   modeling transformation (rotate or translate). The light position
   is reset after the modeling transformation is called. The eye
   position does not change.

   A sphere is drawn using a grey material characteristic. A single
   light source illuminates the object.

   Interaction: pressing the left mouse button alters the modeling
   transformation (x rotation) by 30 degrees. The scene is then
   redrawn with the light in a new position.
-}

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { spin :: IORef Int }

makeState :: IO State
makeState = do
   s <- newIORef 0
   return $ State { spin = s }

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Smooth
   lighting $= Enabled
   light (Light 0) $= Enabled
   depthFunc $= Just Less

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer, DepthBuffer ]
   preservingMatrix $ do
      lookAt (Vertex3 0 0 5) (Vertex3 0 0 0) (Vector3 0 1 0)
      preservingMatrix $ do
         s <- get (spin state)
         rotate (fromIntegral s :: GLdouble) (Vector3 1 0 0)
         position (Light 0) $= Vertex4 0 0 1.5 1
         translate (Vector3 0 0 1.5 :: Vector3 GLdouble)
         lighting $= Disabled
         color (Color3 0 1 1 :: Color3 GLfloat)
         renderObject Wireframe (Cube 0.1)
         lighting $= Enabled
      renderObject Solid (Torus 0.275 0.85 8 15)
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 40 (fromIntegral w / fromIntegral h) 1 20
   matrixMode $= Modelview 0
   loadIdentity

keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state (MouseButton LeftButton) Down _ _ = do
   spin state $~ ((`mod` 360) . (+ 30))
   postRedisplay Nothing
keyboardMouse _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardMouse _ _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   state <- makeState
   myInit
   displayCallback $= display state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboardMouse state)
   mainLoop
