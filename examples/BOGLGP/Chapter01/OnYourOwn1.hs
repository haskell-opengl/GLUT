{-
   OnYourOwn1.hs (adapted from Simple.cpp which is (c) 2004 Astle/Hawkins)
   Copyright (c) Sven Panne 2004-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

import Control.Monad ( unless )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT hiding ( initialize )

--------------------------------------------------------------------------------
-- Setup GLUT and OpenGL, drop into the event loop.
--------------------------------------------------------------------------------
main :: IO ()
main = do
   -- Setup the basic GLUT stuff
   getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]

   -- Create the window
   initialWindowSize $= Size 1024 768
   initialWindowPosition $= Position 100 150
   createWindow "BOGLGP - Chapter 1 - On Your Own 1"

   initialize

   -- Register the event callback functions
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboardMouseHandler
   -- No need for an idle callback here, this would just hog the CPU
   -- without any visible effect

   -- At this point, control is relinquished to the GLUT event handler.
   -- Control is returned as events occur, via the callback functions.
   mainLoop

--------------------------------------------------------------------------------
-- One time setup, including creating menus, creating a light, setting the
-- shading mode and clear color, and loading textures.
--------------------------------------------------------------------------------
initialize :: IO ()
initialize = do
   -- set up the only meny
   attachMenu RightButton (Menu [MenuEntry "Exit" (exitWith ExitSuccess)])

   depthFunc $= Just Less

--------------------------------------------------------------------------------
-- Handle mouse and keyboard events. For this simple demo, just exit on a
-- left click or when q is pressed.
--------------------------------------------------------------------------------
keyboardMouseHandler :: KeyboardMouseCallback
keyboardMouseHandler (MouseButton LeftButton)_ _ _ = exitWith ExitSuccess
keyboardMouseHandler (Char 'q')              _ _ _ = exitWith ExitSuccess
keyboardMouseHandler _                       _ _ _ = postRedisplay Nothing

--------------------------------------------------------------------------------
-- Reset the viewport for window changes.
--------------------------------------------------------------------------------
reshape :: ReshapeCallback
reshape size@(Size width height) =
   unless (height == 0) $ do
      viewport $= (Position 0 0, size)
      matrixMode $= Projection
      loadIdentity
      perspective 90 (fromIntegral width / fromIntegral height) 1 100

      matrixMode $= Modelview 0

--------------------------------------------------------------------------------
-- Clear and redraw the scene.
--------------------------------------------------------------------------------
display :: DisplayCallback
display = do
   -- set up the camera
   loadIdentity
   lookAt (Vertex3 0 1 6) (Vertex3 0 0 0) (Vector3 0 1 0)

   -- clear the screen
   clear [ ColorBuffer, DepthBuffer ]

   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()

   -- draw a triangle
   renderPrimitive Triangles $ do
      color3f (Color3 1 0 0)
      vertex3f (Vertex3 2 2.5 (-1))
      color3f (Color3 1 0 0)
      vertex3f (Vertex3 (-3.5) (-2.5) (-1))
      color3f (Color3 1 0 0)
      vertex3f (Vertex3 2 (-4) 0)

   -- draw a polygon
   renderPrimitive Polygon $ do
      color3f (Color3 0 0 1)
      vertex3f (Vertex3 (-1) 2 0)
      color3f (Color3 0 0 1)
      vertex3f (Vertex3 (-3) (-0.5) 0)
      color3f (Color3 0 0 1)
      vertex3f (Vertex3 (-1.5) (-3) 0)
      color3f (Color3 0 0 1)
      vertex3f (Vertex3 1 (-2) 0)
      color3f (Color3 0 0 1)
      vertex3f (Vertex3 1 1 0)

   -- draw everything and swap the display buffer
   swapBuffers
