{-
   OpenGLApplication.hs (adapted from OpenGLApplication which is (c) 2004 Astle/Hawkins)
   Copyright (c) Sven Panne 2004 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE
-}

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT hiding ( initialize )

----------------------------------------------------------------------------
-- Setup GLUT and OpenGL, drop into the event loop.
----------------------------------------------------------------------------
main :: IO ()
main = do
   -- Setup the basic GLUT stuff
   getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]

   -- Create the window
   initialWindowSize $= Size 800 600
   get gameModeInfo >>= print
   gameModeCapabilities $= [ Where' GameModeWidth  IsEqualTo 800,
                             Where' GameModeHeight IsEqualTo 600 ]
   get gameModeInfo >>= print
   createWindow "BOGLGP - Chapter 2 - OpenGL Application"
   fullScreen

   state <- initialize

   -- Register the event callback functions
   displayCallback $= do render state; swapBuffers
   reshapeCallback $= Just setupProjection
   keyboardMouseCallback $= Just keyboardMouseHandler
   idleCallback $= Just (do prepare state; postRedisplay Nothing)

   -- At this point, control is relinquished to the GLUT event handler.
   -- Control is returned as events occur, via the callback functions.
   mainLoop

----------------------------------------------------------------------------
-- Handle mouse and keyboard events. For this simple demo, just exit when
-- ESCAPE is pressed.
----------------------------------------------------------------------------
keyboardMouseHandler :: KeyboardMouseCallback
keyboardMouseHandler (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardMouseHandler _             _   _ _ = return ()

----------------------------------------------------------------------------
-- The globale state, which is only the current angle in this simple demo.
-- We don't need the window dimensions here, they are not used and would
-- be easily available via GLUT anyway.
----------------------------------------------------------------------------
data State = State { angle :: IORef GLfloat }

makeState :: IO State
makeState = do
   a <- newIORef 0
   return $ State { angle = a }

----------------------------------------------------------------------------
-- Do one time setup, i.e. set the clear color and create the global state.
----------------------------------------------------------------------------
initialize :: IO State
initialize = do
   -- clear to black background
   clearColor $= Color4 0 0 0 0

   makeState

----------------------------------------------------------------------------
-- Reset the viewport for window changes.
----------------------------------------------------------------------------
setupProjection :: ReshapeCallback
setupProjection (Size width height) = do
   -- don't want a divide by zero
   let h = max 1 height
   -- reset the viewport to new dimensions
   viewport $= (Position 0 0, Size width h)
   -- set projection matrix as the current matrix
   matrixMode $= Projection
   -- reset projection matrix
   loadIdentity

   -- calculate aspect ratio of window
   perspective 52 (fromIntegral width / fromIntegral h) 1 1000

   -- set modelview matrix
   matrixMode $= Modelview 0
   -- reset modelview matrix
   loadIdentity

----------------------------------------------------------------------------
-- Perform any data-specific updates for a frame. Here we only increment the
-- angle for the rotation of the triangle.
----------------------------------------------------------------------------
prepare :: State -> IdleCallback
prepare state = do
   angle state $~ (+ 0.1)

----------------------------------------------------------------------------
-- Clear and redraw the scene.
----------------------------------------------------------------------------
render :: State -> DisplayCallback
render state = do
   -- clear screen and depth buffer
   clear [ ColorBuffer, DepthBuffer ]
   loadIdentity

   -- resolve overloading, not needed in "real" programs
   let translate3f = translate :: Vector3 GLfloat -> IO ()
       color3f = color :: Color3 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()

   -- move back 5 units and rotate about all 3 axes
   translate3f (Vector3 0 0 (-5))
   a <- get (angle state)
   rotate a (Vector3 1 0 0)
   rotate a (Vector3 0 1 0)
   rotate a (Vector3 0 0 1)

   -- lime greenish color
   color3f (Color3 0.7 1 0.3)

   -- draw the triangle such that the rotation point is in the center
   renderPrimitive Triangles $ do
      vertex3f (Vertex3   1  (-1) 0)
      vertex3f (Vertex3 (-1) (-1) 0)
      vertex3f (Vertex3   0    1  0)
