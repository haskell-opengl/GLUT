{-
   Double.hs (adapted from double.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This is a simple double buffered program.
   Pressing the left mouse button rotates the rectangle.
   Pressing the middle mouse button stops the rotation.
-}

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { spin :: IORef GLfloat }

makeState :: IO State
makeState = do
   s <- newIORef 0
   return $ State { spin = s }

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer ]
   preservingMatrix $ do
      s <- get (spin state)
      rotate s (Vector3 0 0 1)
      color (Color3 1 1 1 :: Color3 GLfloat)
      rect (Vertex2 (-25) (-25)) (Vertex2 25 25 :: Vertex2 GLfloat)
   swapBuffers

spinDisplay :: State -> IdleCallback
spinDisplay state = do
   let wrap n s = if s > n then s - n else s
   spin state $~ (wrap 360 . (+ 2))
   postRedisplay Nothing

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho (-50) 50 (-50) 50 (-1) 1
   matrixMode $= Modelview 0
   loadIdentity

keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state (MouseButton b) Down _ _ =
   idleCallback $= case b of
      LeftButton -> Just (spinDisplay state)
      _ -> Nothing
-- ESC not handled in the original example, but useful nevertheless
keyboardMouse _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardMouse _ _ _ _ _ = return ()

--  Request double buffer display mode.
--  Register mouse input callback functions
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   createWindow progName
   state <- makeState
   myInit
   displayCallback $= display state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboardMouse state)
   mainLoop
