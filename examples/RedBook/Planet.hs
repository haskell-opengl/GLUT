{-
   Planet.hs (adapted from planet.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program shows how to composite modeling transformations to draw
   translated and rotated models. Interaction: pressing the d and y keys
   (day and year) alters the rotation of the planet around the sun.
-}

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { year, day :: IORef GLint }

makeState :: IO State
makeState = do
   y <- newIORef 0
   d <- newIORef 0
   return $ State { year = y, day = d }

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       translatef = translate :: Vector3 GLfloat -> IO ()
   color3f (Color3 1 1 1)

   preservingMatrix $ do
      renderObject Wireframe (Sphere' 1 20 16)   -- draw sun
      y <- get (year state)
      rotate (fromIntegral y :: GLfloat) (Vector3 0 1 0)
      translatef (Vector3 2 0 0)
      d <- get (day state)
      rotate (fromIntegral d :: GLfloat) (Vector3 0 1 0)
      renderObject Wireframe (Sphere' 0.2 10 8)  -- draw smaller planet

   swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 60 (fromIntegral w / fromIntegral h) 1 20
   matrixMode $= Modelview 0
   loadIdentity
   lookAt (Vertex3 0 0 5) (Vertex3 0 0 0) (Vector3 0 1 0)

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case c of
   'd'   -> update day    10
   'D'   -> update day  (-10)
   'y'   -> update year   10
   'Y'   -> update year (-10)
   '\27' -> exitWith ExitSuccess
   _     -> return ()
   where update angle inc = do
            angle state $~ ((`mod` 360) . (+ inc))
            postRedisplay Nothing
keyboard _ _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   state <- makeState
   myInit
   displayCallback $= display state
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   mainLoop
