{-
   Robot.hs (adapted from robot.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

  This program shows how to composite modeling transformations to draw
  translated and rotated hierarchical models. Interaction: pressing the s
  and e keys (shoulder and elbow) alters the rotation of the robot arm.
-}

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { shoulder, elbow :: IORef GLint }

makeState :: IO State
makeState = do
   s <- newIORef 0
   e <- newIORef 0
   return $ State { shoulder = s, elbow = e }

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
       scalef = scale :: GLfloat -> GLfloat -> GLfloat -> IO ()
   preservingMatrix $ do
      translatef (Vector3 (-1) 0 0)
      s <- get (shoulder state)
      rotate (fromIntegral s :: GLfloat) (Vector3 0 0 1)
      translatef (Vector3 1 0 0)
      preservingMatrix $ do
         scalef 2 0.4 1
         renderObject Wireframe (Cube 1)
      translatef (Vector3 1 0 0)
      e <- get (elbow state)
      rotate (fromIntegral e :: GLfloat) (Vector3 0 0 1)
      translatef (Vector3 1 0 0)
      preservingMatrix $ do
         scalef 2 0.4 1
         renderObject Wireframe (Cube 1)
   swapBuffers

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 65 (fromIntegral w / fromIntegral h) 1 20
   matrixMode $= Modelview 0
   loadIdentity
   -- resolve overloading, not needed in "real" programs
   let translatef = translate :: Vector3 GLfloat -> IO ()
   translatef (Vector3 0 0 (-5))

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case c of
   's'   -> update shoulder   5
   'S'   -> update shoulder (-5)
   'e'   -> update elbow      5
   'E'   -> update elbow    (-5)
   '\27' -> exitWith ExitSuccess
   _     -> return ()
   where update joint inc = do
            joint state $~ ((`mod` 360) . (+ inc))
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
