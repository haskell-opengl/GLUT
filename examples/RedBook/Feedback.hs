{-
   Feedback.hs (adapted from feedback.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2003 <sven_panne@yahoo.com>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates use of OpenGL feedback. First, a lighting
   environment is set up and a few lines are drawn. Then feedback mode is
   entered, and the same lines are drawn. The results in the feedback buffer are
   printed.
-}

import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

-- Initialize lighting.
myInit :: IO ()
myInit = do
   lighting $= Enabled
   light (Light 0) $= Enabled

-- Draw a few lines and two points, one of which will be clipped. If in feedback
-- mode, a passthrough token is issued between each primitive
drawGeometry :: IO ()
drawGeometry = do
   mode <- get renderMode
   -- resolve overloading, not needed in "real" programs
   let normal3f = normal :: Normal3 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()
   renderPrimitive LineStrip $ do
      normal3f (Normal3 0 0 1)
      vertex3f (Vertex3 30 30 0)
      vertex3f (Vertex3 50 60 0)
      vertex3f (Vertex3 70 40 0)
   when (mode == Feedback) $
      passThrough (PassThroughValue 1)
   renderPrimitive Points $
      vertex3f (Vertex3 (-100) (-100) (-100))   -- will be clipped
   when (mode == Feedback) $
      passThrough (PassThroughValue 2)
   renderPrimitive Points $ do
      normal3f (Normal3 0 0 1)
      vertex3f (Vertex3 50 50 0)
   flush   -- not in original example

printBuffer :: Maybe [FeedbackToken] -> IO ()
printBuffer = maybe (putStrLn "feedback buffer overflow") (mapM_ print)

display :: DisplayCallback
display = do
   matrixMode $= Projection
   loadIdentity
   ortho 0 100 0 100 0 1

   clearColor $= Color4 0 0 0 0
   clear [ ColorBuffer ]
   drawGeometry

   (_, tokens) <- getFeedbackTokens 1024 ThreeDColor drawGeometry
   printBuffer tokens

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 100 100
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
