{-
   UnProject.hs (adapted from unproject.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   When the left mouse button is pressed, this program reads the mouse
   position and determines two 3D points from which it was transformed.
   Very little is displayed.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 45 (fromIntegral w / fromIntegral h) 1 100
   matrixMode $= Modelview 0
   loadIdentity

keyboardMouse :: KeyboardMouseCallback
keyboardMouse (MouseButton LeftButton)  Down _ (Position x y) = do
   v@(_, Size _ h) <- get viewport
   mvMatrix <- get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLdouble)
   projMatrix <- get (matrix (Just Projection)) :: IO (GLmatrix GLdouble)
   let realY = h - y -1
   putStrLn ("Coordinates at cursor are (" ++ show x ++", " ++ show realY ++ ")")
   w0 <- unProject (Vertex3 (fromIntegral x) (fromIntegral realY) 0) mvMatrix projMatrix v
   putStrLn ("World coords at z=0.0 are " ++ show w0)
   w1 <- unProject (Vertex3 (fromIntegral x) (fromIntegral realY) 1) mvMatrix projMatrix v
   putStrLn ("World coords at z=1.0 are " ++ show w1)
keyboardMouse (MouseButton RightButton) Down _ _ = exitWith ExitSuccess
keyboardMouse (Char '\27')              Down _ _ = exitWith ExitSuccess
keyboardMouse _                         _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboardMouse
   mainLoop
