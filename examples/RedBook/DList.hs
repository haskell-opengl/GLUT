{-
   DList.hs (adapted from list.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates how to make and execute a 
   display list.  Note that attributes, such as current 
   color and matrix, are changed.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO DisplayList
myInit = do
   [listName] <- genObjectNames 1
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
       vertex2f = vertex :: Vertex2 GLfloat -> IO ()
       translatef = translate :: Vector3 GLfloat -> IO ()
   defineList listName Compile $ do
      color3f (Color3 1 0 0) -- current color red
      renderPrimitive Triangles $ do
         vertex2f (Vertex2 0 0)
         vertex2f (Vertex2 1 0)
         vertex2f (Vertex2 0 1)
      translatef (Vector3 1.5 0.0 0.0) -- move position
   shadeModel $= Flat
   return listName

drawLine :: IO ()
drawLine = do
   -- resolve overloading, not needed in "real" programs
   let vertex2f = vertex :: Vertex2 GLfloat -> IO ()
   renderPrimitive Lines $ do
      vertex2f (Vertex2  0.0 0.5)
      vertex2f (Vertex2 15.0 0.5)

display :: DisplayList -> DisplayCallback
display listName = do
   -- NOTE: The following 'loadIdentity' is missing in the original
   -- example, but without it the translatef calls accumulate and
   -- the graphics wander out of the window after a few redraws...
   loadIdentity

   clear [ ColorBuffer ]
   -- resolve overloading, not needed in "real" programs
   let color3f = color :: Color3 GLfloat -> IO ()
   color3f (Color3 0 1 0) -- current color green
   sequence_ (replicate 10 (callList listName)) -- draw 10 triangles
   drawLine -- is this line green? NO!
            -- where is the line drawn?
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho2D 0.0 2.0 (-0.5*hf/wf) (1.5*hf/wf)
      else ortho2D 0.0 (2.0*wf/hf) (-0.5) 1.5
   matrixMode $= Modelview 0
   loadIdentity

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

-- Open window with initial window size, title bar, 
-- RGBA display mode, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 650 50
   createWindow progName
   listName <- myInit
   reshapeCallback $= Just reshape
   displayCallback $= display listName
   keyboardMouseCallback $= Just keyboard
   mainLoop
