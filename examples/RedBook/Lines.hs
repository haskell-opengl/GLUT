{-
   Lines.hs (adapted from lines.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates geometric primitives and their attributes.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

drawOneLine :: Vertex2 GLfloat -> Vertex2 GLfloat -> IO ()
drawOneLine p1 p2 = renderPrimitive Lines $ do vertex p1; vertex p2

myInit :: IO ()
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat

display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   -- select white for all lines
   color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)

   -- in 1st row, 3 lines, each with a different stipple
   lineStipple $= Just (1, 0x0101)  --  dotted
   drawOneLine (Vertex2  50 125) (Vertex2 150 125)
   lineStipple $= Just (1, 0x00FF)  --  dashed
   drawOneLine (Vertex2 150 125) (Vertex2  50 125)
   lineStipple $= Just (1, 0x1C47)  --  dash/dot/dash
   drawOneLine (Vertex2 250 125) (Vertex2 350 125)

   -- in 2nd row, 3 wide lines, each with different stipple
   lineWidth $= 5.0
   lineStipple $= Just (1, 0x0101)  --  dotted
   drawOneLine (Vertex2  50 100) (Vertex2 150 100)
   lineStipple $= Just (1, 0x00FF)  --  dashed
   drawOneLine (Vertex2 150 100) (Vertex2 250 100)
   lineStipple $= Just (1, 0x1C47)  --  dash/dot/dash
   drawOneLine (Vertex2 250 100) (Vertex2 350 100)
   lineWidth $= 1.0

   -- in 3rd row, 6 lines, with dash/dot/dash stipple
   -- as part of a single connected line strip
   lineStipple $= Just (1, 0x1C47)  --  dash/dot/dash
   renderPrimitive LineStrip $ mapM_ vertex [ Vertex2 (50+(i*50)) (75 :: GLint) | i <- [0..6] ]

   -- in 4th row, 6 independent lines with same stipple
   sequence_ [ drawOneLine (Vertex2 (50+( i   *50)) 50)
                           (Vertex2 (50+((i+1)*50)) 50) | i <- [0..5] ]

   -- in 5th row, 1 line, with dash/dot/dash stipple
   -- and a stipple repeat factor of 5
   lineStipple $= Just (5, 0x1C47)  --  dash/dot/dash
   drawOneLine (Vertex2 50 25) (Vertex2 350 25)

   lineStipple $= Nothing
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
   -- the following line is not in the original example, but it's good style...
   matrixMode $= Modelview 0

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _ _ _ _ = return ()

--  Request double buffer display mode.
--  Register mouse input callback functions
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 400 150
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop
