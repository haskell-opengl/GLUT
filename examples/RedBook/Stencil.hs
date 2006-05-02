{-
   Stencil.hs (adapted from stencil.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2006 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates use of the stencil buffer for masking
   nonrectangular regions. Whenever the window is redrawn, a value of 1 is drawn
   into a diamond-shaped region in the stencil buffer. Elsewhere in the stencil
   buffer, the value is 0. Then a blue sphere is drawn where the stencil value
   is 1, and yellow torii are drawn where the stencil value is not 1.
-}

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data DisplayLists = DisplayLists { yellowMat, blueMat :: DisplayList }

myInit :: IO DisplayLists
myInit = do
   y <- defineNewList Compile $ do
      materialDiffuse Front $= Color4 0.7 0.7 0 1
      materialSpecular Front $= Color4 1 1 1 1
      materialShininess Front $= 64

   b <- defineNewList Compile $ do
      materialDiffuse Front $= Color4 0.1 0.1 0.7 1
      materialSpecular Front $= Color4  0.1 1 1 1
      materialShininess Front $= 45

   position (Light 0) $= Vertex4 1 1 1 0

   light (Light 0) $= Enabled
   lighting $= Enabled
   depthFunc $= Just Less

   clearStencil $= 0
   stencilTest $= Enabled

   return $ DisplayLists { yellowMat = y, blueMat = b }

-- Draw a sphere in a diamond-shaped section in the middle of a window with 2
-- torii.
display :: DisplayLists -> DisplayCallback
display displayLists = do
   clear [ ColorBuffer, DepthBuffer ]

   -- draw blue sphere where the stencil is 1
   stencilFunc $= (Equal, 1, 1)
   stencilOp $= (OpKeep, OpKeep, OpKeep)
   callList (blueMat displayLists)
   renderObject Solid (Sphere' 0.5 15 15)

   -- resolve overloading, not needed in "real" programs
   let rotatef = rotate :: GLfloat -> Vector3 GLfloat -> IO ()

   -- draw the tori where the stencil is not 1
   stencilFunc $= (Notequal, 1, 1)
   preservingMatrix $ do
      rotatef 45 (Vector3 0 0 1)
      rotatef 45 (Vector3 0 1 0)
      callList (yellowMat displayLists)
      renderObject Solid (Torus 0.275 0.85 15 15)
      preservingMatrix $ do
         rotatef 90 (Vector3 1 0 0)
         renderObject Solid (Torus 0.275 0.85 15 15)

   flush

-- Whenever the window is reshaped, redefine the coordinate system and redraw
-- the stencil area.
reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)

   -- create a diamond shaped stencil area
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho2D (-3) 3 (-3*hf/wf) (3*hf/wf)
      else ortho2D (-3*wf/hf) (3*wf/hf) (-3) 3
   matrixMode $= Modelview 0
   loadIdentity

   -- resolve overloading, not needed in "real" programs
   let vertex2f = vertex :: Vertex2 GLfloat -> IO ()
       translatef = translate :: Vector3 GLfloat -> IO ()

   clear [ StencilBuffer ]
   stencilFunc $= (Always, 1, 1)
   stencilOp $= (OpReplace, OpReplace, OpReplace)
   renderPrimitive Quads $ do
      vertex2f (Vertex2 (-1) 0)
      vertex2f (Vertex2 0 1)
      vertex2f (Vertex2 1 0)
      vertex2f (Vertex2 0 (-1))

   matrixMode $= Projection
   loadIdentity
   perspective 45 (wf/hf) 3 7
   matrixMode $= Modelview 0
   loadIdentity
   translatef (Vector3 0 0 (-5))

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

-- Main Loop: Be certain to request stencil bits.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer, WithStencilBuffer ]
   initialWindowSize $= Size 400 400
   initialWindowPosition $= Position 100 100
   createWindow progName
   displayLists <- myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= display displayLists
   mainLoop
