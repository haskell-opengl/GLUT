{-
   BlendEqn.hs (adapted from blendeqn.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   Demonstrate the different blending functions available with the OpenGL
   imaging subset. This program demonstrates use of blendEquation.

   The following keys change the selected blend equation function:
 
       'a'  ->  FuncAdd
       's'  ->  FuncSubtract
       'r'  ->  FuncReverseSubtract
       'm'  ->  Min
       'x'  ->  Max
-}

import Data.Char ( toLower )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

myInit :: IO ()
myInit = do
   clearColor $= Color4 1 1 0 0
   blendFunc $= (One, One)
   blend $= Enabled

display :: DisplayCallback
display = do
   clear [ ColorBuffer ]

   color (Color3 0 0 (1 :: GLfloat))
   rect (Vertex2 (-0.5) (-0.5)) (Vertex2 0.5 (0.5 :: GLfloat))

   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   let aspect = fromIntegral w / fromIntegral h

   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   if aspect < 1
      then let aspect' = recip aspect
           in ortho (-aspect') aspect' (-1) 1 (-1) 1
      else ortho (-1) 1 (-aspect) aspect (-1) 1
   matrixMode $= Modelview 0

keyboard :: KeyboardMouseCallback
keyboard (Char c) Down _ _ = case toLower c of
   -- Colors are added as: (0, 0, 1) + (1, 1, 0) = (1, 1, 1)
   -- which will produce a white square on a yellow background.
   'a'   -> setBlendEquation FuncAdd

   -- Colors are subtracted as: (0, 0, 1) - (1, 1, 0) = (-1, -1, 1)
   -- which is clamped to (0, 0, 1), producing a blue square on a
   -- yellow background
   's'   -> setBlendEquation FuncSubtract

   -- Colors are subtracted as: (1, 1, 0) - (0, 0, 1) = (1, 1, -1)
   -- which is clamed to (1, 1, 0). This produces yellow for both
   -- the square and the background.
   'r'   -> setBlendEquation FuncReverseSubtract

   -- The minimum of each component is computed, as
   -- [min(0, 1), min(0, 1), min(1, 0)] which equates to (0, 0, 0).
   -- This will produce a black square on the yellow background.
   'm'   -> setBlendEquation Min

   -- The minimum of each component is computed, as
   -- [max(0, 1), max(0, 1), max(1, 0)] which equates to (1, 1, 1)
   -- This will produce a white square on the yellow background.
   'x'   -> setBlendEquation Max

   '\27' -> exitWith ExitSuccess
   _     -> return ()
   where setBlendEquation e = do
            blendEquation $= e
            postRedisplay Nothing
keyboard _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 512 512
   initialWindowPosition $= Position 100 100
   createWindow progName
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   displayCallback $= display
   mainLoop
