{-
   FogCoord.hs (adapted from fogcoord.c which is (c) Silicon Graphics, Inc)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   This program demonstrates the use of explicit fog coordinates. You can press
   the keyboard and change the fog coordinate value at any vertex. You can also
   switch between using explicit fog coordinates and the default fog generation
   mode.

   Pressing the 'f' and 'b' keys move the viewer forward and backwards. Pressing
   'c' initiates the default fog generation.  Pressing capital 'C' restores
   explicit fog coordinates.  Pressing '1', '2', '3', '8', '9', and '0' add or
   subtract from the fog coordinate values at one of the three vertices of the
   triangle.
-}

import Control.Monad ( when )
import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { f1, f2, f3 :: IORef (FogCoord1 GLfloat)   }

makeState :: IO State
makeState = do
   f1' <- newIORef (FogCoord1  1)
   f2' <- newIORef (FogCoord1  5)
   f3' <- newIORef (FogCoord1 10)
   return $ State { f1 = f1', f2 = f2', f3 = f3' }

-- Initialize fog
myInit :: IO ()
myInit = do
   let theFogColor = Color4 0 0.25 0.25 1
   fog $= Enabled
   fogMode $= Exp 0.25
   fogColor $= theFogColor
   hint Fog $= DontCare
   fogCoordSrc $= FogCoord
   clearColor $= theFogColor

drawTriangle :: State -> (State -> IORef (FogCoord1 GLfloat)) -> Vertex3 GLfloat -> IO ()
drawTriangle state f v = do
   fc <- get (f state)
   fogCoord fc
   vertex v

-- display draws a triangle at an angle.
display :: State -> DisplayCallback
display state = do
   clear [ ColorBuffer ]

   color (Color3 1 0.75 (0 :: GLfloat))
   renderPrimitive Triangles $ do
      drawTriangle state f1 (Vertex3   2  (-2)   0 )
      drawTriangle state f2 (Vertex3 (-2)   0  (-5))
      drawTriangle state f3 (Vertex3   0    2 (-10))

   swapBuffers

reshape :: ReshapeCallback
reshape size = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 45 1 0.25 25
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))

keyboard :: State -> KeyboardMouseCallback
keyboard state (Char c) Down _ _ = case c of
   'c'   -> setSrc FragmentDepth
   'C'   -> setSrc FogCoord
   '1'   -> inc f1   0.25
   '2'   -> inc f2   0.25
   '3'   -> inc f3   0.25
   '8'   -> inc f1 (-0.25)
   '9'   -> inc f2 (-0.25)
   '0'   -> inc f3 (-0.25)
   'b'   -> trans  (-0.25)
   'f'   -> trans    0.25
   '\27' -> exitWith ExitSuccess
   _     -> return ()
   where setSrc :: FogCoordSrc -> IO ()
         setSrc s = do
            fogCoordSrc $= s
            postRedisplay Nothing

         inc :: (State -> IORef (FogCoord1 GLfloat)) -> GLfloat -> IO ()
         inc f x = do
            FogCoord1 oldValue <- get (f state)
            let newValue = oldValue + x
            when (newValue > 0) $ do
               f state $= FogCoord1 newValue
               postRedisplay Nothing

         trans :: GLfloat -> IO ()
         trans x = do
            matrixMode $= Modelview 0
            translate (Vector3 0 0 x)
            postRedisplay Nothing
keyboard _ _ _ _ _ = return ()

-- Main Loop: Open window with initial window size, title bar, RGBA display
-- mode, and handle input events.
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode ]
   initialWindowSize $= Size 500 500
   createWindow progName
   state <- makeState
   myInit
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboard state)
   displayCallback $= display state
   mainLoop


