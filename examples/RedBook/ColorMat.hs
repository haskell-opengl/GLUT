{-
   Light.hs (adapted from light.c which is (c) Silicon Graphics, Inc.)
   Copyright (c) Sven Panne 2002-2005 <sven.panne@aedion.de>
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE

   After initialization, the program will be in ColorMaterial mode.
   Interaction: pressing the mouse buttons will change the diffuse
   reflection values.
-}

import Data.IORef ( IORef, newIORef )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT

data State = State { r, g, b :: IORef GLfloat }

diffuseMaterial :: State -> IO (Color4 GLfloat)
diffuseMaterial state = do
   r' <- get (r state)
   g' <- get (g state)
   b' <- get (b state)
   return $ Color4 r' g' b' 1

makeState :: IO State
makeState = do
   r' <- newIORef 0.5
   g' <- newIORef 0.5
   b' <- newIORef 0.5
   return $ State { r = r', g = g', b = b' }

-- Initialize material property, light source, lighting model,
-- and depth buffer.
myInit :: State -> IO ()
myInit state = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Smooth
   depthFunc $= Just Less
   dm <- diffuseMaterial state
   materialDiffuse Front $= dm
   materialSpecular Front $= Color4 1 1 1 1
   materialShininess Front $= 25
   position (Light 0) $= Vertex4 1 1 1 0
   lighting $= Enabled
   light (Light 0) $= Enabled
   colorMaterial $= Just (Front, Diffuse)

display :: DisplayCallback
display = do
   clear [ ColorBuffer, DepthBuffer ]
   renderObject Solid (Sphere' 1 20 16)
   flush

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then ortho (-1.5) 1.5 (-1.5 * hf/wf) (1.5 * hf/wf) (-10) 10
      else ortho (-1.5 * wf/hf) (1.5 * wf/hf) (-1.5) 1.5 (-10) 10
   matrixMode $= Modelview 0
   loadIdentity

keyboardMouse :: State -> KeyboardMouseCallback
keyboardMouse state (MouseButton button) Down _ _ = case button of
   LeftButton   -> update r
   MiddleButton -> update g
   RightButton  -> update b
   _ -> return ()
   where update component = do
            component state $~ inc
            dm <- diffuseMaterial state
            color dm
            postRedisplay Nothing
         inc x = let s = x + 0.1 in if s > 1 then 0 else s
keyboardMouse _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyboardMouse _ _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 500 500
   initialWindowPosition $= Position 100 100
   createWindow progName
   state <- makeState
   myInit state
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just (keyboardMouse state)
   mainLoop
